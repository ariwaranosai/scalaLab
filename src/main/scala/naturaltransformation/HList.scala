package naturaltransformation

/**
  * Created by sai on 2016/7/13.
  */

import HList._

sealed abstract class HList {
  def :+:[A](v: A): HList
  type Foldr[Value, F <: Fold[Any, Value], I <: Value] <: Value
  def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldr[Value, F, I]

  type Foldl[Value, F <: Fold[Any, Value], I <: Value] <: Value
  def foldl[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldl[Value, F, I]
}

final case class HCons[H, T <: HList](head : H, tail : T) extends HList {
  def :+:[A](v: A): HCons[A, HCons[H, T]]  = HCons(v, this)

  type Foldr[Value, F <: Fold[Any, Value], I <: Value] = F#Apply[H, tail.Foldr[Value, F, I]]

  def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldr[Value, F, I] =
    f(head, tail.foldr[Value, F, I](f, i))

  type Foldl[Value, F <: Fold[Any, Value], I <: Value] = tail.Foldl[Value, F, F#Apply[H, I]]

  def foldl[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldl[Value, F, I] =
    tail.foldl[Value, F, F#Apply[H,I]](f, f(head, i))
}

object HNil extends HList {
  def :+:[H](v: H): HCons[H, this.type] = HCons(v, this)

  type Foldl[Value, F <: Fold[Any, Value], I <: Value] = I
  def foldl[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I) = i

  type Foldr[Value, F <: Fold[Any, Value], I <: Value] = I
  def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I) = i
}

trait Fold[-Elem, Value] {
  type Apply[N <: Elem, Acc <: Value] <: Value
  def apply[N <: Elem, Acc <: Value](n: N, acc: Acc): Apply[N, Acc]
}


// aliases for building HList types and for pattern matching
object HList {
  type :+:[H, T <: HList] = HCons[H, T]
  val :+: = HCons

  type :::[A <: HList, B <: HList] = A#Foldr[HList, AppHCons.type, B]

  type Reverse_:::[A <: HList, B <: HList] = A#Foldl[HList, AppHCons.type, B]

  type Reverse[A <: HList] = A#Foldl[HList, AppHCons.type, HNil.type ]

  trait ~>[F[_], G[_]] {
    type Apply[X] = G[X]

    def apply[T](f: F[T]): G[T]
  }

  type Id[T] = T

  trait ~>>[G[_]] extends ~>[Id, G] {
    def apply[T](f: T): G[T]
  }

  type HMap[A <: HList, G[_], M <: ~>>[G]] =
    A#Foldr[HList, MapHCons[G, M] , HNil.type]

  class MapHCons[G[_], M <: ~>>[G]](implicit m: Manifest[M]) extends Fold[Any, HList] {
    type Apply[N <: Any, H <: HList] = M#Apply[N] :+: H

    def apply[A, B <: HList](a: A, b: B) = {
      val t = m.erasure.newInstance.asInstanceOf[M]
      HCons(t(a), b)
    }

  }

  object ToListNT extends ~>>[List] {
    override def apply[T](f: T): List[T] = List(f)
  }

  object ToList extends MapHCons[List, ToListNT.type ]

  object AppHCons extends Fold[Any, HList] {
    type Apply[N <: Any, H <: HList] = N :+: H

    // used later for value-level implementations
    def apply[A,B <: HList](a: A, b: B) = HCons(a, b)
  }

  implicit def hlistOps[B <: HList](b: B): HListOps[B] =
    new HListOps[B] {
      def length =
        b.foldr(Length, 0)

      def reverse =
        b.foldl[HList, AppHCons.type, HNil.type ](AppHCons, HNil)

      def :::[A <: HList](a: A): A#Foldr[HList, AppHCons.type, B] =
        a.foldr[HList, AppHCons.type, B](AppHCons, b)

      def reverse_:::[A <: HList](a: A): A Reverse_::: B =
        a.foldl[HList, AppHCons.type, B](AppHCons, b)

      def map[G[_], M <: ~>>[G], T <: MapHCons[G, M]](m: MapHCons[G, M]): HMap[B, G, M] =
        b.foldr[HList, MapHCons[G, M], HNil.type ](m, HNil)
    }

  object Length extends Fold[Any, Int] {
    type Apply[N <: Any, Acc <: Int] = Int
    def apply[A,B <: Int](a: A, b: B) = b+1
  }
}

sealed trait HListOps[B <: HList] {
  def length: Int
  def :::[A <: HList](a: A): A ::: B
  def reverse: Reverse[B]
  def reverse_:::[A <: HList](a: A): A Reverse_::: B
  def map[G[_], M <: ~>>[G], T <: MapHCons[G, M]](m: MapHCons[G, M]): HMap[B, G, M]
}



object HListTest {

  def main(args: Array[String]): Unit = {
    val t = 1 :+: "hello" :+: 1.01 :+: true :+: HNil

    t match {
      case i :+: s :+: 1.01 :+: false :+: HNil => println(s)
      case i :+: _ => println(i)
      case _ => println("No")
    }

    val a = 3 :+: "ai4" :+: List('r','H') :+: HNil
    val fat = a ::: t;

    val fatr = fat.reverse

    val f = a.map(ToList)

    println(t.tail.head)
    println(fat.tail.tail.tail.tail.head)
    println(fat.length)
    println(f.head)
  }
}