package typeprogramming

/**
  * Created by ariwaranosai on 16/3/24.
  *
  */

object TypeLevel {
    def main(args: Array[String]) {
        import scala.language.higherKinds

        sealed trait Bool {
            type IF[T <: Up, F <: Up, Up]
        }

        sealed trait True extends Bool {
            type IF[T <: Up, F <: Up, Up] = T

        }

        sealed trait False extends Bool {
            type IF[T <: Up, F <: Up, Up] = F
        }



        trait Fold[-Elem, Value] {
            type Apply[E <: Elem, V <: Value] <: Value
        }

        sealed trait Nat {
            type Match[NoZero[N <: Nat] <: Up, IfZero <: Up, Up] <: Up

            type Compare[N <: Nat] <: Comparison

            type FoldR[Init <: Type, Type, F <: Fold[Nat, Type]] <: Type
        }

        sealed trait _0 extends Nat {
            type Match[NoZero[_ <: Nat] <: Up, IfZero <: Up, Up] = IfZero
            type FoldR[Init <: Type, Type, F <: Fold[Nat, Type]] = Init

            type ConstLT[A] = LT
            type Compare[N <: Nat] = N#Match[ConstLT, EQ, Comparison]
        }

        sealed trait Succ[N <: Nat] extends Nat {
            type Match[NoZero[_ <: Nat] <: Up, IfZero <: Up, Up] = NoZero[N]

            type FoldR[Init <: Type, Type, F <: Fold[Nat, Type]] =
            F#Apply[Succ[N], N#FoldR[Init, Type, F]]

            type Compare[O <: Nat] = O#Match[N#Compare, GT, Comparison]
        }

        type _1 = Succ[_0]
        type _2 = Succ[_1]
        type _3 = Succ[_2]
        type _4 = Succ[_3]
        type _5 = Succ[_4]
        type _6 = Succ[_5]

        type ConstFalse[A] = False

        type Is0[A <: Nat] = A#Match[ConstFalse, True, Bool]

        sealed trait Comparison {
            type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] <: Up

            type gt = Match[False, False, True, Bool]
            type ge = Match[False, True, True, Bool]
            type eq = Match[False, True, False, Bool]
            type le = Match[True, True, False, Bool]
            type lt = Match[True, False, False, Bool]
        }

        sealed trait GT extends Comparison {
            type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfGT
        }
        sealed trait LT extends Comparison {
            type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfLT
        }
        sealed trait EQ extends Comparison {
            type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfEQ
        }

        type Add[A <: Nat, B <: Nat] = A#FoldR[B, Nat, Inc]
        type Inc = Fold[Nat, Nat] {
            type Apply[N <: Nat, Acc <: Nat] = Succ[Acc]
        }

        type Mult[A <: Nat, B <: Nat] = A#FoldR[_0, Nat, Sum[B]]

        type Sum[B <: Nat] = Fold[Nat, Nat] {
            type Apply[N <: Nat, Acc <: Nat] = Add[B, Acc]
        }

        type Eq[A <: Nat, B <: Nat] = A#Compare[B]#eq

        class BoolRep[B <: Bool](val value: Boolean)

        implicit val falseRep: BoolRep[False] =
            new BoolRep[False](false)
        implicit val trueRep: BoolRep[True] =
            new BoolRep[True](true)

        type Mod[A <: Nat, B <: Nat] = A#FoldR[_0, Nat, ModFold[B]]
        type ModFold[By <: Nat] = Fold[Nat, Nat] {
            type Apply[N <: Nat, Acc <: Nat] = By#Compare[Acc]#eq#IF[_0, Succ[Acc], Nat]
        }

        type Exp[A <: Nat, B <: Nat] = B#FoldR[_1, Nat, ExpFold[A]]

        type ExpFold[B <: Nat] = Fold[Nat, Nat] {
            type Apply[N <: Nat, Acc <: Nat] = Mult[B, Acc]
        }


        def toBoolean[B <: Bool](implicit b: BoolRep[B]) = b.value

        toBoolean[ Eq[ Mult[_2, _3], _6] ]

    }

}
