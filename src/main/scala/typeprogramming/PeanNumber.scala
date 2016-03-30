package typeprogramming

/**
  * Created by ariwaranosai on 16/3/29.
  *
  */

import typeprogramming.{Bool, False, True}

import scala.language.higherKinds

sealed trait Nat {
    type Match[NoZero[N <: Nat] <: Up, IfZero <: Up, Up] <: Up

    type Compare[N <: Nat] <: Comparison
}

sealed trait _0 extends Nat {
    type Match[NoZero[_ <: Nat] <: Up, IfZero <: Up, Up] = IfZero

    type ConstLT[_] = LT
    type Compare[N <: Nat] = N#Match[ConstLT, EQ, Comparison]
}

sealed trait Succ[N <: Nat] extends Nat {
    type Match[NoZero[_ <: Nat] <: Up, IfZero <: Up, Up] = NoZero[N]

    type Compare[O <: Nat] = O#Match[N#Compare, GT, Comparison]
}

object Nat {
    type _1 = Succ[_0]
    type _2 = Succ[_1]
    type _3 = Succ[_2]
    type _4 = Succ[_3]
    type ConstFalse[A] = False
    type Is0[A <: Nat] = A#Match[ConstFalse, True, Bool]

}



sealed trait Comparison {
    type Match[IfLT <: Up, IfEq <: Up, IfGt <: Up, Up] <: Up

    type gt = Match[False, False, True, Bool]
    type ge = Match[False, True, True, Bool]
    type eq = Match[False, True, False, Bool]
    type le = Match[True, True, False, Bool]
    type lt = Match[True, False, False, Bool]
}

sealed trait GT extends Comparison {
    type Match[IfLt <: Up, IfEq <: Up, IfGt <: Up, Up] = IfGt
}

sealed trait EQ extends Comparison {
    type Match[IfLt <: Up, IfEq <: Up, IfGt <: Up, Up] = IfEq
}

sealed trait LT extends Comparison {
    type Match[IfLt <: Up, IfEq <: Up, IfGt <: Up, Up] = IfLt
}
