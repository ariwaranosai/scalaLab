package typeprogramming

/**
  * Created by ariwaranosai on 16/3/29.
  *
  */

sealed trait Bool {
    type IF[T <: Up, F <: Up, Up]
}

sealed trait True extends Bool {
    type IF[T <: Up, F <: Up, Up] = T

}

sealed trait False extends Bool {
    type IF[T <: Up, F <: Up, Up] = F
}
