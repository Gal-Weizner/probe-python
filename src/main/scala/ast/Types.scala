package ast

object Types extends Enumeration {
  type Types = Value
  val String, Int, Real, Bool, BitVector64 = Value
}
