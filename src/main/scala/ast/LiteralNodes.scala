package ast
import sygus.Predicates

abstract class LiteralNode[T](numExamples: Int) extends ASTNode{
  assert(numExamples > 0)
  val height = 0
  val terms = 1
  val value: T

  override def computeOnContext(ctx: Map[String, Any]) = Some(value)
  override val children: Iterable[ASTNode] = Iterable.empty
  def includes(varName: String): Boolean = false
  override lazy val usesVariables: Boolean = false

}
case class StringLiteral(val value: String, numExamples: Int,
                         val predicates: Predicates) extends LiteralNode[String](numExamples) with StringNode{
  override lazy val code: String = '"' + value + '"' //escape?
  override protected val parenless: Boolean = true
}

case class IntLiteral(val value: Int, numExamples: Int,
                      val predicates: Predicates) extends LiteralNode[Int](numExamples) with IntNode{
  override lazy val code: String = value.toString
  override protected val parenless: Boolean = true
}

case class BoolLiteral(val value: Boolean, numExamples: Int,
                       val predicates: Predicates) extends LiteralNode[Boolean](numExamples) with BoolNode {
  override lazy val code: String = value.toString
  override protected val parenless: Boolean = true
}

case class BVLiteral(val value: Long, numExamples: Int,
                     val predicates: Predicates) extends LiteralNode[Long](numExamples) with BVNode {
  override lazy val code: String = f"#x$value%016x"
  override protected val parenless: Boolean = true

}

case class PyStringLiteral(val value: String, numExamples: Int,
                           val predicates: Predicates) extends LiteralNode[String](numExamples) with PyStringNode
{
  override protected val parenless: Boolean = true
  override val code: String = '"' + value.flatMap(c => if (c.toInt >= 32 && c.toInt <= 127 && c != '\\' && c != '"') c.toString
  else c.toInt match {
    case 92 => "\\\\" // \
    case 34 => "\\\"" // "
    case 7 => "\\a" //bell
    case 8 => "\\b" //backspace
    case 9 => "\\t" //tab
    case 10 => "\\n" //lf
    case 11 => "\\v" //vertical tab
    case 12 => "\\f" //formfeed
    case 13 => "\\r" //cr
    case _ => "\\x" + c.toInt.toHexString
  }) + '"'

}

case class PyIntLiteral(val value: Int, numExamples: Int,
                        val predicates: Predicates) extends LiteralNode[Int](numExamples) with PyIntNode
{
  override protected val parenless: Boolean = true
  override val code: String = value.toString

}

case class PyBoolLiteral(val value: Boolean, numExamples: Int,
                         val predicates: Predicates) extends LiteralNode[Boolean](numExamples) with PyBoolNode
{
  override protected val parenless: Boolean = true
  override val code: String = value.toString.capitalize

}