package ast
import sygus.Predicates

abstract class LiteralNode[T](numExamples: Int) extends ASTNode{
  assert(numExamples > 0)
  val height = 0
  val terms = 1
  val value: T

  override def computeOnContext(ctx: Map[String, Any]) = Some(value)
  override def children: Iterable[ASTNode] = Iterable.empty
  def includes(varName: String): Boolean = false
  override lazy val usesVariables: Boolean = false

}
case class StringLiteral(value: String, numExamples: Int,
                         predicates: Predicates) extends LiteralNode[String](numExamples) with StringNode{
  override lazy val code: String = '"' + value + '"' //escape?
  override protected val parenless: Boolean = true
  override def updateValues(predicates_t: Predicates) = this.copy(value = value, numExamples = predicates_t.num_of_examples,
    predicates = predicates_t)

}

case class IntLiteral(value: Int, numExamples: Int,
                      predicates: Predicates) extends LiteralNode[Int](numExamples) with IntNode{
  override lazy val code: String = value.toString
  override protected val parenless: Boolean = true
  override def updateValues(predicates_t: Predicates) = this.copy(value = value, numExamples = predicates_t.num_of_examples,
    predicates = predicates_t)

}

case class BoolLiteral(value: Boolean, numExamples: Int,
                       predicates: Predicates) extends LiteralNode[Boolean](numExamples) with BoolNode {
  override lazy val code: String = value.toString
  override protected val parenless: Boolean = true
  override def updateValues(predicates_t: Predicates) = this.copy(value = value, numExamples = predicates_t.num_of_examples,
    predicates = predicates_t)

}

case class BVLiteral(value: Long, numExamples: Int,
                     predicates: Predicates) extends LiteralNode[Long](numExamples) with BVNode {
  override lazy val code: String = f"#x$value%016x"
  override protected val parenless: Boolean = true
  override def updateValues(predicates_t: Predicates) = this.copy(value = value, numExamples = predicates_t.num_of_examples,
    predicates = predicates_t )


}

case class PyStringLiteral(value: String, numExamples: Int,
                           predicates: Predicates) extends LiteralNode[String](numExamples) with PyStringNode
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

//  override def updateValues(predicates_t: Predicates): PyStringLiteral = this.copy(value = value, numExamples = predicates_t.num_of_examples,
//    predicates = predicates_t)
  override def updateValues(predicates_t: Predicates) = this.copy(numExamples = predicates_t.num_of_examples, predicates = predicates_t)

}

case class PyIntLiteral(value: Int, numExamples: Int,
                        predicates: Predicates) extends LiteralNode[Int](numExamples) with PyIntNode
{
  override protected val parenless: Boolean = true
  override val code: String = value.toString

  override def updateValues(predicates_t: Predicates) = this.copy(numExamples = predicates_t.num_of_examples, predicates = predicates_t)
//  override def updateValues(predicates_t: Predicates) = this.copy(value = value, numExamples = predicates_t.num_of_examples,
//    predicates = predicates_t)
}

case class PyBoolLiteral(value: Boolean, numExamples: Int,
                         predicates: Predicates) extends LiteralNode[Boolean](numExamples) with PyBoolNode
{
  override protected val parenless: Boolean = true
  override val code: String = value.toString.capitalize
  override def updateValues(predicates_t: Predicates) = this.copy(value = value, numExamples = predicates_t.num_of_examples,
    predicates = predicates_t)
}