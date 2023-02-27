package ast

import trace.DebugPrints.eprintln
import sygus.{Predicate, Predicates}

trait QuaternaryOpNode[T] extends ASTNode
{
  val arg0: ASTNode
  val arg1: ASTNode
  val arg2: ASTNode
  val arg3: ASTNode
  override def computeOnContext(ctx: Map[String, Any]): Option[Any] =
  {
    doOp(arg0.predicates.getExampleValue(arg0.values, ctx),arg1.predicates.getExampleValue(arg1.values, ctx),
      arg2.predicates.getExampleValue(arg2.values, ctx), arg3.predicates.getExampleValue(arg3.values, ctx))
  }
//  lazy val values: List[T] = arg0.values
//    .zip(arg1.values)
//    .zip(arg2.values)
//    .zip(arg3.values)
//    .map(tup => doOp(tup._1._1._1, tup._1._1._2, tup._1._2, tup._2)) match {
//    case l if l.forall(_.isDefined) => l.map(_.get)
//    case _ => Nil
//  }

  override val height: Int = 1 + Math.max(arg0.height, Math.max(arg1.height, Math.max(arg2.height, arg3.height)))
  override val terms : Int = 1 + arg0.terms + arg1.terms + arg2.terms + arg3.terms
  override val children: Iterable[ASTNode] = Iterable(arg0, arg1, arg2, arg3)

  assert(arg0.values.length == arg1.values.length &&
    arg1.values.length == arg2.values.length &&
    arg2.values.length == arg3.values.length)

  def doOp(a0: Any, a1: Any, a2: Any, a3: Any): Option[T]

  def make(a0: ASTNode, a1: ASTNode, a2: ASTNode, a3: ASTNode): QuaternaryOpNode[T]

  def includes(varName: String): Boolean =
    arg0.includes(varName) ||
      arg1.includes(varName) ||
      arg2.includes(varName) ||
      arg3.includes(varName)

  override lazy val usesVariables: Boolean =
    arg0.usesVariables || arg1.usesVariables ||
      arg2.usesVariables || arg3.usesVariables
  override def updateValues(predicates: Predicates) = null


}

// TODO Test is extensively before adding it
class QuaternarySubstring(val arg0: PyStringNode, val arg1: PyIntNode,val arg2: PyIntNode,
                          val arg3: PyIntNode,  val predicates: Predicates) extends QuaternaryOpNode[String] with PyStringNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String =
    arg0.parensIfNeeded + "[" + arg1.code + ":" + arg2.code + ":"  + arg3.code + "]"

  override def doOp(a0: Any, a1: Any, a2: Any, a3: Any): Option[String] = (a0, a1, a2, a3) match {
    case (_, _, _, 0) => None
    case (s: String, start_orig: Int, end_orig: Int, step: Int) =>
      val start = (if (start_orig >= 0) start_orig else (s.length + start_orig)).max(0).min(s.length)
      val end = (if (end_orig >= 0) end_orig else (s.length + end_orig)).max(0).min(s.length)

      var rs = ""
      var idx = start

      if (step > 0) {
        while (idx < end) {
          if (idx < s.length) rs += s(idx)
          idx += step
        }
      } else if (step < 0) {
        while (idx > end) {
          if (idx < s.length) rs += s(idx)
          idx += step
        }
      }

      Some(rs)
    case _ =>
      eprintln(s"Wrong types: $arg0 $arg1 $arg2 $arg3")
      None
  }

  override def make(a0: ASTNode, a1: ASTNode, a2: ASTNode, a3: ASTNode): QuaternaryOpNode[String] =
    new QuaternarySubstring(
      a0.asInstanceOf[PyStringNode],
      a1.asInstanceOf[PyIntNode],
      a2.asInstanceOf[PyIntNode],
      a3.asInstanceOf[PyIntNode], predicates)
}
