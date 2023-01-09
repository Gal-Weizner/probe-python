package ast

import ast.Types.Types
import sygus.Predicates

abstract class VariableNode[T](predicates: Predicates) extends ASTNode
{
  override lazy val code: String = name
  override val height: Int = 0
  override val children: Iterable[ASTNode] = Iterable.empty
  val terms = 1
  val name: String
  override protected val parenless: Boolean = true

  override def computeOnContext(ctx: Map[String, Any]): Option[Any] =
  {
    Some(ctx(name))
  }

  def includes(varName: String): Boolean = name == varName
  override lazy val usesVariables: Boolean = true
}

case class StringVariable(val name: String,
                          val predicates: Predicates) extends VariableNode[String](predicates) with StringNode

case class PyStringVariable(val name: String, val predicates: Predicates) extends VariableNode[String](predicates) with PyStringNode

case class IntVariable(val name: String, val predicates: Predicates) extends VariableNode[Int](predicates) with IntNode

case class PyIntVariable(val name: String, val predicates: Predicates) extends VariableNode[Int](predicates) with PyIntNode

case class BoolVariable(val name: String, val predicates: Predicates) extends VariableNode[Boolean](predicates) with BoolNode

case class PyBoolVariable(val name: String, val predicates: Predicates) extends VariableNode[Boolean](predicates) with PyBoolNode

case class BVVariable(val name: String, val predicates: Predicates) extends VariableNode[Long](predicates) with BVNode

case class ListVariable[T](val name: String, val childType: Types,
                           val predicates: Predicates) extends VariableNode[List[T]](predicates) with ListNode[T]

case class MapVariable[K,V](val name: String, val keyType: Types, val valType: Types,
                            val predicates: Predicates) extends VariableNode[Map[K,V]](predicates) with MapNode[K,V]
