package ast

import ast.Types.Types
import sygus.Predicates

trait ListCompNode[T] extends ListNode[T]
{
  val list: ListNode[_]
  val map: ASTNode
  val varName: String

  override val childType: Types = map.nodeType
  override def computeOnContext(ctx: Map[String, Any]): Option[List[T]] = {
    val (start, length) = predicates.getCurrentIterableRange(ctx, list.values.asInstanceOf[List[Iterable[_]]])
    val value_t = list.values.slice(start, start + length)
    val map_t = map.values.slice(start, start + length)
    val res = map_t.zip(value_t).asInstanceOf[List[T]]
    Some(res)
  }
  override val height: Int = 1 + Math.max(list.height, map.height)
  override val terms: Int = 1 + list.terms + map.terms
  override val children: Iterable[ASTNode] = List(list, map)
  override val code: String = s"[${map.code} for $varName in ${list.code}]"
  override protected val parenless: Boolean = true
  override def includes(varName: String): Boolean =
    varName.equals(this.varName) || list.includes(varName) || map.includes(varName)
  override lazy val usesVariables: Boolean = list.usesVariables || map.usesVariables
  override def updateValues(predicates: Predicates): ASTNode = null

}

class StringToStringListCompNode(val list: ListNode[String], val map: PyStringNode, val varName: String,
                                 val predicates: Predicates) extends ListCompNode[String]
class StringToIntListCompNode(val list: ListNode[String], val map: PyIntNode, val varName: String,
                              val predicates: Predicates) extends ListCompNode[Int]
class IntToStringListCompNode(val list: ListNode[Int], val map: PyStringNode, val varName: String,
                              val predicates: Predicates) extends ListCompNode[String]
class IntToIntListCompNode(val list: ListNode[Int], val map: PyIntNode, val varName: String,
                           val predicates: Predicates) extends ListCompNode[Int]