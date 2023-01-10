package ast

import ast.Types.{Types, childOf}

import scala.collection.immutable.WrappedString

import sygus.Predicates
trait MapCompNode[K,V] extends MapNode[K,V]
{
  val list: IterableNode
  val key: ASTNode
  val value: ASTNode
  val varName: String


  assert(key.values.length == value.values.length, "Key and value did not match")

  override val keyType: Types = Types.childOf(list.nodeType)
  override val valType: Types = value.nodeType

  override def computeOnContext(ctx: Map[String, Any]): Option[Any] = {

    val (start, length) = predicates.getCurrentIterableRange(ctx,list.values.head match {
      case _: String => list.values.take(list.predicates.num_of_examples).asInstanceOf[List[String]].map(new WrappedString(_))
      case _: Iterable[_] => list.values.take(list.predicates.num_of_examples).asInstanceOf[List[Iterable[_]]]
      case _ => ???
    })

    val value_t = value.values.slice(start, start + length)
    val key_t = key.values.slice(start, start + length)
    val res = key_t.zip(value_t).toMap
    Some(res)
  }

  override val height: Int = 1 + Math.max(list.height, value.height)
  override val terms: Int = 1 + list.terms + value.terms
  override val children: Iterable[ASTNode] = List(list, value)
  override protected val parenless: Boolean = true
  override val code: String = s"{${key.code}: ${value.code} for $varName in ${list.code}}"
  override def includes(varName: String): Boolean =
    varName.equals(this.varName) || list.includes(varName) || key.includes(varName) || value.includes(varName)
  override lazy val usesVariables: Boolean = list.usesVariables || key.usesVariables || value.usesVariables

}

trait FilteredMapNode[K,V] extends MapNode[K,V]
{
  val map: MapNode[K,V]
  val filter: PyBoolNode
  val keyName: String

  override val keyType: Types = map.keyType
  override val valType: Types = map.valType

  //override val values: List[Map[K,V]] = filterOp(map, filter)
  override val height: Int = 1 + Math.max(map.height, filter.height)
  override val terms: Int = 1 + map.terms + filter.terms
  override val children: Iterable[ASTNode] = List(map, filter)
  override protected val parenless: Boolean = true
  override val code: String = s"{$keyName: ${map.code}[$keyName] for $keyName in ${map.code} if ${filter.code}}"

  override def computeOnContext(ctx: Map[String, Any]): Option[List[Map[K,V]]] = {
    Some(filterOp(map, filter))
  }

  override def includes(varName: String): Boolean =
    varName.equals(this.keyName) || map.includes(varName) || filter.includes(varName)
  override lazy val usesVariables: Boolean = map.usesVariables || filter.usesVariables
  def make(map: MapNode[K,V], filter: PyBoolNode, keyName: String) : FilteredMapNode[K,V]

  def findKeyVarInNode(node: ASTNode) : Option[VariableNode[_]] =
  {
    node match {
      case n: VariableNode[_] if n.name == keyName => Some(n)
      case n: ASTNode if n.terms > 1 => findKeyVar(n.children)
      case _ => None
    }
  }
  def findKeyVar(nodes: Iterable[ASTNode]) : Option[VariableNode[_]] =
  {
    val keyVar = nodes.map(findKeyVarInNode).filter(_.isDefined)
    if (keyVar.isEmpty) None
    else {
      keyVar.head
    }
  }
  def filterOp(map: MapNode[K,V], filter: PyBoolNode) : List[Map[K,V]] =
  {
    ???
//    val keyNode: VariableNode[Boolean] = findKeyVar(filter.children).get.asInstanceOf[VariableNode[Boolean]]
//    val examples = map.predicates.predicates.map(pred => pred.observe(this).asInstanceOf[Iterable[Any]])
//    val filterValues = splitByIterable(examples, filter.values)
//    val keyValues = splitByIterable(examples, keyNode.values)
//    map.values
//      .zip(keyValues.zip(filterValues).map(tup => tup._1.zip(tup._2)))
//      .map( {
//        case (valMap: Map[K,V], keyMap: Iterable[(K,Boolean)]) =>
//          valMap.filter({
//            case (k: K, _: V) => keyMap.find(_._1.equals(k)).get._2
//          })
//      })
  }

}

class StringStringMapCompNode    (val list: PyStringNode, val key: PyStringNode, val value: PyStringNode,
                                  val varName: String, val predicates: Predicates) extends MapCompNode[String,String]
class StringIntMapCompNode       (val list: PyStringNode, val key: PyStringNode, val value: PyIntNode,
                                  val varName: String, val predicates: Predicates) extends MapCompNode[String,Int]
class StringListStringMapCompNode(val list: ListNode[String], val key: PyStringNode, val value: PyStringNode,
                                  val varName: String, val predicates: Predicates) extends MapCompNode[String,String]
class StringListIntMapCompNode   (val list: ListNode[String], val key: PyStringNode, val value: PyIntNode,
                                  val varName: String, val predicates: Predicates) extends MapCompNode[String,Int]
class IntStringMapCompNode       (val list: ListNode[Int], val key: PyIntNode, val value: PyStringNode,
                                  val varName: String, val predicates: Predicates) extends MapCompNode[Int,String]
class IntIntMapCompNode          (val list: ListNode[Int], val key: PyIntNode, val value: PyIntNode,
                                  val varName: String, val predicates: Predicates) extends MapCompNode[Int,Int]

class StringStringFilteredMapNode(val map: MapNode[String,String], val filter: PyBoolNode,
                                  val keyName: String, val predicates: Predicates) extends FilteredMapNode[String,String]
{
  override def make(map: MapNode[String, String], filter: PyBoolNode, keyName: String): FilteredMapNode[String, String] =
    new StringStringFilteredMapNode(map, filter, keyName, predicates)
}
class StringIntFilteredMapNode(val map: MapNode[String,Int], val filter: PyBoolNode,
                               val keyName: String, val predicates: Predicates) extends FilteredMapNode[String,Int]
{
  override def make(map: MapNode[String, Int], filter: PyBoolNode, keyName: String): FilteredMapNode[String, Int] =
    new StringIntFilteredMapNode(map, filter, keyName, predicates)
}
class IntStringFilteredMapNode(val map: MapNode[Int,String], val filter: PyBoolNode,
                               val keyName: String, val predicates: Predicates) extends FilteredMapNode[Int,String]
{
  override def make(map: MapNode[Int, String], filter: PyBoolNode, keyName: String): FilteredMapNode[Int, String] =
    new IntStringFilteredMapNode(map, filter, keyName, predicates)
}
class IntIntFilteredMapNode(val map: MapNode[Int,Int], val filter: PyBoolNode,
                            val keyName: String, val predicates: Predicates) extends FilteredMapNode[Int,Int]
{
  override def make(map: MapNode[Int, Int], filter: PyBoolNode, keyName: String): FilteredMapNode[Int, Int] =
    new IntIntFilteredMapNode(map, filter, keyName, predicates)
}
