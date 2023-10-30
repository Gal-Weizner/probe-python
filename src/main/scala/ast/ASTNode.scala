package ast

import ast.Types.Types
import com.sun.jndi.ldap.LdapCtx
import enumeration.{ProbUpdate}
import sygus.{Predicates}
trait ASTNode {
  val nodeType: Types.Types
  val predicates: Predicates
  val values: List[Any] = predicates.predicates.map(pred => pred.observe(this)) match {
    case l if l.forall(_.isDefined) => l.map(_.get)
    case _ => Nil
  }
  val code: String
  val height: Int
  val terms: Int
  def children: Iterable[ASTNode]
  def computeOnContext(ctx: Map[String, Any]): Option[Any]
  def includes(varName: String): Boolean
  protected val parenless: Boolean
  def parensIfNeeded: String = if (height > 0 && !parenless) "(" + code + ")" else code
  val usesVariables: Boolean
  private var _cost : Option[Int] = None

  def cost: Int = {
    if (_cost.isEmpty) renewCost()
    _cost.get
  }
  def renewCost(): Unit = {
    children.foreach(_.renewCost)
    _cost = Some(ProbUpdate.getRootPrior(this) + children.map(c => c.cost).sum)
  }
  def updateValues(predicates: Predicates): ASTNode
}

trait StringNode extends ASTNode {
  override val nodeType = Types.String

}

trait IntNode extends ASTNode {
  override val nodeType = Types.Int
}

trait BoolNode extends ASTNode {

  override val nodeType = Types.Bool
}

trait IterableNode extends ASTNode {

  def splitByIterable[A](values: Iterable[Iterable[_]], listToSplit: Iterable[A]): List[Iterable[A]] =
  {
    var rs: List[Iterable[A]] = Nil
    var start = 0
    for (delta <- values.map(_.size)) {
      rs = rs :+ listToSplit.slice(start, start + delta)
      start += delta
    }
    rs
  }
}

trait PyStringNode extends IterableNode {
  override val nodeType = Types.PyString
}

trait PyIntNode extends IterableNode {
  override val nodeType = Types.PyInt
}

trait PyBoolNode extends IterableNode {
  override val nodeType = Types.PyBool
}

trait ListNode[T] extends IterableNode {
  val childType: Types
  override lazy val nodeType: Types = Types.List(childType)
}

trait StringListNode extends ListNode[String] { override val childType: Types = Types.PyString }
trait IntListNode extends ListNode[Int] { override val childType: Types = Types.PyInt}
trait BoolListNode extends ListNode[Boolean]  { override val childType: Types = Types.PyBool}

trait MapNode[K,V] extends IterableNode
{
  val keyType: Types
  val valType: Types

  override lazy val nodeType: Types = Types.Map(keyType, valType)
}

trait StringStringMapNode extends MapNode[String,String]
{
  override val keyType: Types = Types.PyString
  override val valType: Types = Types.PyString
}

trait StringIntMapNode extends MapNode[String,Int]
{
  override val keyType: Types = Types.PyString
  override val valType: Types = Types.PyInt
}

trait IntStringMapNode extends MapNode[Int,String]
{
  override val keyType: Types = Types.PyInt
  override val valType: Types = Types.PyString
}

trait IntIntMapNode extends MapNode[Int,Int]
{
  override val keyType: Types = Types.PyInt
  override val valType: Types = Types.PyInt
}

trait BVNode extends ASTNode {
  override val nodeType = Types.BitVec64
}