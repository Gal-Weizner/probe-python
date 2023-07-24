package ast

import ast.Types.Types
import trace.DebugPrints.eprintln
import sygus.Predicates

trait BinaryOpNode[T] extends ASTNode{
  val lhs: ASTNode
  val rhs: ASTNode
  override val height: Int = 1 + Math.max(lhs.height,rhs.height)
  override val terms: Int = 1 + lhs.terms + rhs.terms

  if (lhs.values.length != rhs.values.length) println(lhs.code, lhs.values, rhs.code, rhs.values)
  assert(lhs.values.length == rhs.values.length)
  def doOp(l: Any, r: Any): Option[T]
  def make(l: ASTNode, r: ASTNode): BinaryOpNode[T]
  override def computeOnContext(ctx: Map[String, Any]): Option[Any] = doOp(lhs.predicates.getExampleValue(lhs.values, ctx),rhs.predicates.getExampleValue(rhs.values, ctx))


  override val children: Iterable[ASTNode] = Iterable(lhs,rhs)
  override lazy val usesVariables: Boolean = lhs.usesVariables || rhs.usesVariables

  def includes(varName: String): Boolean = lhs.includes(varName) || rhs.includes(varName)

  protected def wrongType(l: Any, r: Any): Option[T] =
  {
    eprintln(s"[${this.getClass.getSimpleName}] Wrong value types: $l $r")
    None
  }
}

case class StringConcat(val lhs: StringNode, val rhs: StringNode, val predicates: Predicates) extends BinaryOpNode[String] with StringNode {
  override protected val parenless: Boolean = true

  override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
    case (l: String, r: String) => Some(l.asInstanceOf[String] + r.asInstanceOf[String])
    case _ => wrongType(l, r)
  }
  override lazy val code: String = "(str.++ " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
    new StringConcat(l.asInstanceOf[StringNode], r.asInstanceOf[StringNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t).asInstanceOf[StringNode], rhs = rhs.updateValues(predicates_t).asInstanceOf[StringNode],
    predicates = predicates_t)
}

case class StringAt(val lhs: StringNode, val rhs: IntNode, val predicates: Predicates) extends BinaryOpNode[String] with StringNode {

  override protected val parenless: Boolean = true

  override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
    case (l: Any, r: Any) =>
      val str = l.asInstanceOf[String]
      val idx = r.asInstanceOf[Int]
      if (idx < 0 || idx >= str.length) Some("")
      else Some(str(idx).toString)
    case _ => wrongType(l, r)
  }

  override lazy val code: String = "(str.at " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
    new StringAt(l.asInstanceOf[StringNode], r.asInstanceOf[IntNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[StringNode], rhs =rhs.updateValues(predicates_t: Predicates).asInstanceOf[IntNode],
    predicates = predicates_t)
}

case class IntAddition(val lhs: IntNode, val rhs: IntNode, val predicates: Predicates) extends BinaryOpNode[Int] with IntNode {
  override protected val parenless: Boolean = true

  override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
    case (l: Int, r: Int) => Some(l.asInstanceOf[Int] + r.asInstanceOf[Int])
    case _ => wrongType(l, r)
  }

  override lazy val code: String = "(+ " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new IntAddition(l.asInstanceOf[IntNode], r.asInstanceOf[IntNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[IntNode], rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[IntNode],
    predicates = predicates_t)
}

case class IntSubtraction(val lhs: IntNode, val rhs: IntNode, val predicates: Predicates) extends BinaryOpNode[Int] with IntNode {
  override protected val parenless: Boolean = true

  override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
    case (l: Int, r: Int) => Some(l.asInstanceOf[Int] - r.asInstanceOf[Int])
    case _ => wrongType(l, r)
  }
  override lazy val code: String = "(- " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new IntSubtraction(l.asInstanceOf[IntNode], r.asInstanceOf[IntNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[IntNode],
    rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[IntNode], predicates = predicates_t)
}

case class IntLessThanEq(val lhs: IntNode, val rhs: IntNode, val predicates: Predicates) extends BinaryOpNode[Boolean] with BoolNode {
  override protected val parenless: Boolean = true

  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: Int, r: Int) => Some(l <= r)
    case _ => wrongType(l, r)
  }
  override lazy val code: String = "(<= " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new IntLessThanEq(l.asInstanceOf[IntNode], r.asInstanceOf[IntNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[IntNode],
    rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[IntNode], predicates = predicates_t)
}

case class PrefixOf(val lhs: StringNode, val rhs: StringNode, val predicates: Predicates) extends BinaryOpNode[Boolean] with BoolNode {

  override protected val parenless: Boolean = true

  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: String, r: String) => Some(r.asInstanceOf[String].startsWith(l.asInstanceOf[String]))
    case _ => wrongType(l, r)
  }

  override lazy val code: String = "(str.prefixof " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new PrefixOf(l.asInstanceOf[StringNode], r.asInstanceOf[StringNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[StringNode],
    rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[StringNode], predicates = predicates_t)
}

case class SuffixOf(val lhs: StringNode, val rhs: StringNode, val predicates: Predicates) extends BinaryOpNode[Boolean] with BoolNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: String, r: String) => Some(r.asInstanceOf[String].endsWith(l.asInstanceOf[String]))
    case _ => wrongType(l, r)
  }
  override lazy val code: String = "(str.suffixof " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new SuffixOf(l.asInstanceOf[StringNode], r.asInstanceOf[StringNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[StringNode],
    rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[StringNode], predicates = predicates_t)
}

case class PyLessThanEq(val lhs: PyIntNode, val rhs: PyIntNode, val predicates: Predicates) extends BinaryOpNode[Boolean] with PyBoolNode {
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.code + " <= " + rhs.code

  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: Int, r: Int) => Some(l <= r)
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new PyLessThanEq(l.asInstanceOf[PyIntNode], r.asInstanceOf[PyIntNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[PyIntNode],
    rhs.updateValues(predicates_t: Predicates).asInstanceOf[PyIntNode], predicates = predicates_t)


}

case class PyGreaterThan(val lhs: PyIntNode, val rhs: PyIntNode, val predicates: Predicates) extends BinaryOpNode[Boolean] with PyBoolNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.code + " > " + rhs.code

  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: Int, r: Int) => Some(l > r)
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new PyGreaterThan(l.asInstanceOf[PyIntNode], r.asInstanceOf[PyIntNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[PyIntNode],
    rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[PyIntNode], predicates = predicates_t)


}

case class PyStringConcat(val lhs: PyStringNode, val rhs: PyStringNode, val predicates: Predicates) extends BinaryOpNode[String] with PyStringNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.code + " + " + rhs.code
  override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
    case (l: String, r: String) => Some(l + r)
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
    new PyStringConcat(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyStringNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t).asInstanceOf[PyStringNode],
    rhs = rhs.updateValues(predicates_t).asInstanceOf[PyStringNode], predicates = predicates_t)


}

case class PyMapGet(val lhs: MapNode[String,Int], val rhs: PyStringNode, val predicates: Predicates) extends BinaryOpNode[Int] with PyIntNode
{
  override protected val parenless: Boolean = true
  override lazy val code: String = lhs.parensIfNeeded + "[" + rhs.code + "]"

  override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
    case (map: Map[String,Int], key: String) => map.get(key)
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new PyMapGet(l.asInstanceOf[MapNode[String,Int]], r.asInstanceOf[PyStringNode], predicates)
  override def updateValues(predicates_t: Predicates) = copy(lhs = lhs, rhs, predicates = predicates_t)


}

abstract class ListLookup[T](lhs: ListNode[T], rhs: PyIntNode, predicates: Predicates) extends BinaryOpNode[T] {
  override protected val parenless: Boolean = true
  override val nodeType: Types = lhs.childType
  override val code: String = s"${lhs.parensIfNeeded}[${rhs.code}]"

  override def doOp(l: Any, r: Any): Option[T] = (l, r) match {
    case (lst: List[T], idx: Int) =>
      if (idx >= 0 && idx < lst.length) Some(lst(idx)) else None
    case _ => wrongType(l, r)
  }
}
case class PyStringListLookup(lhs: ListNode[PyStringNode], rhs: PyIntNode, predicates: Predicates)
  extends ListLookup[PyStringNode](lhs, rhs, predicates) with PyStringNode {
  override protected val parenless: Boolean = true
  override val code: String = s"${lhs.parensIfNeeded}[${rhs.code}]"

  override def make(lhs: ASTNode, rhs: ASTNode): ListLookup[PyStringNode] =
    PyStringListLookup(lhs.asInstanceOf[ListNode[PyStringNode]], rhs.asInstanceOf[PyIntNode], predicates)
  override def updateValues(predicates_t: Predicates) = copy(predicates = predicates_t)

}

case class PyIntListLookup(lhs: ListNode[PyIntNode], rhs: PyIntNode, predicates: Predicates)
  extends ListLookup[PyIntNode](lhs, rhs, predicates) with PyIntNode {
  override protected val parenless: Boolean = true
  override val code: String = s"${lhs.parensIfNeeded}[${rhs.code}]"

  override def make(lhs: ASTNode, rhs: ASTNode): ListLookup[PyIntNode] =
    PyIntListLookup(lhs.asInstanceOf[ListNode[PyIntNode]], rhs.asInstanceOf[PyIntNode], predicates)
  override def updateValues(predicates_t: Predicates) = copy(predicates = predicates_t)

}

case class PyIntAddition(val lhs: PyIntNode, val rhs: PyIntNode, val predicates: Predicates) extends BinaryOpNode[Int] with PyIntNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.code + " + " + rhs.code

  override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
    case (l: Int, r: Int) => Some(l.asInstanceOf[Int] + r.asInstanceOf[Int])
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new PyIntAddition(l.asInstanceOf[PyIntNode], r.asInstanceOf[PyIntNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[PyIntNode],
    rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[PyIntNode], predicates = predicates_t)


}

case class PyIntMultiply(val lhs: PyIntNode, val rhs: PyIntNode, val predicates: Predicates) extends BinaryOpNode[Int] with PyIntNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.code + " * " + rhs.code

  override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
    case (l: Int, r: Int) => Some(l.asInstanceOf[Int] * r.asInstanceOf[Int])
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new PyIntMultiply(l.asInstanceOf[PyIntNode], r.asInstanceOf[PyIntNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs.updateValues(predicates_t: Predicates).asInstanceOf[PyIntNode],
    rhs.updateValues(predicates_t: Predicates).asInstanceOf[PyIntNode], predicates = predicates_t)

}

case class PyStringMultiply(val lhs: PyStringNode, val rhs: PyIntNode, val predicates: Predicates) extends BinaryOpNode[String] with PyStringNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.code + " * " + rhs.code

  override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
    case (l: String, r: Int) => Some(l.asInstanceOf[String] * r.asInstanceOf[Int])
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
    new PyStringMultiply(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyIntNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[PyStringNode],
    rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[PyIntNode], predicates = predicates_t)

}

case class PyIntSubtraction(val lhs: PyIntNode, val rhs: PyIntNode, val predicates: Predicates) extends BinaryOpNode[Int] with PyIntNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.code + " - " + rhs.code

  override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
    case (l: Int, r: Int) => Some(l - r)
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new PyIntSubtraction(l.asInstanceOf[PyIntNode], r.asInstanceOf[PyIntNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[PyIntNode],
    rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[PyIntNode], predicates = predicates_t)

}

case class PyIntDivision(val lhs: PyIntNode, val rhs: PyIntNode, val predicates: Predicates) extends BinaryOpNode[Int] with PyIntNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String =
    lhs.parensIfNeeded + " // " + rhs.parensIfNeeded

  override def doOp(l: Any, r: Any): Option[Int] =
    (l, r) match {
      case (_: Int, 0) => None
      case (l: Int, r: Int) => Some(l / r)
      case _ => wrongType(l, r)
    }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new PyIntDivision(lhs.asInstanceOf[PyIntNode], rhs.asInstanceOf[PyIntNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[PyIntNode],
    rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[PyIntNode], predicates = predicates_t)


}

case class PyFind(val lhs: PyStringNode, val rhs: PyStringNode, val predicates: Predicates) extends BinaryOpNode[Int] with PyIntNode
{
  override protected val parenless: Boolean = true
  override lazy val code: String = lhs.parensIfNeeded + ".find(" + rhs.code + ")"

  override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
    case (l: String, r: String) => Some(l.indexOf(r))
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new PyFind(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyStringNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[PyStringNode],
    rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[PyStringNode], predicates = predicates_t)


}

case class PyContains(val lhs: PyStringNode, val rhs: PyStringNode, val predicates: Predicates) extends BinaryOpNode[Boolean] with PyBoolNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.parensIfNeeded + " in " + rhs.parensIfNeeded

  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (substr: String, str: String) => Some(str.contains(substr))
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new PyContains(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyStringNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[PyStringNode],
    rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[PyStringNode], predicates = predicates_t)


}

case class PyStringSplit(val lhs: PyStringNode, val rhs: PyStringNode, val predicates: Predicates) extends BinaryOpNode[Iterable[String]] with StringListNode
{
  override protected val parenless: Boolean = true
  override lazy val code: String = lhs.parensIfNeeded + ".split(" + rhs.code + ")"

  override def doOp(l: Any, r: Any): Option[Iterable[String]] = (l, r) match {
    case (_, "") => None
    case (l: String, r: String) => Some(l.split(r).toList)
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Iterable[String]] =
    new PyStringSplit(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyStringNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[PyStringNode],
    rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[PyStringNode], predicates = predicates_t)


}

case class PyStringJoin(val lhs: PyStringNode, val rhs: ListNode[String], val predicates: Predicates) extends BinaryOpNode[String] with PyStringNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.parensIfNeeded + ".join(" + rhs.code + ")"

  override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
    case (str: String, lst: Iterable[_]) => Some(lst.mkString(str))
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
    new PyStringJoin(l.asInstanceOf[PyStringNode], r.asInstanceOf[ListNode[String]], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[PyStringNode],
    rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[ListNode[String]], predicates = predicates_t)


}

case class PyCount(val lhs: PyStringNode, val rhs: PyStringNode, val predicates: Predicates) extends BinaryOpNode[Int] with PyIntNode {
  override protected val parenless: Boolean = true
  override lazy val code: String = lhs.parensIfNeeded + ".count(" + rhs.code + ")"

  override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
    case ("", _) => Some(0)
    case (l: String, "") => Some(l.length + 1)
    case (l: String, r: String) => {
      var count = 0
      var i = 0
      while (i != -1) {
        val nextInstance = l.indexOf(r, i)
        if (nextInstance > -1) {
          count += 1
          i = nextInstance + r.length
        }
        else i = -1
      }
      Some(count)
    }
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new PyCount(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyStringNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[PyStringNode],
    rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[PyStringNode], predicates = predicates_t)


}

case class PyBinarySubstring(val lhs: PyStringNode, val rhs: PyIntNode, val predicates: Predicates) extends BinaryOpNode[String] with PyStringNode
{
  override protected val parenless: Boolean = true
  override lazy val code: String = lhs.parensIfNeeded  + "[" + rhs.code + "]"

  override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
    case (str: String, idx: Int) =>
      if (idx < 0 || idx >= str.length) None
      else Some(str(idx).toString)
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
    new PyBinarySubstring(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyIntNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[PyStringNode],
    rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[PyIntNode], predicates = predicates_t)


}

case class PyStartsWith(val lhs: PyStringNode, val rhs: PyStringNode, val predicates: Predicates) extends BinaryOpNode[Boolean] with PyBoolNode {
  override protected val parenless: Boolean = true
  override lazy val code: String = lhs.code + ".startswith(" + rhs.code + ")"
  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: String, r: String) =>  Some(l.asInstanceOf[String].startsWith(r.asInstanceOf[String]))
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new PyStartsWith(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyStringNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs.updateValues(predicates_t: Predicates).asInstanceOf[PyStringNode],
    rhs.updateValues(predicates_t: Predicates).asInstanceOf[PyStringNode], predicates = predicates_t)


}

case class PyEndsWith(val lhs: PyStringNode, val rhs: PyStringNode, val predicates: Predicates) extends BinaryOpNode[Boolean] with PyBoolNode {
  override protected val parenless: Boolean = true
  override lazy val code: String = lhs.code + ".endswith(" + rhs.code + ")"
  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: String, r: String) =>  Some(l.asInstanceOf[String].endsWith(r.asInstanceOf[String]))
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new PyEndsWith(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyStringNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[PyStringNode],
    rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[PyStringNode], predicates = predicates_t)

}


case class PyStringStep(val lhs: PyStringNode, val rhs: PyIntNode, val predicates: Predicates) extends BinaryOpNode[String] with PyStringNode
{
  override protected val parenless: Boolean = true
  override lazy val code: String = lhs.parensIfNeeded + "[::" + rhs.code + "]"

  override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
    case (_, _: 0) => None
    case (str: String, step: Int) =>
      var rs: StringBuilder = new StringBuilder(Math.abs(str.length / step) + 1)
      var idx = if (step > 0) 0 else str.length - 1
      while (idx >= 0 && idx < str.length) {
        rs += str(idx)
        idx += step
      }
      Some(rs.toString)
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
    new PyStringStep(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyIntNode], predicates)
  override def updateValues(predicates_t: Predicates) = this.copy(lhs = lhs.updateValues(predicates_t: Predicates).asInstanceOf[PyStringNode],
    rhs = rhs.updateValues(predicates_t: Predicates).asInstanceOf[PyIntNode], predicates = predicates_t)


}
