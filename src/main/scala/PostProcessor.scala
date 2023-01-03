package sygus

import ast._

object PostProcessor
{
  def clean(node: ASTNode, predicates: Predicates): ASTNode = if (!node.usesVariables && node.values.toSet.size == 1) //second check is a tad redundant but just to be safe
    Types.typeof(node.values(0)) match {
      case Types.PyString => new PyStringLiteral(node.values(0).asInstanceOf[String],node.values.length, predicates)
      case Types.PyBool => new PyBoolLiteral(node.values(0).asInstanceOf[Boolean],node.values.length, predicates)
      case Types.PyInt => new PyIntLiteral(node.values(0).asInstanceOf[Int],node.values.length, predicates)
      case _ => node
    }
  else node match {
    case add: PyIntAddition =>
      val lhs: PyIntNode = clean(add.lhs, predicates).asInstanceOf[PyIntNode]
      val rhs: PyIntNode = clean(add.rhs, predicates).asInstanceOf[PyIntNode]

      (lhs, rhs) match {
        case (a: PyIntLiteral, b: PyIntLiteral) => new PyIntLiteral(a.value + b.value, a.values.length, predicates)
        case _ => new PyIntAddition(lhs, rhs, predicates)
      }
    case sub: PyIntSubtraction =>
      val lhs: PyIntNode = clean(sub.lhs, predicates).asInstanceOf[PyIntNode]
      val rhs: PyIntNode = clean(sub.rhs, predicates).asInstanceOf[PyIntNode]

      (lhs, rhs) match {
        case (a: PyIntLiteral, b: PyIntLiteral) => new PyIntLiteral(a.value - b.value, a.values.length, predicates)
        case _ => new PyIntSubtraction(lhs, rhs, predicates)
      }
    case sub: PyIntDivision =>
      val lhs: PyIntNode = clean(sub.lhs, predicates).asInstanceOf[PyIntNode]
      val rhs: PyIntNode = clean(sub.rhs, predicates).asInstanceOf[PyIntNode]

      (lhs, rhs) match {
        case (a: PyIntLiteral, b: PyIntLiteral) => new PyIntLiteral(a.value / b.value, a.values.length, predicates)
        case _ => new PyIntDivision(lhs, rhs, predicates)
      }
    case concat: PyStringConcat =>
      val lhs: PyStringNode = clean(concat.lhs, predicates).asInstanceOf[PyStringNode]
      val rhs: PyStringNode = clean(concat.rhs, predicates).asInstanceOf[PyStringNode]
      (lhs, rhs) match {
        case (a: PyStringLiteral, b: PyStringLiteral) => new PyStringLiteral(a.value + b.value, a.values.length, predicates)
        case _ => new PyStringConcat(lhs, rhs, predicates)
      }
    case uni: UnaryOpNode[_] =>
      val arg = clean(uni.arg, predicates)
      uni.make(arg)
    case bin: BinaryOpNode[_] =>
      val lhs: ASTNode = clean(bin.lhs, predicates)
      val rhs: ASTNode = clean(bin.rhs, predicates)
      bin.make(lhs, rhs)
    case ter: TernaryOpNode[_] =>
      val arg0: ASTNode = clean(ter.arg0, predicates)
      val arg1: ASTNode = clean(ter.arg1, predicates)
      val arg2: ASTNode = clean(ter.arg2, predicates)
      ter.make(arg0, arg1, arg2)
    case qua: QuaternaryOpNode[_] =>
      val arg0: ASTNode = clean(qua.arg0, predicates)
      val arg1: ASTNode = clean(qua.arg1, predicates)
      val arg2: ASTNode = clean(qua.arg2, predicates)
      val arg3: ASTNode = clean(qua.arg3, predicates)
      qua.make(arg0, arg1, arg2, arg3)
    case map: MapCompNode[a,b] =>
      val list = clean(map.list, predicates)
      val key = clean(map.key, predicates)
      val value = clean(map.value, predicates)

      map.list.nodeType match {
        case Types.PyString =>
          map.value.nodeType match {
            case Types.PyString =>
              new StringStringMapCompNode(
                list.asInstanceOf[PyStringNode],
                key.asInstanceOf[PyStringNode],
                value.asInstanceOf[PyStringNode],
                map.varName, predicates)

            case Types.PyInt =>
              new StringIntMapCompNode(
                list.asInstanceOf[PyStringNode],
                key.asInstanceOf[PyStringNode],
                value.asInstanceOf[PyIntNode],
                map.varName, predicates)
          }
        case Types.StringList =>
          map.value.nodeType match {
            case Types.PyString =>
              new StringListStringMapCompNode(
                list.asInstanceOf[StringListNode],
                key.asInstanceOf[PyStringNode],
                value.asInstanceOf[PyStringNode],
                map.varName, predicates)
            case Types.PyInt =>
              new StringListIntMapCompNode(
                list.asInstanceOf[StringListNode],
                key.asInstanceOf[PyStringNode],
                value.asInstanceOf[PyIntNode],
                map.varName, predicates)
          }
        case Types.IntList =>
          map.value.nodeType match {
            case Types.PyString =>
              new IntStringMapCompNode(
                list.asInstanceOf[IntListNode],
                key.asInstanceOf[PyIntNode],
                value.asInstanceOf[PyStringNode],
                map.varName, predicates)
            case Types.PyInt =>
              new IntIntMapCompNode(
                list.asInstanceOf[IntListNode],
                key.asInstanceOf[PyIntNode],
                value.asInstanceOf[PyIntNode],
                map.varName, predicates)
          }
      }
    case map: FilteredMapNode[a,b] =>
      val mapNode: MapNode[a,b] = clean(map.map, predicates).asInstanceOf[MapNode[a,b]]
      val filter: PyBoolNode = clean(map.filter, predicates).asInstanceOf[PyBoolNode]
      map.make(mapNode, filter, map.keyName)
    case n => n
  }
}
