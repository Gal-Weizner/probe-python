package sygus
import ast.{ASTNode, LiteralNode, Types, VariableNode}

import scala.collection.mutable
import scala.reflect.internal.util.TriState.{False, True}

trait Predicate {
  val location: Int
  def observe(program:ASTNode): Option[Any]
  def check(program: ASTNode): Boolean
//  def set_index(): Any
}

class Predicates(var predicates: List[Predicate], var num_of_examples: Int) {

  def getCurrentIterableRange(ctx: Map[String, Any], list_values: List[Iterable[_]]): (Int, Int) = {
    var start = 0
    for ((pred, l) <- this.getExamplePredicates().zip(list_values))
      if (pred.context == ctx) return (start, l.size)
      else start += l.size
    (-1, -1)
  }

//  def set_index() =
//    {
//
//    }

  def getExampleValue(values: List[Any], ctx: Map[String, Any]): Any = {
    val res = predicates.zipWithIndex.find { case (p, _) => p.isInstanceOf[ExamplePredicate] && p.asInstanceOf[ExamplePredicate].context == ctx }
    values(res.get._2)
  }

  def getExamplePredicates(): List[ExamplePredicate] = {
    predicates.take(num_of_examples).asInstanceOf[List[ExamplePredicate]]
  }

  def getNonExamplePredicates(): List[Predicate] = {
    predicates.takeRight(predicates.length - num_of_examples)
  }

  def allHolds(program: ASTNode): Boolean = {
    predicates.forall(pred => pred.check(program))
  }

  def someHolds(program: ASTNode): Boolean = {
    predicates.dropRight(1).exists(pred => pred.check(program))
  }
}

object Predicates {
  def apply(predicates: List[Predicate], num_of_examples: Int) = new Predicates(predicates, num_of_examples)
}
class ExamplePredicate (val context: Map[String, Any], val output: Option[Any], val location: Int) extends Predicate
{
  override def observe(program: ASTNode): Option[Any] = program.computeOnContext(context)

  override def check(program: ASTNode): Boolean = program.predicates.getExampleValue(program.values, this.context) == this.output.get

  def updatePredicate(var_name: String, value: Any, location: Int): ExamplePredicate =
  {
    new ExamplePredicate(this.context + (var_name -> value), None, location)
  }
}
object ExamplePredicate{
  def apply(ctx: Map[String, Any], output: Option[Any], location: Int) = new ExamplePredicate(ctx, output, location)
}
class UsesVariablesPredicate(val location: Int) extends Predicate
{
  override def observe(program: ASTNode): Option[Any] = Some(program.usesVariables)

  override def check(program: ASTNode): Boolean = program.values.last.asInstanceOf[Boolean]
}

object UsesVariablesPredicate{
  def apply(location: Int) = new UsesVariablesPredicate(location)
}


object RESLPrecidate
{
  def assign_numbers_to_tree_nodes_rec(node: ASTNode, leaves_map: mutable.Map[(Class[_], Any), Int],
                                       v_map: mutable.Map[(Class[_], List[Int]), Int],
                                       counter: Int): Int = {
    if (node.children.isEmpty) {
      val node_tuple = node match {
        case lit_node: LiteralNode[_] => (lit_node.getClass, lit_node.value)
        case var_node: VariableNode[_] => (var_node.getClass, var_node.name)
      }
      if (!leaves_map.contains(node_tuple)) {
        node match {
          case lit_node: LiteralNode[_] =>
            leaves_map += (lit_node.getClass, lit_node.value) -> counter
            counter + 1

          case var_node: VariableNode[_] =>
            leaves_map += (var_node.getClass, var_node.name) -> counter
            counter + 1
        }
      }
      else {
        counter
      }
    }
    else {
      var current_counter = counter
      for (child <- node.children) {
        current_counter = assign_numbers_to_tree_nodes_rec(child, leaves_map, v_map, current_counter)
      }
      val children_vector = node.children.map(c => idOf_internal(c, leaves_map.toMap, v_map.toMap)).toList
      if (!v_map.contains((node.getClass, children_vector))) {
        v_map += (node.getClass, children_vector) -> current_counter
        current_counter + 1
      }
      else {
        current_counter
      }
    }
  }

  def assign_numbers_to_tree_nodes(node: ASTNode): (Map[(Class[_], Any), Int], Map[(Class[_], List[Int]), Int]) = {
    val v_map = mutable.Map[(Class[_], List[Int]), Int]()
    val leaves_map = mutable.Map[(Class[_], Any), Int]()
    var current_counter = 2
    for (child <- node.children) {
      current_counter = assign_numbers_to_tree_nodes_rec(child, leaves_map, v_map, current_counter)
    }
    v_map += (node.getClass, node.children.map(c => idOf_internal(c, leaves_map.toMap, v_map.toMap)).toList) -> 1
    (leaves_map.toMap, v_map.toMap)
  }

  def idOf_internal(node: ASTNode, leaves_map: Map[(Class[_], Any), Int],
                    v_map: Map[(Class[_], List[Int]), Int]): Int = {
    if (node.children.isEmpty) {
      val key = node match {
        case lit_node: LiteralNode[_] => (lit_node.getClass, lit_node.value)
        case var_node: VariableNode[_] => (var_node.getClass, var_node.name)
      }
      leaves_map.getOrElse(key, 0)
    }
    else {
      v_map.getOrElse((node.getClass, node.children.map(c => idOf_internal(c, leaves_map, v_map)).toList), 0)
    }
  }
}

object RetainPredicate{
  def apply (location: Int, leaves_map: Map[(Class[_], Any), Int],
  v_map: Map[(Class[_], List[Int]), Int]) = {
    new RetainPredicate(leaves_map, v_map, location)
  }

}
class RetainPredicate(val leaves_map: Map[(Class[_], Any), Int], val v_map: Map[(Class[_], List[Int]), Int],
                      val location: Int) extends Predicate
{

  def idOf(node: ASTNode): Int = {
    RESLPrecidate.idOf_internal(node, leaves_map, v_map)
  }

  override def observe(program: ASTNode): Option[Int] = {
    for (child <- program.children)
    {
      if (child.values(location) == 1)
        {
          return Some(1)
        }
    }
    Some(idOf(program))
  }

  override def check(program: ASTNode): Boolean = program.values(location) == 1

  def clonePredicate(new_location: Int): RetainPredicate = {
      new RetainPredicate(leaves_map, v_map, new_location)
  }
}

object ExcludePredicate {
  def apply(location: Int, leaves_map: Map[(Class[_], Any), Int],
            v_map: Map[(Class[_], List[Int]), Int]): ExcludePredicate = {
//    val (leaves_map, v_map) = RESLPrecidate.assign_numbers_to_tree_nodes(expression_tree)
    new ExcludePredicate(leaves_map, v_map, location)
  }

}

class ExcludePredicate(val leaves_map: Map[(Class[_], Any), Int], val v_map: Map[(Class[_], List[Int]), Int],
                      val location: Int) extends Predicate
{
  def idOf(node: ASTNode): Int = {
    RESLPrecidate.idOf_internal(node, leaves_map, v_map)
  }

  override def observe(program: ASTNode): Option[Int] = {
    for (child <- program.children)
    {
      if (child.values(location) == 1)
      {
        return Some(0)
      }
    }
    Some(idOf(program))
  }

  override def check(program: ASTNode): Boolean = program.values(location) == 0

  def clonePredicate(new_location: Int): ExcludePredicate = {
    new ExcludePredicate(leaves_map, v_map, new_location)
  }
}

