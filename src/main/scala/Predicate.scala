package sygus
import ast.{ASTNode, LiteralNode, Types}

import scala.collection.mutable
import scala.reflect.internal.util.TriState.{False, True}

trait Predicate {

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
class ExamplePredicate (val context: Map[String, Any], val output: Option[Any]) extends Predicate
{
  override def observe(program: ASTNode): Option[Any] = program.computeOnContext(context)

  override def check(program: ASTNode): Boolean = program.predicates.getExampleValue(program.values, this.context) == this.output.get

  def updatePredicate(var_name: String, value: Any): ExamplePredicate =
  {
    new ExamplePredicate(this.context + (var_name -> value), None)
  }
}
object ExamplePredicate{
  def apply(ctx: Map[String, Any], output: Option[Any]) = new ExamplePredicate(ctx, output)
}
class UsesVariablesPredicate() extends Predicate
{
  override def observe(program: ASTNode): Option[Any] = Some(program.usesVariables)

  override def check(program: ASTNode): Boolean = program.values.last.asInstanceOf[Boolean]
}

object UsesVariablesPredicate{
  def apply() = new UsesVariablesPredicate()
}

class RetainPredicate(expression_tree: ASTNode) extends Predicate
{
  val (leaves_map, v_map) = assign_numbers_to_tree_nodes(expression_tree)

  def assign_numbers_to_tree_nodes_rec(node: ASTNode, leaves_map: mutable.Map[ASTNode, Int],
                                       v_map: mutable.Map[(Class[_], List[Int]), Int],
                                       counter: Int): Int = {
    if (node.children.isEmpty) {
      if(!leaves_map.contains(node))
        {
          leaves_map += node -> counter
          counter + 1
        }
      else
        {
          counter
        }
    }
    else {
      var current_counter = counter
      for(child <- node.children){
        current_counter = assign_numbers_to_tree_nodes_rec(child, leaves_map, v_map, current_counter)
      }
      val children_vector = node.children.map(c=>idOf_internal(c, leaves_map.toMap, v_map.toMap)).toList
      if(!v_map.contains((node.getClass, children_vector))){
        v_map += (node.getClass, children_vector) -> current_counter
        current_counter + 1
      }
      else
        {
          current_counter
        }
    }
  }

  def assign_numbers_to_tree_nodes(node: ASTNode): (Map[ASTNode, Int], Map[(Class[_], List[Int]), Int]) = {
    val v_map = mutable.Map[(Class[_], List[Int]), Int]()
    val leaves_map = mutable.Map[ASTNode, Int]()
    var current_counter = 2
    for (child <- node.children) {
      current_counter = assign_numbers_to_tree_nodes_rec(child, leaves_map, v_map, current_counter)
    }
    v_map += (node.getClass, node.children.map(c=>idOf_internal(c, leaves_map.toMap, v_map.toMap)).toList) -> 1
    (leaves_map.toMap, v_map.toMap)
  }

  def idOf(node: ASTNode): Int = {
    idOf_internal(node, leaves_map, v_map)
  }

  def idOf_internal(node: ASTNode, leaves_map:Map[ASTNode, Int],
                    v_map: Map[(Class[_], List[Int]), Int]): Int = {
    if (node.children.isEmpty)
    {
      leaves_map(node)
    }
    else
      {
        v_map((node.getClass, node.children.map(c=>idOf_internal(c, leaves_map, v_map)).toList))
      }
  }

  override def observe(program: ASTNode): Option[Int] = ???

  override def check(program: ASTNode): Boolean = program.values.last.asInstanceOf[Boolean]
}


