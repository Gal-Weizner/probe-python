package sygus
import ast.ASTNode

import scala.reflect.internal.util.TriState.{False, True}

trait Predicate {

  def observe(program:ASTNode): Option[Any]
  def check(program: ASTNode): Boolean
}

class Predicates(var predicates: List[Predicate], var num_of_examples: Int) {

  def getCurrentIterableRange(ctx: Map[String, Any], list_values: List[Iterable[_]]): (Int, Int) = {
    var start = 0
    for ((pred, l) <- this.getExamplePredicates().zip(list_values))
      if (pred.context == ctx) return (start, l.size)
      else start += l.size
    (-1, -1)
  }

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
    predicates.exists(pred => pred.check(program))
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



