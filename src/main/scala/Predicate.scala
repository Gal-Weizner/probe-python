package sygus
import ast.ASTNode

import scala.reflect.internal.util.TriState.{False, True}

trait Predicate {

  def observe(program:ASTNode): Option[Any]
  def check(program: ASTNode): Any
}

class Predicates(var predicates: List[Predicate], var num_of_examples: Int)
{

  def getCurrentIterableRange(ctx: Map[String, Any], list_values: List[Iterable[_]]): (Int, Int) = {
    var start = 0
    for ((pred,l) <- getExamplePredicates().zip(list_values))
        if (pred.context == ctx) return (start, l.size)
        else start += l.size
    (-1,-1)
  }
  def getExampleValue(values: List[Any], ctx: Map[String, Any]): Any =
  {
    val res = predicates.zipWithIndex.find{case (p,_) => p.isInstanceOf[ExamplePredicate] && p.asInstanceOf[ExamplePredicate].context == ctx}
    values(res.get._2)
  }

  def getExamplePredicates(): List[ExamplePredicate] =
  {
    predicates.take(num_of_examples).asInstanceOf[List[ExamplePredicate]]
  }

  def getNonExamplePredicates(): List[Predicate] = {
    predicates.take(predicates.length - num_of_examples)
  }
//  def updateExamplePredicates(context: Map[String, Any]): List[Predicate] =
//  {
//    val example_predicates = getExamplePredicates()
//    for(pred <- example_predicates)
//    {
//      pred.asInstanceOf[ExamplePredicate].context[]
//    }
//
//  }
  def allHolds(program: ASTNode): Boolean =
  {
    val mask = predicates.map(_.check(program) == True)
    var all: Boolean = false
    if (mask.length == predicates.length)
    {
      all = true
    }
    else
      {
        all = false
      }
    all
  }
}

object Predicates {
  def apply(predicates: List[Predicate], num_of_examples: Int) = new Predicates(predicates, num_of_examples)
}
class ExamplePredicate (val context: Map[String, Any]) extends Predicate
{
  override def observe(program: ASTNode): Option[Any] = program.computeOnContext(context)

  override def check(program: ASTNode): Boolean = this.observe(program).get == this.context

  def updatePredicate(var_name: String, value: Any): ExamplePredicate =
  {
    new ExamplePredicate(this.context + (var_name -> value))
  }
}
object ExamplePredicate{
  def apply(ctx: Map[String, Any]) = new ExamplePredicate(ctx)
}
class UsesVariablesPredicate (val program: ASTNode) extends Predicate
{
  override def observe(program: ASTNode): Option[Any] = Some(program.usesVariables)

  override def check(program: ASTNode): Boolean = this.observe(program).get == true
}



