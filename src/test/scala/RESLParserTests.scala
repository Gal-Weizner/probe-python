import ast.{ASTNode, PyCount, PyIntAddition, PyIntLiteral, PyIntVariable, PyStringVariable}
import enumeration.InputsValuesManager
import org.junit.Assert._
import org.junit.Test
import org.scalatestplus.junit.JUnitSuite
import sygus._

import scala.collection.mutable

class RESLParserTests extends JUnitSuite {
  val predicates = Predicates(List(ExamplePredicate(Map("x" -> 18), Option(19), 0), UsesVariablesPredicate(1)), 1)
  val parser = new RESLParser(predicates, "int")
  val parsed = parser.parse("x + 1")

  @Test def check_number_assignment_to_tree1: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> 9, "y" -> 10), Option(19), 0), UsesVariablesPredicate(1)), 1)
    val parser = new RESLParser(predicates, "int")
    val (v_map, l_map) = parser.parse("x+y")
    val tree = PyIntAddition(PyIntVariable("x", predicates), PyIntVariable("y", predicates), predicates)
    val retain_predicate = RetainPredicate(0, l_map.toMap, v_map.toMap)
    assertEquals(retain_predicate.idOf(tree), 1)
    assertEquals(retain_predicate.idOf(tree.children.head), 2)
    assertEquals(retain_predicate.idOf(tree.children.drop(1).head), 3)
  }

  @Test def check_number_assignment_to_tree2: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> 9, "y" -> 10), Option(19), 0), UsesVariablesPredicate(1)), 1)
    val parser = new RESLParser(predicates, "int")
    val (v_map, l_map) = parser.parse("x")
    assertEquals(l_map.size, 1)
    assertEquals(v_map.size, 0)
    val tree = PyIntVariable("x", predicates)
    val retain_predicate = RetainPredicate(0, l_map.toMap, v_map.toMap)
    assertEquals(retain_predicate.idOf(tree), 1)
  }

  @Test def check_number_assignment_to_tree3: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> 9, "y" -> 10), Option(19), 0), UsesVariablesPredicate(1)), 1)
    val parser = new RESLParser(predicates, "int")
    val (v_map, l_map) = parser.parse("9")
    assertEquals(l_map.size, 1)
    assertEquals(v_map.size, 0)
    val tree = PyIntLiteral(9, 1, predicates)
    val retain_predicate = RetainPredicate(0, l_map.toMap, v_map.toMap)
    assertEquals(retain_predicate.idOf(tree), 1)
  }

  @Test def check_var_parsing: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("inp" -> "aabbcde", "var"->"a"),
      Option(Map('a'-> 2, 'b'-> 2, 'c'-> 1, 'd'-> 1, 'e'-> 1)), 0), UsesVariablesPredicate(1)), 1)
    val exp_parser = new ExpressionParser(predicates)
    val tree = exp_parser.parse("inp.count(var)")
    val resl_parser = new RESLParser(predicates, "String")
    val (v_map, l_map) = resl_parser.parse("inp.count(var)")
    val retain_predicate = RetainPredicate(0, l_map.toMap, v_map.toMap)
    assert(l_map.contains(("ast.PyStringVariable","var")))
    assertEquals(retain_predicate.idOf(tree), 1)
  }
}

