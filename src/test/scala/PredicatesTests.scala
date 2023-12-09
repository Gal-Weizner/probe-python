import ast.{ASTNode, PyCount, PyIntLiteral, PyStringConcat, PyStringLower, PyStringNode, PyStringUpper, PyStringVariable, Types}
import enumeration.InputsValuesManager
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test
import sygus.{ExamplePredicate, ExpressionParser, Predicates, PythonPBETask, RESLParser, RetainPredicate, UsesVariablesPredicate}

import scala.collection.mutable

class PredicatesTests {
  @Test def basic_predicates: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("inp1" -> "Welcome to USA. usa usa usa usa!",
      "inp2" -> "USA"), Option(5),0), UsesVariablesPredicate(1)), 1)
    val literal1: PyIntLiteral = new PyIntLiteral(42, 1, predicates)
    assertEquals(predicates.getExamplePredicates().head.observe(literal1).get, 42)
    assertFalse(predicates.getExamplePredicates().head.check(literal1))
    assertFalse(predicates.allHolds(literal1))
    assertFalse(predicates.someHolds(literal1))
    assertFalse(predicates.predicates.last.check(literal1))

    val literal2: PyIntLiteral = new PyIntLiteral(5, 1, predicates)
    assertTrue(predicates.getExamplePredicates().head.check(literal2))
    assertTrue(predicates.someHolds(literal2))
    assertFalse(predicates.allHolds(literal2))

    val variable1 = new PyStringVariable("inp1", predicates)
    assertEquals(predicates.getExamplePredicates().head.observe(variable1).get,
      "Welcome to USA. usa usa usa usa!")
    assertFalse(predicates.getExamplePredicates().head.check(variable1))
    assertFalse(predicates.someHolds(variable1))
    assertFalse(predicates.allHolds(variable1))

    assertEquals(predicates.getExamplePredicates().length, 1)
    assertEquals(predicates.getNonExamplePredicates().length, 1)
  }

  @Test def more_advanced_predicates: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("inp1" -> 9), Option(3),0),
      ExamplePredicate(Map("inp1" -> 6), Option(2),1),
      ExamplePredicate(Map("inp1" -> 10), Option(3),2), UsesVariablesPredicate(3)), 3)
    val literal1: PyIntLiteral = new PyIntLiteral(42, 1, predicates)
    assertEquals(literal1.values.length, 4)
    val variable1 = new PyStringVariable("inp1", predicates)
    assertEquals(predicates.getExampleValue(variable1.values, Map("inp1" -> 6)), 6)
    assert(predicates.getExamplePredicates().head.updatePredicate("var", "",0).context.contains("var"))

  }

  @Test def check_number_assignment_to_tree1: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> 9, "y" -> 10), Option(19),0), UsesVariablesPredicate(1)), 1)
    val resl_parser = new RESLParser(predicates, "int")
    val exp_parser = new ExpressionParser(predicates)
    val tree = exp_parser.parse("x+y")
    val (v_map, l_map) = resl_parser.parse("x+y")
    val retain_predicate = RetainPredicate(0, l_map.toMap, v_map.toMap)
    assertEquals(retain_predicate.idOf(tree), 1)
    assertEquals(retain_predicate.idOf(tree.children.head), 2)
    assertEquals(retain_predicate.idOf(tree.children.drop(1).head), 3)
  }

  @Test def check_number_assignment_to_tree2: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> 9, "y" -> 10), Option(21),0), UsesVariablesPredicate(1)), 1)
    val exp_parser = new ExpressionParser(predicates)
    val resl_parser = new RESLParser(predicates, "int")
    val tree = exp_parser.parse("x+y+2")
    val (v_map, l_map) = resl_parser.parse("x+y+2")

    val retain_predicate = RetainPredicate(0, l_map.toMap, v_map.toMap)
    assertEquals(retain_predicate.idOf(tree), 1)
    assertEquals(retain_predicate.idOf(tree.children.head), 2)
    assertEquals(retain_predicate.idOf(tree.children.drop(1).head), 5)
    assertEquals(retain_predicate.idOf(tree.children.drop(1).head.children.head), 3)
    assertEquals(retain_predicate.idOf(tree.children.drop(1).head.children.drop(1).head), 4)
  }

  @Test def check_number_assignment_to_tree3: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> 9, "y" -> 10), Option(18),0),
      UsesVariablesPredicate(1)), 1)
    val exp_parser = new ExpressionParser(predicates)
    val resl_parser = new RESLParser(predicates, "int")
    val tree = exp_parser.parse("x+x")
    val (v_map, l_map) = resl_parser.parse("x+x")
    val retain_predicate = RetainPredicate(0, l_map.toMap, v_map.toMap)
    assertEquals(retain_predicate.idOf(tree), 1)
    assertEquals(retain_predicate.idOf(tree.children.head), 2)
    assertEquals(retain_predicate.idOf(tree.children.drop(1).head), 2)
  }

  @Test def check_number_assignment_to_tree4: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> "9", "y" -> 10), Option(18),0),
      UsesVariablesPredicate(1)), 1)
    val exp_parser = new ExpressionParser(predicates)
    val resl_parser = new RESLParser(predicates, "string")
    val tree = exp_parser.parse("x+x.upper()")
    val (v_map, l_map) = resl_parser.parse("x+x.upper()")
    val retain_predicate = RetainPredicate(0, l_map.toMap, v_map.toMap)
    assertEquals(retain_predicate.idOf(tree), 1)
    assertEquals(retain_predicate.idOf(tree.children.head), 2)
    assertEquals(retain_predicate.idOf(tree.children.drop(1).head), 3)
    assertEquals(retain_predicate.idOf(tree.children.drop(1).head.children.head), 2)
  }

  @Test def check_number_assignment_to_tree5: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> "9", "y" -> 10), Option(18),0),
      UsesVariablesPredicate(1)), 1)
    val exp_parser = new ExpressionParser(predicates)
    val resl_parser = new RESLParser(predicates, "string")
    val tree = exp_parser.parse("x.upper()+x.upper()")
    val (v_map, l_map) = resl_parser.parse("x.upper()+x.upper()")
    val retain_predicate = RetainPredicate(0, l_map.toMap, v_map.toMap)
    assertEquals(retain_predicate.idOf(tree), 1)
    assertEquals(retain_predicate.idOf(tree.children.head), 3)
    assertEquals(retain_predicate.idOf(tree.children.head.children.head), 2)
    assertEquals(retain_predicate.idOf(tree.children.drop(1).head), 3)
    assertEquals(retain_predicate.idOf(tree.children.drop(1).head.children.head), 2)
  }


  @Test def check_retain_observe1: Unit = {
    val examples_list = List(ExamplePredicate(Map("x" -> "a"), Option("A"), 0))
    val predicates = Predicates(examples_list, 1)
    val exp_parser = new ExpressionParser(predicates)
    val resl_parser = new RESLParser(predicates, "string")
    val tree = exp_parser.parse("x.upper()")
    val (v_map, l_map) = resl_parser.parse("x.upper()")
    val retain_predicate = RetainPredicate(1, l_map.toMap, v_map.toMap)
    val new_predicates = Predicates(List(examples_list.head, retain_predicate, UsesVariablesPredicate(1)), 1)
    val program = PyStringUpper(PyStringVariable("x", new_predicates), new_predicates)
    assertEquals(retain_predicate.observe(program).get,Some(1).get)
  }
  @Test def check_retain_observe: Unit = {
    val examples_list = List(ExamplePredicate(Map("x" -> "a"), Option("AA"), 0))
    val predicates = Predicates(examples_list, 1)
    val resl_parser = new RESLParser(predicates, "string")
    val (v_map, l_map) = resl_parser.parse("x.upper()+x.upper()")
    val retain_predicate = RetainPredicate(1, l_map.toMap, v_map.toMap)
    val new_predicates = Predicates(List(examples_list.head, retain_predicate, UsesVariablesPredicate(1)), 1)
    val program = PyStringUpper(PyStringVariable("x", new_predicates), new_predicates)
    assertEquals(retain_predicate.observe(program).get,3)
  }

  @Test def check_retain_observe2: Unit = {
    val examples_list = List(ExamplePredicate(Map("x" -> "a"), Option("AA"), 0))
    val predicates = Predicates(examples_list, 1)
    val resl_parser = new RESLParser(predicates, "string")
    val (v_map, l_map) = resl_parser.parse("x.upper()+x.upper()")
    val retain_predicate = RetainPredicate(1, l_map.toMap, v_map.toMap)
    val new_predicates = Predicates(List(examples_list.head, retain_predicate, UsesVariablesPredicate(1)), 1)
    val program = PyStringLower(PyStringVariable("x", new_predicates), new_predicates)
    assertEquals(retain_predicate.observe(program).get, 0)
  }

  @Test def check_retain_observe3: Unit = {
    val examples_list = List(ExamplePredicate(Map("x" -> "a"), Option("AA"), 0))
    val predicates = Predicates(examples_list, 1)
    val resl_parser = new RESLParser(predicates, "string")
    val (v_map, l_map) = resl_parser.parse("x.upper()")
    val retain_predicate = RetainPredicate(1, l_map.toMap, v_map.toMap)
    val new_predicates = Predicates(List(examples_list.head, retain_predicate, UsesVariablesPredicate(1)), 1)
    val program = PyStringConcat(PyStringUpper(PyStringVariable("x", new_predicates), new_predicates),
      PyStringUpper(PyStringVariable("x", new_predicates), new_predicates), new_predicates)
    assertEquals(retain_predicate.observe(program).get, 1)
  }

  @Test def check_retain_observe4: Unit = {
    val examples_list = List(ExamplePredicate(Map("x" -> "A a A", "y"-> "a"), Option(3), 0))
    val predicates = Predicates(examples_list, 1)
    val resl_parser = new RESLParser(predicates, "string")
    val (v_map, l_map) = resl_parser.parse("y.upper()")
    val retain_predicate = RetainPredicate(1, l_map.toMap, v_map.toMap)
    val new_predicates = Predicates(List(examples_list.head, retain_predicate, UsesVariablesPredicate(1)), 1)
    val program = PyCount(PyStringUpper(PyStringVariable("x", new_predicates), new_predicates),
      PyStringUpper(PyStringVariable("y", new_predicates), new_predicates), new_predicates)
    assertEquals(retain_predicate.observe(program).get, 1)
  }

 @Test def check_retain_observe_from_task: Unit = {
      val task = PythonPBETask.fromString(
        """{
          |  "varName": "rs",
          |  "env": [
          |    {
          |      "#": "",
          |      "$": "",
          |      "inp1": "'Welcome to USA. usa usa usa usa!'",
          |      "inp2": "'USA'",
          |      "rs": "5"
          |    }
          |  ],
          |  "retain": [{
          |    "expression":"inp1.upper()",
          |    "type":"String"
          |  }]
          |}""".stripMargin, true)

   val oeManager = new InputsValuesManager()
   val bank = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
   val enumerator = new enumeration.PyProbEnumerator(task.vocab, oeManager, task.predicates, false, 0, bank,
     bank)
    enumerator.next()
     assertEquals(enumerator.next().values(1), 0)
 }
}

