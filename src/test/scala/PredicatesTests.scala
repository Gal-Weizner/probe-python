import ast.{PyIntLiteral, PyStringVariable}
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test
import sygus.{ExamplePredicate, Predicates, UsesVariablesPredicate}

class PredicatesTests {
  @Test def basic_predicates: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("inp1" -> "Welcome to USA. usa usa usa usa!",
      "inp2" -> "USA"), Option(5)), UsesVariablesPredicate()), 1)
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

    val variable1 =  new PyStringVariable("inp1", predicates)
    assertEquals(predicates.getExamplePredicates().head.observe(variable1).get,
      "Welcome to USA. usa usa usa usa!")
    assertFalse(predicates.getExamplePredicates().head.check(variable1))
    assertFalse(predicates.someHolds(variable1))
    assertFalse(predicates.allHolds(variable1))

    assertEquals(predicates.getExamplePredicates().length, 1)
    assertEquals(predicates.getNonExamplePredicates().length, 1)
  }

  @Test def more_advanced_predicates: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("inp1" -> 9), Option(3)), ExamplePredicate(Map("inp1" -> 6), Option(2)),
      ExamplePredicate(Map("inp1" -> 10), Option(3)), UsesVariablesPredicate()), 3)
    val literal1: PyIntLiteral = new PyIntLiteral(42, 1, predicates)
    assertEquals(literal1.values.length, 4)
    val variable1 =  new PyStringVariable("inp1", predicates)
    assertEquals(predicates.getExampleValue(variable1.values, Map("inp1"-> 6)), 6)
    assert(predicates.getExamplePredicates().head.updatePredicate("var", "").context.contains("var"))

  }

}
