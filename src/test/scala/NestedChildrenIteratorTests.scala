import ast.Types.String
import ast.{ASTNode, IntAddition, IntLiteral, IntNode, IntVariable, PyIntLiteral, PyStringLiteral, PyStringVariable, Types}
import enumeration.{ChildrenIterator, NestedChildrenIterator, ProbChildrenIterator}
import org.junit.Test
import org.scalatestplus.junit.JUnitSuite
import org.junit.Assert._
import sygus.{ExamplePredicate, Predicates}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap}

class NestedChildrenIteratorsTests extends JUnitSuite {
  @Test def nestedIterator1(): Unit = {
    val predicates1 = Predicates(List(ExamplePredicate(Map("name" -> "Sorin Lerner"), Option("")),
      ExamplePredicate(Map("name" -> "Nadia"), Option(""))), 2)
    val predicates2 = Predicates(List(ExamplePredicate(Map("var" -> "Sorin Lerner"), Option("")),
      ExamplePredicate(Map("var" -> "Nadia"), Option(""))), 2)
    var main = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
    var mini = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
    main += (1 -> ArrayBuffer(new PyIntLiteral(0, 2, predicates1)))
    mini += (1 -> ArrayBuffer(PyStringVariable("name", predicates1),
      PyStringVariable("var", predicates2)))

    val chit = new NestedChildrenIterator(List(Types.Iterable(Types.Any)), 1,
      main, mini, predicates1)
    assertTrue(chit.hasNext)
    assertEquals(List("name"), chit.next().map(_.code))
    assertEquals(List("var"), chit.next().map(_.code))
    assertFalse(chit.hasNext)
  }

  @Test def nestedIterator2(): Unit = {
    var main = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
    var mini = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
    val predicates1 = Predicates(List(ExamplePredicate(Map("name" -> "SL"), Option("")),
      ExamplePredicate(Map("name" -> "N"), Option("")), ExamplePredicate(Map("name" -> "SB"), Option(""))), 3)
    val predicates2 = Predicates(List(ExamplePredicate(Map("var" -> "SL"), Option("")),
      ExamplePredicate(Map("var" -> "N"), Option("")), ExamplePredicate(Map("var" -> "SB"), Option(""))), 3)
    main += (1 -> ArrayBuffer(new PyStringLiteral("s", 3, predicates2),
      new PyIntLiteral(0, 3, predicates2)))
    mini += (1 -> ArrayBuffer(PyStringVariable("name", predicates1),
      PyStringVariable("var", predicates2)))
    val chit = new NestedChildrenIterator(List(Types.PyString, Types.PyString), 2,
      main, mini, predicates1)
    assertTrue(chit.hasNext)
    assertEquals(List("name", "name"), chit.next().map(_.code))
    assertEquals(List("name", "var"), chit.next().map(_.code))
    assertEquals(List("var", "name"), chit.next().map(_.code))
    assertEquals(List("var", "var"), chit.next().map(_.code))
    assertEquals(List("name", "\"s\""), chit.next().map(_.code))
    assertEquals(List("var", "\"s\""), chit.next().map(_.code))
    assertEquals(List("\"s\"", "name"), chit.next().map(_.code))
    val child = chit.next()
    assertEquals(List("\"s\"", "var"), child.map(_.code))
    assertEquals(List(List("s", "s", "s"), List("SL", "N", "SB")), child.map(_.values))

    assertFalse(chit.hasNext)
  }

  @Test def onesIterator(): Unit = {
    //Limit by height
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> 0), Option(0))), 0)
    val nodes = List(new IntLiteral(1, 1, predicates), new IntLiteral(2, 1, predicates),
      new IntLiteral(3, 1, predicates), new IntAddition(new IntVariable("x", predicates),
        new IntLiteral(1, 1, predicates), predicates))
    val chit = new ChildrenIterator(nodes, List(Types.Int), 2)
    assertTrue(chit.hasNext)
    assertEquals(List("(+ x 1)"), chit.next().map(_.code))
    assertFalse(chit.hasNext)
  }

  @Test def pairsHeightFiltered(): Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> 0), Option(0))), 0)
    val nodes = List(new IntLiteral(1, 1, predicates), new IntLiteral(2, 1, predicates),
      new IntLiteral(3, 1, predicates), new IntAddition(new IntVariable("x", predicates),
        new IntLiteral(1, 1, predicates), predicates))
    val chit = new ChildrenIterator(nodes, List(Types.Int, Types.Int), 2)
    assertTrue(chit.hasNext)
    assertEquals(List("1", "(+ x 1)"), chit.next().map(_.code))
    assertEquals(List("2", "(+ x 1)"), chit.next().map(_.code))
    assertEquals(List("3", "(+ x 1)"), chit.next().map(_.code))
    assertEquals(List("(+ x 1)", "1"), chit.next().map(_.code))
    assertEquals(List("(+ x 1)", "2"), chit.next().map(_.code))
    assertEquals(List("(+ x 1)", "3"), chit.next().map(_.code))
    assertEquals(List("(+ x 1)", "(+ x 1)"), chit.next().map(_.code))
    assertFalse(chit.hasNext)
  }

//  @Test def costRollingFourChildren: Unit = {
//    val nodes = List(
//      new IntNode {
//        override val values: List[Int] = List(0)
//        override val code: String = "0"
//        override val height: Int = 0
//        override val terms: Int = 1
//        override val children: Iterable[ASTNode] = Nil
//        override protected val parenless: Boolean = true
//
//        override val usesVariables: Boolean = false
//        override def includes(varName: String): Boolean = false
//        override def updateValues(predicates: Predicates): ASTNode = null
//        override def cost: Int = 1
//
//        override val predicates: Predicates = ???
//
//        override def computeOnContext(ctx: Map[String, Any]): Option[Any] = ???
//      }, new IntNode {
//        override val values: List[Int] = List(1)
//        override val code: String = "1"
//        override val height: Int = 0
//        override val terms: Int = 1
//        override val children: Iterable[ASTNode] = Nil
//        override protected val parenless: Boolean = true
//
//        override val usesVariables: Boolean = false
//        override def includes(varName: String): Boolean = false
//        override def updateValues(predicates: Predicates): ASTNode = null
//        override def cost: Int = 1
//
//        override val predicates: Predicates = ???
//
//        override def computeOnContext(ctx: Map[String, Any]): Option[Any] = ???
//      }, new IntNode {
//        override val values: List[Int] = List(2)
//        override val code: String = "x"
//        override val height: Int = 0
//        override val terms: Int = 1
//        override val children: Iterable[ASTNode] = Nil
//        override protected val parenless: Boolean = true
//        override def includes(varName: String): Boolean = false
//
//        override val usesVariables: Boolean = false
//        override def cost: Int = 1
//        override def updateValues(predicates: Predicates): ASTNode = null
//
//        override val predicates: Predicates = ???
//
//        override def computeOnContext(ctx: Map[String, Any]): Option[Any] = ???
//      })
//  }
}
