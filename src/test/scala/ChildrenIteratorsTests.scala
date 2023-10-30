import ast.{ASTNode, IntAddition, IntLiteral, IntNode, IntVariable, Types}
import enumeration.{ChildrenIterator, ProbChildrenIterator}
import org.junit.Test
import org.scalatestplus.junit.JUnitSuite
import org.junit.Assert._
import sygus.{ExamplePredicate, Predicates}


class ChildrenIteratorsTests extends JUnitSuite {
    @Test def pairsIterator(): Unit = {
                  val predicates = Predicates(List(ExamplePredicate(Map("x" -> 0), Option(0), 0)), 1)
                  val nodes = List(new IntLiteral(1, 1, predicates),
                    new IntLiteral(2, 1, predicates), new IntLiteral(3, 1, predicates))
                  val chit = new ChildrenIterator(nodes, List(Types.Int, Types.Int), 1)
                  assertTrue(chit.hasNext)
                  assertEquals(List("1", "1"), chit.next().map(_.code))
                  assertEquals(List("1", "2"), chit.next().map(_.code))
                  assertEquals(List("1", "3"), chit.next().map(_.code))
                  assertEquals(List("2", "1"), chit.next().map(_.code))
                  assertEquals(List("2", "2"), chit.next().map(_.code))
                  assertEquals(List("2", "3"), chit.next().map(_.code))
                  assertEquals(List("3", "1"), chit.next().map(_.code))
                  assertEquals(List("3", "2"), chit.next().map(_.code))
                  assertEquals(List("3", "3"), chit.next().map(_.code))
                  assertFalse(chit.hasNext)
                  }

     @Test def onesIterator(): Unit = {
                  //Limit by height
       val predicates = Predicates(List(ExamplePredicate(Map("x" -> 0), Option(0), 0)), 0)
       val nodes = List(new IntLiteral(1, 1, predicates), new IntLiteral(2, 1, predicates),
         new IntLiteral(3, 1, predicates), new IntAddition(new IntVariable("x", predicates),
           new IntLiteral(1, 1, predicates), predicates))
                  val chit = new ChildrenIterator(nodes, List(Types.Int), 2)
                  assertTrue(chit.hasNext)
                  assertEquals(List("(+ x 1)"), chit.next().map(_.code))
                  assertFalse(chit.hasNext)
                  }

     @Test def pairsHeightFiltered(): Unit = {
                  val predicates = Predicates(List(ExamplePredicate(Map("x" -> 0), Option(0), 0 )), 0)
                  val nodes = List(new IntLiteral(1, 1, predicates), new IntLiteral(2, 1, predicates),
                    new IntLiteral(3, 1, predicates), new IntAddition(new IntVariable("x",predicates),
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
/////TODO: What is the point of this?
//
//     @Test def costRollingFourChildren: Unit = {
//                  val nodes = List(
//                  new IntNode {
//                  override val values: List[Int] = List(0)
//                  override val code: String = "0"
//                  override val height: Int = 0
//                  override val terms: Int = 1
//                  override val children: Iterable[ASTNode] = Nil
//                  override protected val parenless: Boolean = true
//
//                   override val usesVariables: Boolean = false
//                    override def includes(varName: String): Boolean = false
//                    def updateValues(predicates: Predicates): ASTNode = null
//                  override def cost: Int = 1
//
//                    override val predicates: Predicates = predicates
//
//                    override def computeOnContext(ctx: Map[String, Any]): Option[Any] = ???
//                  }, new IntNode {
//                  override val values: List[Int] = List(1)
//                  override val code: String = "1"
//                  override val height: Int = 0
//                  override val terms: Int = 1
//                  override val children: Iterable[ASTNode] = Nil
//                  override protected val parenless: Boolean = true
//
//                  override val usesVariables: Boolean = false
//                  override def includes(varName: String): Boolean = false
//                  override def updateValues(predicates: Predicates): ASTNode = null
//                  override def cost: Int = 1
//
//                      override val predicates: Predicates = predicates
//
//                      override def computeOnContext(ctx: Map[String, Any]): Option[Any] = ???
//                    }, new IntNode {
//                  override val values: List[Int] = List(2)
//                  override val code: String = "x"
//                  override val height: Int = 0
//                  override val terms: Int = 1
//                  override val children: Iterable[ASTNode] = Nil
//                  override protected val parenless: Boolean = true
//                  override def includes(varName: String): Boolean = false
//
//                  override val usesVariables: Boolean = false
//                  override def cost: Int = 1
//                  override def updateValues(predicates: Predicates): ASTNode = null
//
//                  override val predicates: Predicates = predicates
//
//                  override def computeOnContext(ctx: Map[String, Any]): Option[Any] = ???
//                    })
//     }
}
