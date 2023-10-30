import ast._
import org.junit.Test
import org.junit.Assert._
import org.scalatestplus.junit.JUnitSuite
import sygus.{ExamplePredicate, PostProcessor, Predicates}

class PostprocessorTests  extends JUnitSuite{
  @Test def constantFoldIntOperation: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("inp"-> ""), Option(""),0)), 1)
    val expr = new PyIntAddition(
      new PyIntSubtraction(
        new PyIntDivision(new PyIntLiteral(2, 1, predicates),
          new PyIntLiteral(3, 1, predicates), predicates),
        new PyIntLiteral(-1, 1, predicates), predicates),
      new PyIntLiteral(8, 1, predicates), predicates)
    val postProcessed = PostProcessor.clean(expr)
    assertEquals("9",postProcessed.code)
  }
  @Test def constantFoldIntOperationOneVar1: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> 2), Option(""),0)), 1)
    val x = new PyIntVariable("x",predicates)
    val expr = new PyIntAddition(
      new PyIntSubtraction(
        new PyIntDivision(x, new PyIntLiteral(3, 1, predicates), predicates),
        new PyIntLiteral(-1, 1, predicates), predicates),
      new PyIntLiteral(8, 1, predicates), predicates)
    val postProcessed = PostProcessor.clean(expr)
    assertEquals("x // 3 - -1 + 8",postProcessed.code)
  }
  @Test def constantFoldIntOperationOneVar2: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> 2), Option(""),0)), 1)
    val x = new PyIntVariable("x",predicates)
    val expr = new PyIntAddition(
      new PyIntSubtraction(
        new PyIntDivision(new PyIntLiteral(2, 1, predicates), x, predicates),
        new PyIntLiteral(-1, 1, predicates), predicates),
      new PyIntLiteral(8, 1, predicates), predicates)
    val postProcessed = PostProcessor.clean(expr)
    assertEquals("2 // x - -1 + 8",postProcessed.code)
  }
  @Test def constantFoldIntOperationOneVar3: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> 2), Option(""),0)), 1)
    val x = new PyIntVariable("x", predicates)
    val expr = new PyIntAddition(
      new PyIntSubtraction(
        new PyIntDivision(new PyIntLiteral(2, 1, predicates),
          new PyIntLiteral(3, 1, predicates), predicates),x, predicates),
      new PyIntLiteral(8, 1, predicates),predicates)
    val postProcessed = PostProcessor.clean(expr)
    assertEquals("0 - x + 8",postProcessed.code)
  }
  @Test def constantFoldIntOperationOneVar4: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> 2), Option(""),0)), 1)
    val x = new PyIntVariable("x",predicates)
    val expr = new PyIntAddition(
      new PyIntSubtraction(
        new PyIntDivision(new PyIntLiteral(2, 1, predicates),
          new PyIntLiteral(3, 1, predicates), predicates),
        new PyIntLiteral(-1, 1, predicates), predicates), x, predicates)
    val postProcessed = PostProcessor.clean(expr)
    assertEquals("1 + x",postProcessed.code)
  }
  @Test def constantFoldStringToInt: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> 2), Option(""),0)), 1)
    val expr = new PyIntAddition(
      new PyIntSubtraction(
        new PyIntDivision(new PyIntLiteral(2, 1, predicates),
          new PyIntLiteral(3, 1, predicates), predicates),
        new PyIntLiteral(-1, 1, predicates), predicates),
      new PyStringToInt(new PyStringLiteral("8",1, predicates), predicates), predicates)
    val postProcessed = PostProcessor.clean(expr)
    assertEquals("9",postProcessed.code)
  }
  @Test def constantFoldingStrings1: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> 2), Option(""),0)), 1)
    val expr = new PyIntToString(new PyIntLiteral(-8,1, predicates), predicates)
    assertEquals("\"-8\"",PostProcessor.clean(expr).code)
  }
  @Test def constantFoldingStrings1WithVar: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> 8), Option(""),0)), 1)
    val x = new PyIntVariable("x",predicates)
    val expr = new PyIntToString(x, predicates)
    assertEquals("str(x)",PostProcessor.clean(expr).code)
  }

  @Test def constantFoldingStrings2: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> 8), Option(""),0)), 1)
    val expr = new PyFind(new PyStringLiteral("",1, predicates),
      new PyStringLiteral(" ",1, predicates), predicates)
    assertEquals("-1",PostProcessor.clean(expr).code)
  }
  @Test def constantFoldingStrings2Var: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> "8"), Option(""),0)), 1)
    val x = new PyStringVariable("x", predicates)
    val expr = new PyFind(x,new PyStringLiteral(" ",1, predicates), predicates)
    assertEquals("x.find(\" \")",PostProcessor.clean(expr).code)
    val expr2 = new PyFind(new PyStringLiteral("",1, predicates),x, predicates)
    assertEquals("\"\".find(x)",PostProcessor.clean(expr2).code)
  }
  @Test def constantFoldingStrings3: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> ""), Option(""),0)), 1)
    val expr = new PyStringConcat(
      new PyBinarySubstring(
        new PyStringLiteral("abc",1, predicates),
        new PyIntLiteral(1,1, predicates), predicates),
      new TernarySubstring(
        new PyStringLiteral("abcde",1, predicates),
        new PyIntLiteral(1,1, predicates),
        new PyIntSubtraction(new PyLength(new PyStringLiteral("abcde",1, predicates), predicates),
          new PyIntLiteral(2,1, predicates), predicates), predicates), predicates)
    assertEquals("\"bbc\"",PostProcessor.clean(expr).code)
  }
  @Test def constantFoldingBoolean1: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> ""), Option(""),0)), 1)
    val expr = new PyLessThanEq(new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(2,1, predicates), predicates)
    assertEquals("True",PostProcessor.clean(expr).code)
    val expr2 = new PyGreaterThan(new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(2,1, predicates), predicates)
    assertEquals("False",PostProcessor.clean(expr2).code)
  }

  @Test def constantFoldingBoolean1WithVar: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("n" -> 2), Option(""),0)), 1)
    val n = new PyIntVariable("n", predicates)
    val expr = new PyLessThanEq(n, new PyIntLiteral(2,1, predicates), predicates)
    assertEquals("n <= 2",PostProcessor.clean(expr).code)
    val expr2 = new PyGreaterThan(new PyIntLiteral(1,1, predicates), n, predicates)
    assertEquals("1 > n",PostProcessor.clean(expr2).code)
  }

  @Test def constantFoldingBoolean2: Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> ""), Option(""),0)), 1)
    val expr = new PyContains(new PyStringLiteral("abc",1, predicates),
      new PyStringLiteral("",1, predicates), predicates)
    assertEquals("False",PostProcessor.clean(expr).code)
  }
}

