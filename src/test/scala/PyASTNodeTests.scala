import ast._
import org.junit.Test
import org.scalatestplus.junit.JUnitSuite
import org.junit.Assert._
import sygus.{ExamplePredicate, Predicates}

class PyASTNodeTests extends JUnitSuite {

  @Test def stringLiteralNode(): Unit = {
    val input = Map("inp1" -> "'Welcome to USA. usa usa usa usa!'", "inp2" -> "'USA'")
    val output = Option(5)
    val example_predicate = ExamplePredicate(input, output)
    val predicates = Predicates(List(example_predicate), 1)
    val literal: PyStringLiteral = new PyStringLiteral("abc", 1, predicates)
    assertEquals(1, literal.values.length)
    assertEquals("abc", literal.values.head)
    assertEquals(Types.PyString, literal.nodeType)
    assertEquals("\"abc\"", literal.code)
    assertEquals(0, literal.height)
    assertEquals(1, literal.terms)
    assertTrue(literal.children.isEmpty)
  }

  @Test def stringLiteralEscaping(): Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("inp"->"\"a\\tb\\r\\n\""), Option("\"a\\tb\\r\\n\""))), 1)
    assertEquals("\"a\\tb\\r\\n\"", new PyStringLiteral("a\tb\r\n", 1, predicates).code)
    assertEquals("\"a\\\\tb\\\\r\\\\n\"", new PyStringLiteral("a\\tb\\r\\n", 1, predicates).code)
    assertEquals("\"a\\\"b\\\"c\"", new PyStringLiteral("a\"b\"c", 1, predicates).code)
    assertEquals("\"\\xd83d\\xdca9\"", new PyStringLiteral("\uD83D\uDCA9", 1, predicates).code)
  }

  @Test def intLiteralNode(): Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("inp"-> 42), Option(42))), 1)
    val literal: PyIntLiteral = new PyIntLiteral(42, 1, predicates)
    assertEquals(1, literal.values.length)
    assertEquals(42, literal.values.head)
    assertEquals(Types.PyInt, literal.nodeType)
    assertEquals("42", literal.code)
    assertEquals(0, literal.height)
    assertEquals(1, literal.terms)
    assertTrue(literal.children.isEmpty)
  }

  @Test def boolLiteralNode(): Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("inp"-> false), Option(false))), 1)
    var literal: PyBoolLiteral = new PyBoolLiteral(false, 1, predicates)
    assertEquals(1, literal.values.length)
    assertEquals(false, literal.values.head)
    assertEquals(Types.PyBool, literal.nodeType)
    assertEquals("False", literal.code)
    assertEquals(0, literal.height)
    assertEquals(1, literal.terms)
    assertTrue(literal.children.isEmpty)

    literal = new PyBoolLiteral(true, 1, predicates)
    assertEquals(1, literal.values.length)
    assertEquals(true, literal.values.head)
    assertEquals(Types.PyBool, literal.nodeType)
    assertEquals("True", literal.code)
    assertEquals(0, literal.height)
    assertEquals(1, literal.terms)
    assertTrue(literal.children.isEmpty)
  }

  @Test def intToStringNode(): Unit = {
    val example = ExamplePredicate(Map("inp" -> 83), Option("83"))
    val predicates = Predicates(List(example), 1)
    val node: PyIntToString = new PyIntToString(new PyIntLiteral(83, 1, predicates), predicates)
    assertEquals(1, node.values.length)
    assertEquals("83", node.values.head)
    assertEquals(Types.PyString, node.nodeType)
    assertEquals("str(83)", node.code)
    assertEquals(1, node.height)
    assertEquals(2, node.terms)
    assertEquals(node.children.size, 1)
  }

  @Test def stringToIntNode(): Unit = {
    val example = ExamplePredicate(Map("inp" -> "83"), Option(83))
    val predicates = Predicates(List(example), 1)
    val node: PyStringToInt = new PyStringToInt(new PyStringLiteral("83", 1, predicates), predicates)
    assertEquals(1, node.values.length)
    assertEquals(83, node.values.head)
    assertEquals(Types.PyInt, node.nodeType)
    assertEquals("int(\"83\")", node.code)
    assertEquals(1, node.height)
    assertEquals(2, node.terms)
    assertEquals(node.children.size, 1)
  }

  @Test def stringLowerNode(): Unit = {
    val example1 = ExamplePredicate(Map("inp" -> "aBC"), Option("abc"))
    val predicates1 = Predicates(List(example1), 1)
    var node: PyStringLower = new PyStringLower(new PyStringLiteral("aBC", 1, predicates1), predicates1)
    assertEquals(1, node.values.length)
    assertEquals("abc", node.values.head)
    assertEquals(Types.PyString, node.nodeType)
    assertEquals("\"aBC\".lower()", node.code)
    assertEquals(1, node.height)
    assertEquals(2, node.terms)
    assertEquals(node.children.size, 1)

    val example2 = ExamplePredicate(Map("inp1" -> "aBC", "inp2"->"deF"), Option("abcdef"))
    val predicates2 = Predicates(List(example2), 1)
    node = new PyStringLower(new PyStringConcat(
      new PyStringLiteral("aBC", 1, predicates2),
      new PyStringLiteral("deF", 1, predicates2), predicates2), predicates2)
    assertEquals(1, node.values.length)
    assertEquals("abcdef", node.values.head)
    assertEquals(Types.PyString, node.nodeType)
    assertEquals("(\"aBC\" + \"deF\").lower()", node.code)
    assertEquals(2, node.height)
    assertEquals(4, node.terms)
    assertEquals(node.children.size, 1)
  }

  @Test def intMultiplication(): Unit = {
    val example = ExamplePredicate(Map("inp1" -> 1, "inp2" -> 2, "inp3"->3, "inp4"->4), Option("1 * 2 * 3 * 4"))
    val predicates = Predicates(List(example), 1)
    val multiplyNumbers = new PyIntMultiply(new
        PyIntMultiply(new PyIntLiteral(1, 1, predicates),
          new PyIntLiteral(2, 1, predicates), predicates),
      new PyIntMultiply(new PyIntLiteral(3, 1, predicates),
        new PyIntLiteral(4, 1, predicates), predicates), predicates)
    assertEquals("1 * 2 * 3 * 4", multiplyNumbers.code)
    assertEquals(24, multiplyNumbers.values.head)
  }

  @Test def stringMultiplication(): Unit = {
    val example = ExamplePredicate(Map("inp" -> "a"), Option("aaa"))
    val predicates = Predicates(List(example), 1)
    val multiply = new PyStringMultiply(
      new PyStringLiteral("a", 1, predicates),
      new PyIntLiteral(3, 1, predicates), predicates)
    assertEquals("\"a\" * 3", multiply.code)
    assertEquals("aaa", multiply.values.head)
  }

  @Test def alpha(): Unit = {
    val example1 = ExamplePredicate(Map("inp" -> "abc"), Option(true))
    val example2 = ExamplePredicate(Map("inp" -> "a123"), Option(false))
    val example3 = ExamplePredicate(Map("inp" -> "a "), Option(false))
    val example4 = ExamplePredicate(Map("inp" -> "a%*"), Option(false))

    val isAlpha1 = new PyIsAlpha(new PyStringLiteral("abc", 1, Predicates(List(example1), 1)), Predicates(List(example1), 1))
    val isAlpha2 = new PyIsAlpha(new PyStringLiteral("a123", 1, Predicates(List(example2), 1)), Predicates(List(example2), 1))
    val isAlpha3 = new PyIsAlpha(new PyStringLiteral("a ", 1, Predicates(List(example3), 1)), Predicates(List(example3), 1))
    val isAlpha4 = new PyIsAlpha(new PyStringLiteral("a%*", 1, Predicates(List(example4), 1)), Predicates(List(example4), 1))
    assertEquals(true, isAlpha1.values.head)
    assertEquals(false, isAlpha2.values.head)
    assertEquals(false, isAlpha3.values.head)
    assertEquals(false, isAlpha4.values.head)
  }

  @Test def numeric(): Unit = {
    val example1 = ExamplePredicate(Map("inp" -> "abc123"), Option(false))
    val example2 = ExamplePredicate(Map("inp" -> "123"), Option(true))
    val example3 = ExamplePredicate(Map("inp" -> "123 "), Option(false))
    val example4 = ExamplePredicate(Map("inp" -> "123%*"), Option(false))
    val isNumeric1 = new PyIsNumeric(new PyStringLiteral("abc123", 1, Predicates(List(example1), 1)), Predicates(List(example1), 1))
    val isNumeric2 = new PyIsNumeric(new PyStringLiteral("123", 1, Predicates(List(example2), 1)), Predicates(List(example2), 1))
    val isNumeric3 = new PyIsNumeric(new PyStringLiteral("123 ", 1, Predicates(List(example3), 1)), Predicates(List(example3), 1))
    val isNumeric4 = new PyIsNumeric(new PyStringLiteral("123%*", 1, Predicates(List(example4), 1)), Predicates(List(example4), 1))
    assertEquals(false, isNumeric1.values.head)
    assertEquals("\"abc123\".isnumeric()", isNumeric1.code)
    assertEquals(true, isNumeric2.values.head)
    assertEquals(false, isNumeric3.values.head)
    assertEquals(false, isNumeric4.values.head)
  }

  @Test def startsWith(): Unit = {
    val example1 = ExamplePredicate(Map("inp" -> "abc123", "inp2"-> "abc"), Option(true))
    val example2 = ExamplePredicate(Map("inp" -> "123", "inp2"->"23"), Option(false))
    val example3 = ExamplePredicate(Map("inp" -> "abc123", "inp2"-> "123"), Option(false))
    val example4 = ExamplePredicate(Map("inp" -> "123", "inp2"->"3"), Option(false))
    val StartsWith1 = new PyStartsWith(new PyStringLiteral("abc123", 1, Predicates(List(example1), 1)),
      new PyStringLiteral("abc", 1, Predicates(List(example1), 1)), Predicates(List(example1), 1))
    val StartsWith2 = new PyStartsWith(new PyStringLiteral("123", 1, Predicates(List(example2), 1)),
      new PyStringLiteral("23", 1, Predicates(List(example2), 1)), Predicates(List(example2), 1))
    val EndsWith1 = new PyEndsWith(new PyStringLiteral("abc123", 1, Predicates(List(example3), 1)),
      new PyStringLiteral("123", 1, Predicates(List(example3), 1)), Predicates(List(example3), 1))
    val EndsWith2 = new PyEndsWith(new PyStringLiteral("123", 1, Predicates(List(example4), 1)),
      new PyStringLiteral("3", 1, Predicates(List(example4), 1)), Predicates(List(example4), 1))
    assertEquals(true, StartsWith1.values.head)
    assertEquals("\"abc123\".startswith(\"abc\")", StartsWith1.code)
    assertEquals(false, StartsWith2.values.head)
    assertEquals("\"abc123\".endswith(\"123\")", EndsWith1.code)
    assertEquals(true, EndsWith1.values.head)
    assertEquals(true, EndsWith2.values.head)

  }

//  @Test def maxNode(): Unit =
//  {
//    val node: PyMax = new PyMax(new IntListNode {
//      override val values: List[Iterable[Int]] = List(-1123 :: 2 :: 1 :: Nil)
//      override protected val parenless: Boolean = true
//      override val code: String = "[-1123, 2, 1]"
//      override val height: Int = 1
//      override val terms: Int = 1
//      override val children: Iterable[ASTNode] = Nil
//
//      override def includes(varName: String): Boolean = false
//      override lazy val usesVariables: Boolean = false
//      override def updateValues(predicates_t: Predicates): ASTNode = null
//
//      override val predicates: Predicates = Predicates(List(ExamplePredicate(Map("inp1"-> "[-1123, 2, 1]"), Option(2))), 1)
//
//      override def computeOnContext(ctx: Map[String, Any]): Option[Any] = doOp(arg.predicates.getExampleValue(arg.values, ctx))
//    })
//    assertEquals(1, node.values.length)
//    assertEquals(2, node.values.head)
//    assertEquals(Types.PyInt, node.nodeType)
//    assertEquals("max([-1123, 2, 1])", node.code)
//    assertEquals(2, node.height)
//    assertEquals(2, node.terms)
//    assertEquals(node.children.size, 1)
//  }

//  @Test def minNode(): Unit =
//  {
//    val node: PyMin = new PyMin(new IntListNode {
//      override val values: List[Iterable[Int]] = List(-1123 :: 2 :: 1 :: Nil)
//      override protected val parenless: Boolean = true
//      override val code: String = "[-1123, 2, 1]"
//      override val height: Int = 1
//      override val terms: Int = 1
//      override val children: Iterable[ASTNode] = Nil
//
//      override def includes(varName: String): Boolean = false
//      override lazy val usesVariables: Boolean = false
//
//      override val predicates: Predicates = null
//
//      override def computeOnContext(ctx: Map[String, Any]): Option[Any] = null
//
//      override def updateValues(predicates: Predicates): ASTNode = null
//    })
//    assertEquals(1, node.values.length)
//    assertEquals(-1123, node.values.head)
//    assertEquals(Types.PyInt, node.nodeType)
//    assertEquals("min([-1123, 2, 1])", node.code)
//    assertEquals(2, node.height)
//    assertEquals(2, node.terms)
//    assertEquals(node.children.size, 1)
//  }

  // Binary Operations
  @Test def binarySubstringNode(): Unit =
  {
    val example1 = ExamplePredicate(Map("inp" -> "abc"), Option("a"))
    val str: PyStringNode = new PyStringLiteral("abc", 1, Predicates(List(example1),1))

    var node: PyBinarySubstring = new PyBinarySubstring(str, new PyIntLiteral(0,1, Predicates(List(example1),1)),
      Predicates(List(example1),1))
    assertEquals(1, node.values.length)
    assertEquals("a", node.values.head)
    assertEquals(Types.PyString, node.nodeType)
    assertEquals("\"abc\"[0]", node.code)
    assertEquals(1, node.height)
    assertEquals(3, node.terms)
    assertEquals(node.children.size, 2)

    val example2 = ExamplePredicate(Map("inp" -> "abc"), Option("b"))
    node = new PyBinarySubstring(str, new PyIntLiteral(1,1, Predicates(List(example2),1)),
      Predicates(List(example2),1))
    assertEquals(1, node.values.length)
    assertEquals("b", node.values.head)
    assertEquals(Types.PyString, node.nodeType)
    assertEquals("\"abc\"[1]", node.code)
    assertEquals(1, node.height)
    assertEquals(3, node.terms)
    assertEquals(node.children.size, 2)

    val example3 = ExamplePredicate(Map("inp" -> "abc"), Option("c"))
    node = new PyBinarySubstring(str, new PyIntLiteral(2,1, Predicates(List(example3),1)),
      Predicates(List(example3),1))
    assertEquals(1, node.values.length)
    assertEquals("c", node.values.head)
    assertEquals(Types.PyString, node.nodeType)
    assertEquals("\"abc\"[2]", node.code)
    assertEquals(1, node.height)
    assertEquals(3, node.terms)
    assertEquals(node.children.size, 2)

//    node = new PyBinarySubstring(str, new PyIntLiteral(3,1))
//    assertEquals(0, node.values.length)
//    assertEquals(Types.PyString, node.nodeType)
//    assertEquals("\"abc\"[3]", node.code) /// TODO: ASK HILA WHY VALUES IS EXPECTED TO BE OF LEGNTH 0
//    assertEquals(1, node.height)
//    assertEquals(3, node.terms)
//    assertEquals(node.children.size, 2)
  }

  // Ternary Operations
  @Test def ternarySubstringNode(): Unit =
  {
    val example = ExamplePredicate(Map("inp" -> "abc"), Option("abc"))
    val predicates = Predicates(List(example),1)
    val str: PyStringNode = new PyStringLiteral("abc", 1, predicates)
    var node: TernarySubstring = new TernarySubstring(
      str,
      new PyIntLiteral(0,1,predicates),
      new PyIntLiteral(3,1, predicates), predicates)
    assertEquals(1, node.values.length)
    assertEquals("abc", node.values.head)
    assertEquals(Types.PyString, node.nodeType)
    assertEquals("\"abc\"[0:3]", node.code)
    assertEquals(1, node.height)
    assertEquals(4, node.terms)
    assertEquals(node.children.size, 3)

    // [-4, -3] -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(-4,1, predicates),
      new PyIntLiteral(-3,1, predicates), predicates)
    assertEquals("", node.values.head)

    // [-4, -2] -> "a"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(-4,1, predicates),
      new PyIntLiteral(-2,1, predicates),predicates)
    assertEquals("a", node.values.head)

    // [-4, -1] -> "ab"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(-4,1, predicates),
      new PyIntLiteral(-1,1, predicates), predicates)
    assertEquals("ab", node.values.head)

    // [-4, 0]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(-4,1, predicates),
      new PyIntLiteral(0,1, predicates), predicates)
    assertEquals("", node.values.head)

    // [-4, 1]  -> "a"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(-4,1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("a", node.values.head)

    // [-4, 2]  -> "ab"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(-4,1, predicates),
      new PyIntLiteral(2,1, predicates), predicates)
    assertEquals("ab", node.values.head)

    // [-4, 3]  -> "abc"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(-4,1,predicates),
      new PyIntLiteral(3,1, predicates), predicates)
    assertEquals("abc", node.values.head)

    // [-4, 4]  -> "abc"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(-4,1, predicates),
      new PyIntLiteral(4,1, predicates), predicates)
    assertEquals("abc", node.values.head)

    // [0, -4]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(-4,1, predicates), predicates)
    assertEquals("", node.values.head)

    // [0, -3]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(-3,1, predicates), predicates)
    assertEquals("", node.values.head)

    // [0, -2]  -> "a"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(-2,1, predicates), predicates)
    assertEquals("a", node.values.head)

    // [0, -1]  -> "ab"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("ab", node.values.head)

    // [0, 0]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(0, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // [0, 1]  -> "a"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(1, 1, predicates), predicates)
    assertEquals("a", node.values.head)

    // [0, 2]  -> "ab"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(2, 1, predicates), predicates)
    assertEquals("ab", node.values.head)

    // [0, 3]  -> "abc"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(3, 1, predicates), predicates)
    assertEquals("abc", node.values.head)

    // [0, 4]  -> "abc"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(4, 1, predicates), predicates)
    assertEquals("abc", node.values.head)

    // [1, -4]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(-4, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // [1, -3]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(-3, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // [1, -2]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(-2, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // [1, -1]  -> "b"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("b", node.values.head)

    // [1, 0]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(0, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // [1, 1]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // [1, 2]  -> "b"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(2, 1, predicates), predicates)
    assertEquals("b", node.values.head)

    // [1, 3]  -> "bc"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(3, 1, predicates), predicates)
    assertEquals("bc", node.values.head)

    // [1, 4]  -> "bc"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(4, 1, predicates), predicates)
    assertEquals("bc", node.values.head)


    // [3, -4]  -> ""
    // [3, -3]  -> ""
    // [3, -2]  -> ""
    // [3, -1]  -> ""
    // [3, 0]  -> ""
    // [3, 1]  -> ""
    // [3, 2]  -> ""
    // [3, 3]  -> ""
    // [3, 4]  -> ""
    for (i <- -3 to 4) {
      node = new TernarySubstring(
        str,
        new PyIntLiteral(3,1, predicates),
        new PyIntLiteral(i, 1, predicates), predicates)
      assertEquals("", node.values.head)
    }
  }

  // Quaternary Operations
  @Test def quaternarySubstringNode(): Unit =
  {
    val example = ExamplePredicate(Map("inp" -> "abc"), Option("abc"))
    val predicates = Predicates(List(example),1)
    val str: PyStringNode = new PyStringLiteral("abc", 1, predicates)
    var node: QuaternarySubstring = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(3,1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals(1, node.values.length)
    assertEquals("abc", node.values.head)
    assertEquals(Types.PyString, node.nodeType)
    assertEquals("\"abc\"[0:3:1]", node.code)
    assertEquals(1, node.height)
    assertEquals(5, node.terms)
    assertEquals(node.children.size, 4)

    // [-4, -3] -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-4,1, predicates),
      new PyIntLiteral(-3,1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("", node.values.head)

    // [-4, -2] -> "a"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-4,1, predicates),
      new PyIntLiteral(-2,1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("a", node.values.head)

    // [-4, -1] -> "ab"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-4,1, predicates),
      new PyIntLiteral(-1,1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("ab", node.values.head)

    // [-4, 0]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-4,1, predicates),
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("", node.values.head)

    // [-4, 1]  -> "a"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-4,1, predicates),
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("a", node.values.head)

    // [-4, 2]  -> "ab"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-4,1, predicates),
      new PyIntLiteral(2,1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("ab", node.values.head)

    // [-4, 3]  -> "abc"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-4,1, predicates),
      new PyIntLiteral(3,1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("abc", node.values.head)

    // [-4, 4]  -> "abc"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-4,1, predicates),
      new PyIntLiteral(4,1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("abc", node.values.head)

    // [0, -4]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(-4,1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("", node.values.head)

    // [0, -3]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(-3,1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("", node.values.head)

    // [0, -2]  -> "a"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(-2,1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("a", node.values.head)

    // [0, -1]  -> "ab"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(-1, 1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("ab", node.values.head)

    // [0, 0]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(0, 1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("", node.values.head)

    // [0, 1]  -> "a"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(1, 1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("a", node.values.head)

    // [0, 2]  -> "ab"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(2, 1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("ab", node.values.head)

    // [0, 3]  -> "abc"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(3, 1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("abc", node.values.head)

    // [0, 4]  -> "abc"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1, predicates),
      new PyIntLiteral(4, 1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("abc", node.values.head)

    // [1, -4]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(-4, 1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("", node.values.head)

    // [1, -3]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(-3, 1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("", node.values.head)

    // [1, -2]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(-2, 1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("", node.values.head)

    // [1, -1]  -> "b"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(-1, 1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("b", node.values.head)

    // [1, 0]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(0, 1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("", node.values.head)

    // [1, 1]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(1, 1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("", node.values.head)

    // [1, 2]  -> "b"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(2, 1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("b", node.values.head)

    // [1, 3]  -> "bc"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(3, 1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("bc", node.values.head)

    // [1, 4]  -> "bc"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1, predicates),
      new PyIntLiteral(4, 1, predicates),
      new PyIntLiteral(1,1, predicates), predicates)
    assertEquals("bc", node.values.head)


    // [3, -4]  -> ""
    // [3, -3]  -> ""
    // [3, -2]  -> ""
    // [3, -1]  -> ""
    // [3, 0]  -> ""
    // [3, 1]  -> ""
    // [3, 2]  -> ""
    // [3, 3]  -> ""
    // [3, 4]  -> ""
    for (i <- -3 to 4) {
      node = new QuaternarySubstring(
        str,
        new PyIntLiteral(3,1, predicates),
        new PyIntLiteral(i, 1, predicates),
        new PyIntLiteral(1,1, predicates), predicates)
      assertEquals("", node.values.head)
    }

    // s[3:-3:-1] -> 'cb'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(3,1, predicates),
      new PyIntLiteral(-3, 1, predicates),
      new PyIntLiteral(-1,1, predicates), predicates)
    assertEquals("cb", node.values.head)

    // s[3:-2:-1] -> 'c'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(3,1, predicates),
      new PyIntLiteral(-2, 1, predicates),
      new PyIntLiteral(-1,1, predicates), predicates)
    assertEquals("c", node.values.head)

    // s[3:-1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(3,1, predicates),
      new PyIntLiteral(-1, 1, predicates),
      new PyIntLiteral(-1,1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[3:0:-1] -> 'cb'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(3,1, predicates),
      new PyIntLiteral(0, 1, predicates),
      new PyIntLiteral(-1,1, predicates), predicates)
    assertEquals("cb", node.values.head)

    // s[3:1:-1] -> 'c'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(3,1, predicates),
      new PyIntLiteral(1, 1, predicates),
      new PyIntLiteral(-1,1, predicates), predicates)
    assertEquals("c", node.values.head)

    // s[3:3:-1] -> ''
    // s[3:2:-1] -> ''
    // s[3:4:-1] -> ''
    for (i <- 2 to 4) {
      node = new QuaternarySubstring(
        str,
        new PyIntLiteral(3, 1, predicates),
        new PyIntLiteral(i, 1, predicates),
        new PyIntLiteral(-1, 1, predicates), predicates)
      assertEquals("", node.values.head)
    }

    // s[2:-3:-1] -> 'cb'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(2, 1, predicates),
      new PyIntLiteral(-3, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("cb", node.values.head)

    // s[2:-2:-1] -> 'c'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(2, 1, predicates),
      new PyIntLiteral(-2, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("c", node.values.head)

    // s[2:-1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(2, 1, predicates),
      new PyIntLiteral(-1, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[2:0:-1] -> 'cb'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(2, 1, predicates),
      new PyIntLiteral(0, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("cb", node.values.head)

    // s[2:1:-1] -> 'c'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(2, 1, predicates),
      new PyIntLiteral(1, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("c", node.values.head)

    // s[2:2:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(2, 1, predicates),
      new PyIntLiteral(2, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[2:3:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(2, 1, predicates),
      new PyIntLiteral(3, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[2:4:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(2, 1, predicates),
      new PyIntLiteral(4, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[1:-3:-1] -> 'b'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1, 1, predicates),
      new PyIntLiteral(-3, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("b", node.values.head)

    // s[1:-2:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1, 1, predicates),
      new PyIntLiteral(-2, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[1:-1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1, 1, predicates),
      new PyIntLiteral(-1, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[1:0:-1] -> 'b'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1, 1, predicates),
      new PyIntLiteral(0, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("b", node.values.head)

    // s[1:1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1, 1, predicates),
      new PyIntLiteral(1, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[1:2:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1, 1, predicates),
      new PyIntLiteral(2, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[1:3:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1, 1, predicates),
      new PyIntLiteral(3, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[0:-3:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0, 1, predicates),
      new PyIntLiteral(-3, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[0:-2:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0, 1, predicates),
      new PyIntLiteral(-2, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[0:-1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0, 1, predicates),
      new PyIntLiteral(-1, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[0:0:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0, 1, predicates),
      new PyIntLiteral(0, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[0:1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0, 1, predicates),
      new PyIntLiteral(1, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[0:2:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0, 1, predicates),
      new PyIntLiteral(2, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[0:3:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0, 1, predicates),
      new PyIntLiteral(3, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[0:4:-1] -> ''
    for (i <- -3 to 4) {
      node = new QuaternarySubstring(
        str,
        new PyIntLiteral(0,1, predicates),
        new PyIntLiteral(i, 1, predicates),
        new PyIntLiteral(-1,1, predicates), predicates)
      assertEquals("", node.values.head)
    }

    // s[-1:-3:-1] -> 'cb'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-1, 1, predicates),
      new PyIntLiteral(-3, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("cb", node.values.head)

    // s[-1:-2:-1] -> 'c'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-1, 1, predicates),
      new PyIntLiteral(-2, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("c", node.values.head)

    // s[-1:-1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-1, 1, predicates),
      new PyIntLiteral(-1, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[-1:0:-1] -> 'cb'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-1, 1, predicates),
      new PyIntLiteral(0, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("cb", node.values.head)

    // s[-1:1:-1] -> 'c'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-1, 1, predicates),
      new PyIntLiteral(1, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("c", node.values.head)

    // s[-1:2:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-1, 1, predicates),
      new PyIntLiteral(2, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[-1:3:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-1, 1, predicates),
      new PyIntLiteral(3, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[-2:-3:-1] -> 'b'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-2, 1, predicates),
      new PyIntLiteral(-3, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("b", node.values.head)

    // s[-2:-2:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-2, 1, predicates),
      new PyIntLiteral(-2, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[-2:-1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-2, 1, predicates),
      new PyIntLiteral(-2, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[-2:0:-1] -> 'b'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-2, 1, predicates),
      new PyIntLiteral(0, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("b", node.values.head)

    // s[-2:1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-2, 1, predicates),
      new PyIntLiteral(1, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[-2:2:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-2, 1, predicates),
      new PyIntLiteral(2, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

    // s[-2:3:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-2, 1, predicates),
      new PyIntLiteral(3, 1, predicates),
      new PyIntLiteral(-1, 1, predicates), predicates)
    assertEquals("", node.values.head)

  }

  // TODO Write the unit tests for other nodes
  // Ternary Operations
  @Test def stringConcatNode(): Unit = ()
  @Test def stringStepNode(): Unit = ()
  @Test def intAdditionNode(): Unit = ()
  @Test def intSubtractionNode(): Unit = ()
  @Test def intDivisionNode(): Unit = ()
  @Test def findNode(): Unit = ()
  @Test def containsNode(): Unit = ()
  @Test def stringReplaceNode(): Unit = ()

  // List Operations
  @Test def stringSplitNode(): Unit = ()
  @Test def stringJoinNode(): Unit = ()
  @Test def stringStepListNode(): Unit = {
    val predicates = Predicates(List(ExamplePredicate(Map("x" -> "abcde"), Option("abcde")),
      ExamplePredicate( Map("x" -> "a"), Option("a")), ExamplePredicate(Map("x" -> "ab"), Option("a"))), 3)
    val x = new PyStringVariable("x", predicates)
    val step = new PyStringStep(x,new PyIntLiteral(1,x.values.length, predicates), predicates)
    assertEquals("x[::1]",step.code)
    assertEquals(List("abcde","a","ab"),step.values)

    val step2 = new PyStringStep(x,new PyIntLiteral(-1,x.values.length, predicates), predicates)
    assertEquals("x[::-1]",step2.code)
    assertEquals(List("edcba", "a", "ba"),step2.values)

    val step3 = new PyStringStep(x,new PyIntLiteral(2,x.values.length, predicates), predicates)
    assertEquals("x[::2]",step3.code)
    assertEquals(List("ace", "a", "a"),step3.values)

    val step4 = new PyStringStep(x,new PyIntLiteral(-2,x.values.length, predicates), predicates)
    assertEquals("x[::-2]",step4.code)
    assertEquals(List("eca", "a", "b"),step4.values)

    val step5 = new PyStringStep(x, new PyIntLiteral(0,x.values.length, predicates), predicates)
    assertEquals(Nil,step5.values)
  }
  @Test def substringListNode(): Unit = ()
  @Test def stringToIntListNode(): Unit = ()
  @Test def sortedStringListNode(): Unit = ()
  @Test def stringCount(): Unit = {
    val predicates1 = Predicates(List(ExamplePredicate(Map("x" -> ""), Option(0)),
      ExamplePredicate(Map("x" -> "abc"), Option(1)), ExamplePredicate(Map("x" -> "bc"), Option(0)), ExamplePredicate(Map("x" -> "aaaabc"),
        Option(4)), ExamplePredicate(Map("x" -> "abcabc"), Option(2))), 5)
    val x = new PyStringVariable("x", predicates1)
    val count = new PyCount(x,new PyStringLiteral("a",x.values.length, predicates1), predicates1)
    assertEquals("x.count(\"a\")",count.code)
    assertEquals(List(0, 1, 0, 4, 2), count.values)

    val predicates2 = Predicates(List(ExamplePredicate(Map("x" -> ""), Option(0)),
      ExamplePredicate(Map("x" -> "abc"), Option(0)), ExamplePredicate(Map("x" -> "bc"), Option(0)), ExamplePredicate(Map("x" -> "aaaabc"),
        Option(2)), ExamplePredicate(Map("x" -> "abcabc"), Option(0))), 5)
    val count2 = new PyCount(x,new PyStringLiteral("aa",x.values.length, predicates2), predicates2)
    assertEquals("x.count(\"aa\")",count2.code)
    assertEquals(List(0, 0, 0, 2, 0),count2.values)
  }

  @Test def printingNodes() = {
    val predicates1 = Predicates(List(ExamplePredicate(Map("inp" -> "'abc'"), Option("'abc '"))), 1)
    assertEquals("2",new PyIntLiteral(2,1, predicates1).code)
    val inp = new PyStringVariable("inp",predicates1)
    val addStrings = new PyStringConcat(inp,new PyStringLiteral(" ",1, predicates1), predicates1)
    assertEquals("inp + \" \"", addStrings.code)
    val substr = new TernarySubstring(addStrings,new PyIntLiteral(0,1, predicates1),
      new PyIntLiteral(1,1, predicates1), predicates1)
    assertEquals("(inp + \" \")[0:1]",substr.code)
    val substr2 = new TernarySubstring(inp,new PyIntLiteral(0,1, predicates1),
      new PyIntLiteral(1,1, predicates1), predicates1)
    assertEquals("inp[0:1]", substr2.code)

    val split = new PyStringSplit(addStrings,new PyStringLiteral(",",1, predicates1), predicates1)
    assertEquals("(inp + \" \").split(\",\")",split.code)
    val split2 = new PyStringSplit(inp,new PyStringLiteral(",",1, predicates1), predicates1)
    assertEquals("inp.split(\",\")",split2.code)

    val step = new PyStringStep(inp,new PyIntLiteral(-2,1, predicates1), predicates1)
    assertEquals("inp[::-2]",step.code)
    val step2 = new PyStringStep(addStrings,new PyIntLiteral(-2,1, predicates1), predicates1)
    assertEquals("(inp + \" \")[::-2]",step2.code)

    val find = new PyFind(addStrings,inp, predicates1)
    assertEquals("(inp + \" \").find(inp)",find.code)

    val find2 = new PyFind(step2,inp, predicates1)
    assertEquals("(inp + \" \")[::-2].find(inp)",find2.code)

    val addNumbers = new PyIntAddition(new PyIntAddition(new PyIntLiteral(1,1, predicates1),
      new PyIntLiteral(2,1, predicates1), predicates1),
      new PyIntAddition(new PyIntLiteral(3,1, predicates1),
        new PyIntLiteral(4,1, predicates1), predicates1), predicates1)
    assertEquals("1 + 2 + 3 + 4", addNumbers.code)

    val divNumbers = new PyIntDivision(addNumbers,new PyIntLiteral(1,1, predicates1), predicates1)
    assertEquals("(1 + 2 + 3 + 4) // 1", divNumbers.code)

    val divNumbers2 = new PyIntDivision(new PyIntLiteral(1,1, predicates1),addNumbers, predicates1)
    assertEquals("1 // (1 + 2 + 3 + 4)", divNumbers2.code)
  }
}