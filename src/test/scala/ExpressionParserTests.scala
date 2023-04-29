import ast.{ASTNode, PyIntLiteral, PyStringConcat, PyStringLiteral, PyStringVariable, StringLiteral, StringNode, StringStringMapCompNode, Types}
import enumeration.InputsValuesManager
import org.junit.Assert._
import org.junit.Test
import org.scalatestplus.junit.JUnitSuite
import sygus.{ExamplePredicate, ExpressionParser, Predicates, PythonPBETask, UsesVariablesPredicate}

import scala.collection.mutable

class ExpressionParserTests extends JUnitSuite{
  val predicates = Predicates(List(ExamplePredicate(Map("x" -> 18), Option(19)), UsesVariablesPredicate()), 1)
  val parser = new ExpressionParser(predicates)
  @Test def parseArithAddition(): Unit = assertEquals("x + 1", parser.parse("x + 1").code)
  @Test def parseArithSubstraction(): Unit = assertEquals("x - 1", parser.parse("x - 1").code)
  val minus_one = parser.parse("-1")
  @Test def parseNegativeExpressions(): Unit = assertEquals("-1", minus_one.code)
  @Test def NegativeExpressionsHeight(): Unit = assertEquals(0, minus_one.height)
  @Test def parseInt(): Unit = assertEquals("123", parser.parse("123").code)
  @Test def parseNegInt(): Unit = assertEquals("-321", parser.parse("-321").code)

  @Test def parsePosInt(): Unit = assertEquals("32", parser.parse("+32").code)

  @Test def parseTrue(): Unit = assertEquals("True", parser.parse("True").code)

  @Test def parseFalse(): Unit = assertEquals("False", parser.parse("False").code)
  @Test def parseString(): Unit = assertEquals("abc", parser.parse("'abc'").code)
//
//  @Test def parseEmptyString(): Unit = assertEquals(Some(""), parser.parse("''"))
//
//  @Test def parseStringDoubleQuotes(): Unit = assertEquals(Some("abc"), parser.parse("\"abc\""))
//
//  @Test def parseStrings(): Unit = assertEquals(Some("abcdef"), parser.parse("'abc' 'def'"))
//
//  // Lists
//  @Test def parseIntList(): Unit = assertEquals(Some(List(123)), parser.parse("[123]"))
//
//  @Test def parseStrList(): Unit = assertEquals(Some(List("abc")), parser.parse("['abc']"))
//
//  @Test def parseBoolList(): Unit = assertEquals(Some(List(false)), parser.parse("[False]"))
//
//  @Test def parseIntMultiList(): Unit = assertEquals(Some(List(1, 2, -3)), parser.parse("[1, 2, -3]"))
//
//  @Test def parseStrMultiList(): Unit = assertEquals(Some(List("a", "b", "c")), parser.parse("['a', 'b', 'c']"))
//
//  @Test def parseBoolMultiList(): Unit = assertEquals(Some(List(false, true)), parser.parse("[False, True]"))
//
//  @Test def parseEmptyList(): Unit = assertEquals(Some(List()), parser.parse("[]"))
//
//  @Test def parseIntBoolList(): Unit = assertEquals(None, parser.parse("[1, True]"))
//
//  @Test def parseIntStrList(): Unit = assertEquals(None, parser.parse("[2, 'asd']"))
//
//  @Test def parseExtraSpaceList0(): Unit = assertEquals(Some(List(1, 2, -3)), parser.parse("  [1, 2, -3] "))
//
//  @Test def parseExtraSpaceList1(): Unit = assertEquals(Some(List(1, 2, -3)), parser.parse("[1,2,-3]"))
//
//  @Test def parseExtraSpaceList2(): Unit = assertEquals(Some(List(1, 2, -3)), parser.parse("[1 , 2 , -3]"))
//
//  @Test def parseExtraSpaceList3(): Unit = assertEquals(Some(List(1, 2, -3)), parser.parse(" [ 1 , 2 , -3 ] "))
//
//  // Maps
//  @Test def parseStrStrMap(): Unit = assertEquals(Some(Map("a" -> "a", "b" -> "b")), parser.parse("{'a': 'a', 'b': 'b'}"))
//
//  @Test def parseStrIntMap(): Unit = assertEquals(Some(Map("a" -> 0, "b" -> 1)), parser.parse("{'a': 0, 'b': 1}"))
//
//  @Test def parseIntStrMap(): Unit = assertEquals(Some(Map(0 -> "a", 1 -> "b")), parser.parse("{0: 'a', 1: 'b'}"))
//
//  @Test def parseIntIntMap(): Unit = assertEquals(Some(Map(0 -> 1, 1 -> 2)), parser.parse("{0: 1, 1: 2}"))
//
//  @Test def parseExtraSpacesMap0(): Unit = assertEquals(Some(Map(0 -> 1, 1 -> 2)), parser.parse(" {0: 1, 1: 2} "))
//
//  @Test def parseExtraSpacesMap1(): Unit = assertEquals(Some(Map(0 -> 1, 1 -> 2)), parser.parse("{0 : 1, 1 : 2}"))
//
//  @Test def parseExtraSpacesMap2(): Unit = assertEquals(Some(Map(0 -> 1, 1 -> 2)), parser.parse("{ 0: 1, 1: 2 }"))
//
//  @Test def parseExtraSpacesMap3(): Unit = assertEquals(Some(Map(0 -> 1, 1 -> 2)), parser.parse("{ 0 : 1 , 1 : 2 }"))
//
//  @Test def parseExtraSpacesMap4(): Unit = assertEquals(Some(Map(0 -> 1, 1 -> 2)), parser.parse("{0:1,1:2}"))
//
//  @Test def parseEmptyMap(): Unit = assertEquals(Some(Map()), parser.parse("{}"))
//
//  @Test def parseBadValTypeMap(): Unit = assertEquals(None, parser.parse("{0: 'a', 1: 2}"))
//
//  @Test def parseBadKeyTypeMap(): Unit = assertEquals(None, parser.parse("{0: 'a', 'a': 'b'}"))


}

class ExpressionParserTestsEnumerator extends JUnitSuite {

  @Test def enumerateVocabNoOE: Unit = {
    val task = PythonPBETask.fromString(
      """{
        |  "varName": "rs",
        |  "env": [
        |    {
        |      "#": "",
        |      "$": "",
        |      "s": "'test'",
        |      "rs": "'es'"
        |    },
        |    {
        |      "#": "",
        |      "$": "",
        |      "s": "'example'",
        |      "rs": "'m'"
        |    },
        |    {
        |      "#": "",
        |      "$": "",
        |      "s": "'testing'",
        |      "rs": "'t'"
        |    }
        |  ]
        |}""".stripMargin, true)
    val oeManager = new InputsValuesManager()
    val bank = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
    val enumerator = new enumeration.PyProbEnumerator(task.vocab, oeManager, task.predicates, false, 0, bank,
      bank)
    val parser = new ExpressionParser(task.predicates)
    assertEquals(enumerator.hasNext, true)
    assertEquals(6, task.vocab.leavesMakers.size)
    assertEquals(task.vocab.nodeMakers.size, 36)
    var current_code: String = ""
//    for(i <- 1 to 50)
//      {
//        current_code = enumerator.next().code
//        assertEquals(current_code, parser.parse(current_code).code)
//      }

    parser.parse("len(s)")
//    current_code = enumerator.next().code
//    assertEquals(current_code, parser.parse(current_code).code)
    //  assertEquals(enumerator.next().code,"0")
    //  assertEquals(enumerator.next().code,"1")
    //  assertEquals(enumerator.next().code,"-1")
    //  assertEquals(enumerator.next().code,"3")
    //  assertEquals(enumerator.next().code,"s")
    //  assertEquals(enumerator.next().code,"len(s)")
    //  assertEquals(enumerator.next().code,"\" \".isalpha()")
    //  assertEquals(enumerator.next().code,"s.isalpha()")
    //  assertEquals(enumerator.next().code,"s.upper()")
    //  assertEquals(enumerator.next().code,"str(0)")
    //  assertEquals(enumerator.next().code,"str(1)")
    //  assertEquals(enumerator.next().code,"str(-1)")
    //  assertEquals(enumerator.next().code,"str(3)")
    //  val ast1 = enumerator.next()
    //  assertEquals(ast1.cost, 3)
    //  assertEquals(ast1.code,"\" \" + \" \"")
    //  assertEquals(enumerator.next().code,"\" \" + s")
    //  assertEquals(enumerator.next().code,"s + \" \"")
    //  assertEquals(enumerator.next().code,"s + s")
    //  assertEquals(enumerator.next().code,"s[0]")
    //  assertEquals(enumerator.next().code,"s[1]")
    //  assertEquals(enumerator.next().code,"s[3]")
    //  assertEquals(enumerator.next().code,"s[::-1]")
    //  assertEquals(enumerator.next().code,"s[::3]")
    //  assertEquals(enumerator.next().code,"len(str(-1))")
    //  assertEquals(enumerator.next().code,"str(len(s))")
    //  assertEquals(enumerator.next().code,"\" \".split(\" \")")
    //  assertEquals(enumerator.next().code,"\" \".split(s)")
    //  assertEquals(enumerator.next().code,"s.split(\" \")")
    //  assertEquals(enumerator.next().code,"1 + 3")
    //  assertEquals(enumerator.next().code,"-1 + -1")
    //  assertEquals(enumerator.next().code,"3 + 3")
    //  assertEquals(enumerator.next().code,"-1 * 3")
    //  assertEquals(enumerator.next().code,"3 * 3")
    //  assertEquals(enumerator.next().code,"-1 - 3")
    //  assertEquals(enumerator.next().code, "{var: var for var in s}")
    //  val ast2 = enumerator.next()
    //  assertEquals(ast2.cost, 4)
    //  assertEquals(ast2.code,"\" \" + s.upper()")
    //  assertEquals(enumerator.next().code,"\" \" + str(0)")
    //  assertEquals(enumerator.next().code,"\" \" + str(1)")
    //  assertEquals(enumerator.next().code,"\" \" + str(-1)")
    //  assertEquals(enumerator.next().code,"\" \" + str(3)")
    //  assertEquals(enumerator.next().code,"s + s.upper()")
    //  assertEquals(enumerator.next().code,"s + str(0)")
    //  assertEquals(enumerator.next().code,"s + str(1)")
    //  assertEquals(enumerator.next().code,"s + str(-1)")
    //  assertEquals(enumerator.next().code,"s + str(3)")
    //  assertEquals(enumerator.next().code,"s.upper() + \" \"")
    //  assertEquals(enumerator.next().code,"s.upper() + s")
    //  assertEquals(enumerator.next().code,"str(0) + \" \"")
    //  assertEquals(enumerator.next().code,"str(0) + s")
    //  assertEquals(enumerator.next().code,"str(1) + \" \"")
    //  assertEquals(enumerator.next().code,"str(1) + s")
    //  assertEquals(enumerator.next().code,"str(-1) + \" \"")
    //  assertEquals(enumerator.next().code,"str(-1) + s")
    //  assertEquals(enumerator.next().code,"str(3) + \" \"")
    //  assertEquals(enumerator.next().code,"str(3) + s")
    //  assertEquals(enumerator.next().code,"s.upper()[0]")
    //  assertEquals(enumerator.next().code,"s.upper()[1]")
    //  assertEquals(enumerator.next().code,"s.upper()[3]")
    //  assertEquals(enumerator.next().code,"str(-1)[0]")
    //  assertEquals(enumerator.next().code,"s.upper()[::-1]")
    //  assertEquals(enumerator.next().code,"s.upper()[::3]")
    //  assertEquals(enumerator.next().code,"str(-1)[::-1]")
    //  assertEquals(enumerator.next().code,"len(\" \" + s)")
    //  assertEquals(enumerator.next().code,"len(s + s)")
    //  assertEquals(enumerator.next().code,"len(s[::3])")
    //  assertEquals(enumerator.next().code,"len({var: var for var in s})")
    //  assertEquals(enumerator.next().code,"(s + s).upper()")
    //  print(enumerator.next().cost)
    //  assertEquals(enumerator.next().code,"0 > 0")
    //  assertEquals(enumerator.next().code,"0 > -1")
    //  //    val ast2 = enumerator.next()
    //  //    assertEquals(ast2.cost,3)
    //  assertEquals(enumerator.nested, false)
    //  assertEquals(ast1.code, "{var: var for var in \" \"}")
    //  assertEquals(ast1.cost, 3)
    //  assertEquals(ast1.getClass, classOf[StringStringMapCompNode])
    //
    //  assertEquals(enumerator.next().code, "{var: var for var in s}")
    //
    //  val ast4 = enumerator.next()
    //  assertEquals(ast4.code, "-1 + -1")
    //  assertEquals(ast4.cost, 3)
    //  assertEquals("\" \" + str(0)",enumerator.next().code)
    //  assertEquals("\" \" + str(1)",enumerator.next().code)
    //  assertEquals("\" \" + str(-1)",enumerator.next().code)
    //  assertEquals("s + str(0)",enumerator.next().code)
    //  assertEquals("s + str(1)",enumerator.next().code)
    //  assertEquals("s + str(-1)",enumerator.next().code)
    //  assertEquals("str(0) + \" \"",enumerator.next().code)
    //  assertEquals("str(0) + s",enumerator.next().code)
    //  assertEquals("str(1) + \" \"",enumerator.next().code)
    //  assertEquals("str(1) + s",enumerator.next().code)
    //  assertEquals("str(-1) + \" \"",enumerator.next().code)
    //  assertEquals("str(-1) + s",enumerator.next().code)
    //  assertEquals("str(-1)[0]",enumerator.next().code)
    //  assertEquals("str(-1)[::-1]",enumerator.next().code)
    //  assertEquals("len(\" \" + s)",enumerator.next().code)
    //  assertEquals("len(s + s)",enumerator.next().code)
    //  assertEquals("len({var: var for var in s})",enumerator.next().code)
    //  assertEquals("str(len(str(-1)))",enumerator.next().code)
    //  assertEquals("str(-1 + -1)",enumerator.next().code)
    //  assertEquals("\" \"[0:0]",enumerator.next().code)
    //  assertEquals("s[0:-1]",enumerator.next().code)
    //  assertEquals("s[1:-1]",enumerator.next().code)
    //  assertEquals("str(0).split(\" \")",enumerator.next().code)
    //  assertEquals("str(1).split(\" \")",enumerator.next().code)
    //  assertEquals("str(-1).split(\" \")",enumerator.next().code)
    //  assertEquals("{var: var for var in str(0)}",enumerator.next().code)
    //  assertEquals("{var: var for var in str(1)}",enumerator.next().code)
    //  assertEquals("{var: var for var in str(-1)}",enumerator.next().code)
    //  assertEquals("0 - len(s)",enumerator.next().code)
    //  assertEquals("1 - len(s)",enumerator.next().code)
    //  assertEquals("-1 - len(s)",enumerator.next().code)
    //  assertEquals("\" \" + \" \" + \" \"",enumerator.next().code)
    //  assertEquals("\" \" + \" \" + s",enumerator.next().code)
    //  assertEquals("\" \" + s + \" \"",enumerator.next().code)
    //  assertEquals("\" \" + s + s",enumerator.next().code)
    //  assertEquals("\" \" + s[0]",enumerator.next().code)
    //  assertEquals("\" \" + s[1]",enumerator.next().code)
    //  assertEquals("\" \" + s[::-1]",enumerator.next().code)
    //  assertEquals("\" \" + str(len(s))",enumerator.next().code)
    //  assertEquals("s + \" \" + \" \"",enumerator.next().code)
    //  assertEquals("s + \" \" + s",enumerator.next().code)
    //  assertEquals("s + s + \" \"",enumerator.next().code)
    //  assertEquals("s + s + s",enumerator.next().code)
    //  assertEquals("s + s[0]",enumerator.next().code)
    //  assertEquals("s + s[1]",enumerator.next().code)
    //  assertEquals("s + s[::-1]",enumerator.next().code)
    //  assertEquals("s + str(len(s))",enumerator.next().code)
    //  assertEquals("s[0] + \" \"",enumerator.next().code)
    //  assertEquals("s[0] + s",enumerator.next().code)
    //  assertEquals("s[1] + \" \"",enumerator.next().code)
    //  assertEquals("s[1] + s",enumerator.next().code)
    //  assertEquals("s[::-1] + \" \"",enumerator.next().code)
    //  assertEquals("s[::-1] + s",enumerator.next().code)
    //  assertEquals("str(len(s)) + \" \"",enumerator.next().code)
    //  assertEquals("str(len(s)) + s",enumerator.next().code)
    //  assertEquals("str(0) + str(0)",enumerator.next().code)
    //  assertEquals("str(0) + str(1)",enumerator.next().code)
    //  assertEquals("str(0) + str(-1)",enumerator.next().code)
    //  assertEquals("str(1) + str(0)",enumerator.next().code)
    //  assertEquals("str(1) + str(1)",enumerator.next().code)
    //  assertEquals("str(1) + str(-1)",enumerator.next().code)
    //  assertEquals("str(-1) + str(0)",enumerator.next().code)
    //  assertEquals("str(-1) + str(1)",enumerator.next().code)
    //  assertEquals("str(-1) + str(-1)",enumerator.next().code)
    //  assertEquals("s[len(str(-1))]",enumerator.next().code)
    //  assertEquals("s[::-1][0]",enumerator.next().code)
    //  assertEquals("s[::-1][1]",enumerator.next().code)
    //  assertEquals("s[::len(str(-1))]",enumerator.next().code)
    //  assertEquals("s[::-1 + -1]",enumerator.next().code)
    //  assertEquals("(s + s)[::-1]",enumerator.next().code)
    //  assertEquals("len(\" \" + str(-1))",enumerator.next().code)
    //  assertEquals("len(s + str(-1))",enumerator.next().code)
    //  assertEquals("len(s[1:-1])",enumerator.next().code)
    //  assertEquals("str(len(\" \" + s))",enumerator.next().code)
    //  assertEquals("str(len(s + s))",enumerator.next().code)
    //  assertEquals("str(len({var: var for var in s}))",enumerator.next().code)
    //  assertEquals("str(0 - len(s))",enumerator.next().code)
    //  assertEquals("str(1 - len(s))",enumerator.next().code)
    //  assertEquals("str(-1 - len(s))",enumerator.next().code)
    //  assertEquals("s[1:len(s)]",enumerator.next().code)
    //  assertEquals("s.split(s[0])",enumerator.next().code)
    //  assertEquals("s.split(s[1])",enumerator.next().code)
    //  assertEquals("(\" \" + \" \").split(s)",enumerator.next().code)
    //  assertEquals("(\" \" + s).split(\" \")",enumerator.next().code)
    //  assertEquals("(s + \" \").split(s)",enumerator.next().code)
    //  assertEquals("(s + s).split(\" \")",enumerator.next().code)
    //  assertEquals("s[0].split(\" \")",enumerator.next().code)
    //  assertEquals("s[1].split(\" \")",enumerator.next().code)
    //  assertEquals("s[::-1].split(\" \")",enumerator.next().code)
    //  assertEquals("str(len(s)).split(\" \")",enumerator.next().code)
    //  assertEquals("str(-1).split(str(1))",enumerator.next().code)
    //  assertEquals("{var: var + var for var in \" \"}",enumerator.next().code)

  }


}
