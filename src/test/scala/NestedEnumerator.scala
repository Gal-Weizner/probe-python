import enumeration.{InputsValuesManager, ProbUpdate}
import org.junit.Test
import org.scalatestplus.junit.JUnitSuite
import org.junit.Assert._
import sygus.{ExamplePredicate, InputParser, Predicates, PySynthesisTask, Python3Lexer, Python3Parser, PythonPBETask}
import ast._
import enumeration.ProbUpdate.priors
import org.antlr.v4.runtime.{BailErrorStrategy, BufferedTokenStream, CharStreams}
import vocab.VocabFactory
import sygus.Python3Parser._

import scala.collection.mutable
import scala.io.Source.fromFile

class NestedEnumerator extends JUnitSuite {

  @Test def enumerateVocabNoOE: Unit = {
      val predicates1 = Predicates(List(ExamplePredicate(Map("inp"->"abc"), Option(""),0)), 1)
      val predicates2 = Predicates(List(ExamplePredicate(Map("inp"->"abc"), Option(""),0),
        ExamplePredicate(Map("inp"->"abc"), Option(""),1), ExamplePredicate(Map("inp"->"abc"), Option(""),2),
        ExamplePredicate(Map("inp"->"abc"), Option(""),3)), 4)
      val stringLiteral: StringNode = new StringLiteral("abc", 1, predicates1)
      assertEquals(1, stringLiteral.values.length)
      assertEquals("abc", stringLiteral.values(0))
      assertEquals(Types.String, stringLiteral.nodeType)
      assertEquals(stringLiteral.updateValues(predicates2).values, List("abc", "abc", "abc", "abc"))

      val node = new PyStringConcat(new PyStringLiteral("abc", 1, predicates1),
          new PyStringLiteral("def", 1, predicates1), predicates1)
      assertEquals(List("abcdef"), node.values)
      assertEquals(node.updateValues(predicates2).values, List("abcdef", "abcdef", "abcdef", "abcdef"))

      val concat = new PyStringConcat(node, new PyStringLiteral("klm", 1, predicates1), predicates1)
      assertEquals(List("abcdefklm"), concat.values)
      assertEquals(concat.updateValues(predicates2).values, List("abcdefklm", "abcdefklm", "abcdefklm", "abcdefklm"))

      val predicates3 = Predicates(List(ExamplePredicate(Map("x" -> "abcde"), Option(""),0),
        ExamplePredicate(Map("x" -> "a"), Option(""),1), ExamplePredicate(Map("x" -> "ab"), Option(""),2)),3)
      val x = new PyStringVariable("x", predicates3)
      assertEquals(x.values, List("abcde", "a", "ab"))
      val predicates4 = Predicates(List(ExamplePredicate(Map("x" -> "abcde"), Option(""),0),
        ExamplePredicate(Map("x" -> "abcde"), Option(""),1), ExamplePredicate(Map("x" -> "abcde"), Option(""),2),
        ExamplePredicate(Map("x" -> "a"), Option(""),3), ExamplePredicate(Map("x" -> "ab"), Option(""),4)), 5)
      assertEquals(x.updateValues(predicates4).values, List("abcde", "abcde", "abcde", "a", "ab"))

    val predicates5 = Predicates(List(ExamplePredicate(Map("inp"->"abc"), Option(""),0), ExamplePredicate(Map("inp"->"abc"), Option(""),1),
      ExamplePredicate(Map("inp"->"abc"), Option(""),2),ExamplePredicate(Map("inp"->"abc"), Option(""),3)), 4)
    val literal: PyIntLiteral = new PyIntLiteral(42, 2, predicates1)
      assertEquals(literal.values, List(42, 42))
      assertEquals(literal.updateValues(predicates5).values, List(42, 42, 42, 42))
  }

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
    assertEquals(enumerator.hasNext, true)
    assertEquals(6,task.vocab.leavesMakers.size)
    assertEquals(task.vocab.nodeMakers.size,36)
    assertEquals(enumerator.next().code, "\" \"")
    assertEquals(enumerator.next().code,"0")
    assertEquals(enumerator.next().code,"1")
    assertEquals(enumerator.next().code,"-1")
    assertEquals(enumerator.next().code,"3")
    assertEquals(enumerator.next().code,"s")
    assertEquals(enumerator.next().code,"len(s)")
    assertEquals(enumerator.next().code,"\" \".isalpha()")
    assertEquals(enumerator.next().code,"s.isalpha()")
    assertEquals(enumerator.next().code,"s.isnumeric()")
    assertEquals(enumerator.next().code,"s.upper()")
    assertEquals(enumerator.next().code,"str(0)")
    assertEquals(enumerator.next().code,"str(1)")
    assertEquals(enumerator.next().code,"str(-1)")
    assertEquals(enumerator.next().code,"str(3)")
    val ast1 = enumerator.next()
    assertEquals(ast1.cost, 3)
    assertEquals(ast1.code,"\" \" + \" \"")
    assertEquals(enumerator.next().code,"\" \" + s")
    assertEquals(enumerator.next().code,"s + \" \"")
    assertEquals(enumerator.next().code,"s + s")
    assertEquals(enumerator.next().code,"s[0]")
    assertEquals(enumerator.next().code,"s[1]")
    assertEquals(enumerator.next().code,"s[3]")
    assertEquals(enumerator.next().code,"s[::-1]")
    assertEquals(enumerator.next().code,"s[::3]")
    assertEquals(enumerator.next().code,"len(str(-1))")
    assertEquals(enumerator.next().code,"str(len(s))")
    assertEquals(enumerator.next().code,"\" \".split(\" \")")
    assertEquals(enumerator.next().code,"\" \".split(s)")
    assertEquals(enumerator.next().code,"s.split(\" \")")
    assertEquals(enumerator.next().code,"1 + 3")
    assertEquals(enumerator.next().code,"-1 + -1")
    assertEquals(enumerator.next().code,"3 + 3")
    assertEquals(enumerator.next().code,"-1 * 3")
    assertEquals(enumerator.next().code,"3 * 3")
    assertEquals(enumerator.next().code,"-1 - 3")
    assertEquals(enumerator.next().code, "{var: var for var in s}")
    val ast2 = enumerator.next()
    assertEquals(ast2.cost, 4)
    assertEquals(ast2.code,"\" \" + s.upper()")
    assertEquals(enumerator.next().code,"\" \" + str(0)")
    assertEquals(enumerator.next().code,"\" \" + str(1)")
    assertEquals(enumerator.next().code,"\" \" + str(-1)")
    assertEquals(enumerator.next().code,"\" \" + str(3)")
    assertEquals(enumerator.next().code,"s + s.upper()")
    assertEquals(enumerator.next().code,"s + str(0)")
    assertEquals(enumerator.next().code,"s + str(1)")
    assertEquals(enumerator.next().code,"s + str(-1)")
    assertEquals(enumerator.next().code,"s + str(3)")
    assertEquals(enumerator.next().code,"s.upper() + \" \"")
    assertEquals(enumerator.next().code,"s.upper() + s")
    assertEquals(enumerator.next().code,"str(0) + \" \"")
    assertEquals(enumerator.next().code,"str(0) + s")
    assertEquals(enumerator.next().code,"str(1) + \" \"")
    assertEquals(enumerator.next().code,"str(1) + s")
    assertEquals(enumerator.next().code,"str(-1) + \" \"")
    assertEquals(enumerator.next().code,"str(-1) + s")
    assertEquals(enumerator.next().code,"str(3) + \" \"")
    assertEquals(enumerator.next().code,"str(3) + s")
    assertEquals(enumerator.next().code,"s.upper()[0]")
    assertEquals(enumerator.next().code,"s.upper()[1]")
    assertEquals(enumerator.next().code,"s.upper()[3]")
    assertEquals(enumerator.next().code,"str(-1)[0]")
    assertEquals(enumerator.next().code,"s.upper()[::-1]")
    assertEquals(enumerator.next().code,"s.upper()[::3]")
    assertEquals(enumerator.next().code,"str(-1)[::-1]")
    assertEquals(enumerator.next().code,"len(\" \" + s)")
    assertEquals(enumerator.next().code,"len(s + s)")
    assertEquals(enumerator.next().code,"len(s[::3])")
    assertEquals(enumerator.next().code,"len({var: var for var in s})")
    assertEquals(enumerator.next().code,"(s + s).upper()")
    print(enumerator.next().cost)
    assertEquals(enumerator.next().code,"0 > 0")
    assertEquals(enumerator.next().code,"0 > -1")
//    val ast2 = enumerator.next()
//    assertEquals(ast2.cost,3)
    assertEquals(enumerator.nested, false)
    assertEquals(ast1.code, "{var: var for var in \" \"}")
    assertEquals(ast1.cost, 3)
    assertEquals(ast1.getClass, classOf[StringStringMapCompNode])

    assertEquals(enumerator.next().code, "{var: var for var in s}")

    val ast4 = enumerator.next()
    assertEquals(ast4.code, "-1 + -1")
    assertEquals(ast4.cost, 3)
    assertEquals("\" \" + str(0)",enumerator.next().code)
    assertEquals("\" \" + str(1)",enumerator.next().code)
    assertEquals("\" \" + str(-1)",enumerator.next().code)
    assertEquals("s + str(0)",enumerator.next().code)
    assertEquals("s + str(1)",enumerator.next().code)
    assertEquals("s + str(-1)",enumerator.next().code)
    assertEquals("str(0) + \" \"",enumerator.next().code)
    assertEquals("str(0) + s",enumerator.next().code)
    assertEquals("str(1) + \" \"",enumerator.next().code)
    assertEquals("str(1) + s",enumerator.next().code)
    assertEquals("str(-1) + \" \"",enumerator.next().code)
    assertEquals("str(-1) + s",enumerator.next().code)
    assertEquals("str(-1)[0]",enumerator.next().code)
    assertEquals("str(-1)[::-1]",enumerator.next().code)
    assertEquals("len(\" \" + s)",enumerator.next().code)
    assertEquals("len(s + s)",enumerator.next().code)
    assertEquals("len({var: var for var in s})",enumerator.next().code)
    assertEquals("str(len(str(-1)))",enumerator.next().code)
    assertEquals("str(-1 + -1)",enumerator.next().code)
    assertEquals("\" \"[0:0]",enumerator.next().code)
    assertEquals("s[0:-1]",enumerator.next().code)
    assertEquals("s[1:-1]",enumerator.next().code)
    assertEquals("str(0).split(\" \")",enumerator.next().code)
    assertEquals("str(1).split(\" \")",enumerator.next().code)
    assertEquals("str(-1).split(\" \")",enumerator.next().code)
    assertEquals("{var: var for var in str(0)}",enumerator.next().code)
    assertEquals("{var: var for var in str(1)}",enumerator.next().code)
    assertEquals("{var: var for var in str(-1)}",enumerator.next().code)
    assertEquals("0 - len(s)",enumerator.next().code)
    assertEquals("1 - len(s)",enumerator.next().code)
    assertEquals("-1 - len(s)",enumerator.next().code)
    assertEquals("\" \" + \" \" + \" \"",enumerator.next().code)
    assertEquals("\" \" + \" \" + s",enumerator.next().code)
    assertEquals("\" \" + s + \" \"",enumerator.next().code)
    assertEquals("\" \" + s + s",enumerator.next().code)
    assertEquals("\" \" + s[0]",enumerator.next().code)
    assertEquals("\" \" + s[1]",enumerator.next().code)
    assertEquals("\" \" + s[::-1]",enumerator.next().code)
    assertEquals("\" \" + str(len(s))",enumerator.next().code)
    assertEquals("s + \" \" + \" \"",enumerator.next().code)
    assertEquals("s + \" \" + s",enumerator.next().code)
    assertEquals("s + s + \" \"",enumerator.next().code)
    assertEquals("s + s + s",enumerator.next().code)
    assertEquals("s + s[0]",enumerator.next().code)
    assertEquals("s + s[1]",enumerator.next().code)
    assertEquals("s + s[::-1]",enumerator.next().code)
    assertEquals("s + str(len(s))",enumerator.next().code)
    assertEquals("s[0] + \" \"",enumerator.next().code)
    assertEquals("s[0] + s",enumerator.next().code)
    assertEquals("s[1] + \" \"",enumerator.next().code)
    assertEquals("s[1] + s",enumerator.next().code)
    assertEquals("s[::-1] + \" \"",enumerator.next().code)
    assertEquals("s[::-1] + s",enumerator.next().code)
    assertEquals("str(len(s)) + \" \"",enumerator.next().code)
    assertEquals("str(len(s)) + s",enumerator.next().code)
    assertEquals("str(0) + str(0)",enumerator.next().code)
    assertEquals("str(0) + str(1)",enumerator.next().code)
    assertEquals("str(0) + str(-1)",enumerator.next().code)
    assertEquals("str(1) + str(0)",enumerator.next().code)
    assertEquals("str(1) + str(1)",enumerator.next().code)
    assertEquals("str(1) + str(-1)",enumerator.next().code)
    assertEquals("str(-1) + str(0)",enumerator.next().code)
    assertEquals("str(-1) + str(1)",enumerator.next().code)
    assertEquals("str(-1) + str(-1)",enumerator.next().code)
    assertEquals("s[len(str(-1))]",enumerator.next().code)
    assertEquals("s[::-1][0]",enumerator.next().code)
    assertEquals("s[::-1][1]",enumerator.next().code)
    assertEquals("s[::len(str(-1))]",enumerator.next().code)
    assertEquals("s[::-1 + -1]",enumerator.next().code)
    assertEquals("(s + s)[::-1]",enumerator.next().code)
    assertEquals("len(\" \" + str(-1))",enumerator.next().code)
    assertEquals("len(s + str(-1))",enumerator.next().code)
    assertEquals("len(s[1:-1])",enumerator.next().code)
    assertEquals("str(len(\" \" + s))",enumerator.next().code)
    assertEquals("str(len(s + s))",enumerator.next().code)
    assertEquals("str(len({var: var for var in s}))",enumerator.next().code)
    assertEquals("str(0 - len(s))",enumerator.next().code)
    assertEquals("str(1 - len(s))",enumerator.next().code)
    assertEquals("str(-1 - len(s))",enumerator.next().code)
    assertEquals("s[1:len(s)]",enumerator.next().code)
    assertEquals("s.split(s[0])",enumerator.next().code)
    assertEquals("s.split(s[1])",enumerator.next().code)
    assertEquals("(\" \" + \" \").split(s)",enumerator.next().code)
    assertEquals("(\" \" + s).split(\" \")",enumerator.next().code)
    assertEquals("(s + \" \").split(s)",enumerator.next().code)
    assertEquals("(s + s).split(\" \")",enumerator.next().code)
    assertEquals("s[0].split(\" \")",enumerator.next().code)
    assertEquals("s[1].split(\" \")",enumerator.next().code)
    assertEquals("s[::-1].split(\" \")",enumerator.next().code)
    assertEquals("str(len(s)).split(\" \")",enumerator.next().code)
    assertEquals("str(-1).split(str(1))",enumerator.next().code)
    assertEquals("{var: var + var for var in \" \"}",enumerator.next().code)

  }
