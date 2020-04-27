import ast._
import jdk.nashorn.internal.runtime.BitVector
import org.junit.Test
import org.scalatestplus.junit.JUnitSuite
import org.junit.Assert._

import scala.collection.BitSet

class ASTNodeTests extends JUnitSuite{
  @Test def stringLiteralNode(): Unit = {
    val stringLiteral: StringNode = new StringLiteral("abc",1)
    assertEquals(1,stringLiteral.values.length)
    assertEquals("abc",stringLiteral.values(0))
    assertEquals(Types.String,stringLiteral.nodeType)
    assertEquals("\"abc\"",stringLiteral.code)
    assertEquals(0,stringLiteral.height)
    assertEquals(1,stringLiteral.terms)
    assertTrue(stringLiteral.children.isEmpty)
  }

  @Test def intLiteralNode(): Unit = {
    val intLiteral: IntNode = new IntLiteral(2,2)
    assertEquals(List(2,2),intLiteral.values)
    assertEquals(Types.Int,intLiteral.nodeType)
    assertEquals("2",intLiteral.code)
    assertEquals(0,intLiteral.height)
    assertEquals(1,intLiteral.terms)
    assertTrue(intLiteral.children.isEmpty)
  }

  @Test def boolLiteralNode(): Unit = {
    val boolLiteral: BoolNode = new BoolLiteral(true,1)
    assertEquals(true,boolLiteral.values(0))
    assertEquals(Types.Bool,boolLiteral.nodeType)
    assertEquals("true",boolLiteral.code)
    assertEquals(0,boolLiteral.height)
    assertEquals(1,boolLiteral.terms)
    assertTrue(boolLiteral.children.isEmpty)
  }

  @Test def bvLiteralNode(): Unit = {
    val bvLiteral: BVNode = new BVLiteral(2,1)
    assertEquals(2,bvLiteral.values(0))
    assertEquals(Types.BitVector,bvLiteral.nodeType)
    assertEquals("2",bvLiteral.code)
    assertEquals(0,bvLiteral.height)
    assertEquals(1,bvLiteral.terms)
    assertTrue(bvLiteral.children.isEmpty)
  }

  @Test def variableNode(): Unit = {
    val contexts: List[Map[String,Any]] = List(Map("x" -> "abc"),Map("x" -> "","y" -> "abcd"))
    val stringVariableNode : StringNode = new StringVariable("x", contexts)
    assertEquals(Types.String,stringVariableNode.nodeType)
    assertEquals("x", stringVariableNode.code)
    assertEquals(0,stringVariableNode.height)
    assertEquals(1,stringVariableNode.terms)
    assertEquals("abc",stringVariableNode.values(0))
    assertEquals("",stringVariableNode.values(1))
    assertTrue(stringVariableNode.children.isEmpty)

    val intVariableNode: IntNode = new IntVariable("y",Map("y" -> 2) :: Nil)
    assertEquals(Types.Int,intVariableNode.nodeType)
    assertEquals("y", intVariableNode.code)
    assertEquals(0,intVariableNode.height)
    assertEquals(1,intVariableNode.terms)
    assertEquals(1,intVariableNode.values.length)
    assertEquals(List(2),intVariableNode.values)
    assertTrue(intVariableNode.children.isEmpty)

    val contexts2: List[Map[String,Any]] = List(Map("x" -> "abc","z"-> true),Map("x" -> "","y" -> "abcd","z" -> false), Map("z" -> true))
    val boolVariableNode: BoolNode = new BoolVariable("z",contexts2)
    assertEquals(Types.Bool,boolVariableNode.nodeType)
    assertEquals(0,boolVariableNode.height)
    assertEquals(1,boolVariableNode.terms)
    assertEquals(3,boolVariableNode.values.length)
    assertEquals(List(true,false,true),boolVariableNode.values)
    assertTrue(boolVariableNode.children.isEmpty)
  }

  @Test def stringConcatNode: Unit = {
    val lhs = new StringLiteral("abc",2)
    val rhs = new StringVariable("x",Map("x" -> "123") :: Map("x" -> "456") :: Nil)
    val strConcat: StringNode = new StringConcat(lhs,rhs)
    assertEquals(Types.String, strConcat.nodeType)
    assertEquals(1, strConcat.height)
    assertEquals(3, strConcat.terms)
    assertEquals("(str.++ \"abc\" x)", strConcat.code)
    assertEquals(List("abc123","abc456"),strConcat.values)
    assertEquals(List(lhs,rhs), strConcat.children)
  }

  @Test def stringReplaceNode: Unit = {
    val arg0 = new StringVariable("x",Map("x" -> "12312") :: Map("x" -> "456") :: Nil)
    val arg1 = new StringLiteral("12",2)
    val arg2 = new StringLiteral("2", 2)
    val strReplace: StringNode = new StringReplace(arg0, arg1, arg2)
    assertEquals(Types.String,strReplace.nodeType)
    assertEquals(1,strReplace.height)
    assertEquals(4, strReplace.terms)
    assertEquals("(str.replace x \"12\" \"2\")",strReplace.code)
    assertEquals(List("2312","456"),strReplace.values)
    assertEquals(List(arg0,arg1,arg2),strReplace.children)
  }

  @Test def stringAtNode: Unit = {
    val lhs = new StringVariable("str",List(Map("str" -> "abc"),Map("str" -> "abcd")))
    val rhs = new IntLiteral(1,2)
    val strAt: StringNode = new StringAt(lhs,rhs)
    assertEquals(Types.String,strAt.nodeType)
    assertEquals(1,strAt.height)
    assertEquals(3, strAt.terms)
    assertEquals("(str.at str 1)",strAt.code)
    assertEquals(List("b","b"),strAt.values)
    assertEquals(List(lhs,rhs), strAt.children)

    val strAt2 = new StringAt(strAt,rhs)
    assertEquals("(str.at (str.at str 1) 1)",strAt2.code)
    assertEquals(2,strAt2.height)
    assertEquals(5, strAt2.terms)
    assertEquals(List("",""), strAt2.values)
    assertEquals(List(strAt,rhs),strAt2.children)
  }

  @Test def intToStringNode: Unit = {
    val arg = new IntVariable("i", Map("i" -> 1) :: Map("i" -> -1) :: Map("i" -> 0) :: Nil)
    val intToString: StringNode = new IntToString(arg)
    assertEquals(Types.String, intToString.nodeType)
    assertEquals(1, intToString.height)
    assertEquals(2, intToString.terms)
    assertEquals("(int.to.str i)", intToString.code)
    assertEquals(List("1", "", "0"), intToString.values)
    assertEquals(List(arg),intToString.children)
  }

  @Test def stringITENode: Unit = {
    val cond = new BoolVariable("b",Map("b" -> true) :: Map("b" -> false) :: Nil)
    val exp1 = new StringLiteral("true",2)
    val exp2 = new StringLiteral("false",2)
    val ite: StringNode = new StringITE(cond,exp1,exp2)
    assertEquals(Types.String,ite.nodeType)
    assertEquals(1,ite.height)
    assertEquals(4, ite.terms)
    assertEquals("(ite b \"true\" \"false\")", ite.code)
    assertEquals(List("true","false"),ite.values)
    assertEquals(List(cond,exp1,exp2), ite.children)
  }

  @Test def substringNode: Unit = {
    val str = new StringVariable("str", Map("str" -> "a") :: Map("str" -> "ab") :: Map("str" -> "abc"):: Map("str" -> "abcde") :: Nil)
    val from = new IntLiteral(1,4)
    val to = new IntLiteral(3,4)
    val substring : StringNode = new Substring(str,from,to)

    assertEquals(Types.String, substring.nodeType)
    assertEquals(1, substring.height)
    assertEquals(4, substring.terms)
    assertEquals("(str.substr str 1 3)",substring.code)
    //Desired behavior from CVC4/Z3, checked by z3.
    assertEquals(List("","b","bc","bcd"),substring.values)
    assertEquals(List(str,from,to),substring.children)

  }

  @Test def intAddNode: Unit = {
    val lhs = new IntLiteral(1,1)
    val rhs = new IntLiteral(2,1)
    val add :IntNode = new IntAddition(lhs,rhs)
    assertEquals(Types.Int, add.nodeType)
    assertEquals(1, add.height)
    assertEquals(3, add.terms)
    assertEquals("(+ 1 2)", add.code)
    assertEquals(List(3), add.values)
    assertEquals(List(lhs,rhs),add.children)
  }

  @Test def intSubNode: Unit = {
    val lhs = new IntLiteral(1,1)
    val rhs = new IntLiteral(2,1)
    val sub :IntNode = new IntSubtraction(lhs,rhs)
    assertEquals(Types.Int, sub.nodeType)
    assertEquals(1, sub.height)
    assertEquals(3, sub.terms)
    assertEquals("(- 1 2)", sub.code)
    assertEquals(List(-1), sub.values)
    assertEquals(List(lhs,rhs),sub.children)
  }

  @Test def strelnNode: Unit = {
    val str = new StringVariable("s", Map("s" -> "") :: Map("s" -> " ") :: Nil)
    val strlen: IntNode = new StringLength(str)
    assertEquals(Types.Int, strlen.nodeType)
    assertEquals(1,strlen.height)
    assertEquals(2,strlen.terms)
    assertEquals("(str.len s)", strlen.code)
    assertEquals(List(0,1), strlen.values)
    assertEquals(List(str),strlen.children)
  }

  @Test def stringToIntNode: Unit = {
    val str = new StringLiteral("88",1)
    val strToInt: IntNode = new StringToInt(str)

    assertEquals(Types.Int,strToInt.nodeType)
    assertEquals(1, strToInt.height)
    assertEquals(2, strToInt.terms)
    assertEquals("(str.to.int \"88\")",strToInt.code)
    assertEquals(List(88), strToInt.values)
    assertEquals(List(str),strToInt.children)

    val str2 = new StringLiteral("a",1)
    val strToInt2: IntNode = new StringToInt(str2)

    assertEquals(Types.Int,strToInt2.nodeType)
    assertEquals(1, strToInt2.height)
    assertEquals(2, strToInt2.terms)
    assertEquals("(str.to.int \"a\")",strToInt2.code)
    assertEquals(List(-1), strToInt2.values)
    assertEquals(List(str2),strToInt2.children)
  }

  @Test def intITENode: Unit = {
    val cond = new BoolVariable("b",Map("b" -> true) :: Map("b" -> false) :: Nil)
    val exp1 = new IntLiteral(5,2)
    val exp2 = new IntLiteral(-10,2)
    val ite: IntNode = new IntITE(cond,exp1,exp2)
    assertEquals(Types.Int,ite.nodeType)
    assertEquals(1,ite.height)
    assertEquals(4,ite.terms)
    assertEquals("(ite b 5 -10)", ite.code)
    assertEquals(List(5,-10),ite.values)
    assertEquals(List(cond,exp1,exp2), ite.children)
  }

  @Test def indexOfNode: Unit = {
    val arg0 = new StringLiteral("abcd",3)
    val arg1 = new StringVariable("s", Map("s" -> "a") :: Map("s" -> "cd") :: Map("s" -> "def") :: Nil)
    val arg2 = new IntLiteral(1,3)
    val indexOf : IntNode = new IndexOf(arg0,arg1,arg2)

    assertEquals(Types.Int,indexOf.nodeType)
    assertEquals(1,indexOf.height)
    assertEquals(4, indexOf.terms)
    assertEquals("(str.indexof \"abcd\" s 1)",indexOf.code)
    assertEquals(List(-1,2,-1),indexOf.values)
    assertEquals(List(arg0,arg1,arg2), indexOf.children)
  }

  @Test def lteNode: Unit = {
    val lhs = new IntLiteral(1,1)
    val rhs = new IntLiteral(1,1)
    val lte :BoolNode = new IntLessThanEq(lhs,rhs)
    assertEquals(Types.Bool, lte.nodeType)
    assertEquals(1, lte.height)
    assertEquals(3, lte.terms)
    assertEquals("(<= 1 1)", lte.code)
    assertEquals(List(true), lte.values)
    assertEquals(List(lhs,rhs), lte.children)
  }

  @Test def eqNode: Unit = {
    val ctxs = Map("i" -> 5, "j" -> 6) :: Map("i" -> 5, "j" -> 5) :: Nil
    val lhs = new IntVariable("i",ctxs)
    val rhs = new IntVariable("j",ctxs)
    val eq: BoolNode = new IntEquals(lhs,rhs)
    assertEquals(Types.Bool,eq.nodeType)
    assertEquals(1,eq.height)
    assertEquals(3, eq.terms)
    assertEquals("(= i j)", eq.code)
    assertEquals(List(false,true),eq.values)
    assertEquals(List(lhs,rhs),eq.children)
  }

  @Test def prefixOfNode: Unit = {
    val lhs = new StringLiteral("abc",2)
    val rhs = new StringVariable("x", Map("x" -> "ab"):: Map("x" -> "c") :: Nil)
    //CVC4 has (str.prefixof possible_prefix full_string), so we will too.
    val prefixOf: BoolNode = new PrefixOf(rhs,lhs)
    assertEquals(Types.Bool,prefixOf.nodeType)
    assertEquals(1,prefixOf.height)
    assertEquals(3, prefixOf.terms)
    assertEquals("(str.prefixof x \"abc\")", prefixOf.code)
    assertEquals(List(true,false),prefixOf.values)
    assertEquals(List(rhs,lhs),prefixOf.children)
  }

  @Test def suffixOfNode: Unit = {
    val lhs = new StringLiteral("abc",2)
    val rhs = new StringVariable("x", Map("x" -> "ab"):: Map("x" -> "c") :: Nil)
    //CVC4 has (str.prefixof possible_prefix full_string), so we will too.
    val suffixOf: BoolNode = new SuffixOf(rhs,lhs)
    assertEquals(Types.Bool,suffixOf.nodeType)
    assertEquals(1,suffixOf.height)
    assertEquals(3, suffixOf.terms)
    assertEquals("(str.suffixof x \"abc\")", suffixOf.code)
    assertEquals(List(false,true),suffixOf.values)
    assertEquals(List(rhs, lhs),suffixOf.children)
  }

  @Test def strContains: Unit = {
    val lhs = new StringLiteral("abc",2)
    val rhs = new StringVariable("x", Map("x" -> "d"):: Map("x" -> "c") :: Nil)
    val contains: BoolNode = new Contains(lhs,rhs)
    assertEquals(Types.Bool,contains.nodeType)
    assertEquals(1,contains.height)
    assertEquals(3, contains.terms)
    assertEquals("(str.contains \"abc\" x)", contains.code)
    assertEquals(List(false,true),contains.values)
    assertEquals(List(lhs,rhs),contains.children)
    //second iteration
    assertEquals(List(lhs,rhs),contains.children)
  }

  @Test def bvAndNode: Unit = {
    val lhs = new BVLiteral(23,1)
    val rhs = new BVLiteral(8,1)
    val bvAndNode: BVNode = new BVAnd(lhs,rhs)
    assertEquals(Types.BitVector, bvAndNode.nodeType)
    assertEquals(1, bvAndNode.height)
    assertEquals(3, bvAndNode.terms)
    assertEquals("(bv.and 23 8)", bvAndNode.code)
    assertEquals(List(0),bvAndNode.values)
    assertEquals(List(lhs,rhs), bvAndNode.children)
  }

  @Test def bvOrNode: Unit = {
    val lhs = new BVLiteral(23,1)
    val rhs = new BVLiteral(8,1)
    val bvOrNode: BVNode = new BVOr(lhs,rhs)
    assertEquals(Types.BitVector, bvOrNode.nodeType)
    assertEquals(1, bvOrNode.height)
    assertEquals(3, bvOrNode.terms)
    assertEquals("(bv.or 23 8)", bvOrNode.code)
    assertEquals(List(31),bvOrNode.values)
    assertEquals(List(lhs,rhs), bvOrNode.children)
  }

  @Test def bvXOrNode: Unit = {
    val lhs = new BVLiteral(23,1)
    val rhs = new BVLiteral(8,1)
    val bvXOrNode: BVNode = new BVXor(lhs,rhs)
    assertEquals(Types.BitVector, bvXOrNode.nodeType)
    assertEquals(1, bvXOrNode.height)
    assertEquals(3, bvXOrNode.terms)
    assertEquals("(bv.xor 23 8)", bvXOrNode.code)
    assertEquals(List(31),bvXOrNode.values)
    assertEquals(List(lhs,rhs), bvXOrNode.children)
  }

  @Test def bvComplement: Unit = {
    val arg = new BVLiteral(23,1)
    val bvComplement: BVNode = new BVNot(arg)
    assertEquals(Types.BitVector, bvComplement.nodeType)
    assertEquals(1, bvComplement.height)
    assertEquals(2, bvComplement.terms)
    assertEquals("(bv.not 23)", bvComplement.code)
    assertEquals(List(-24),bvComplement.values)
    assertEquals(List(arg), bvComplement.children)
  }

  @Test def bvShiftLeft: Unit = {
    val lhs = new BVLiteral(23,1)
    val rhs = new BVLiteral(8,1)
    val one = new BVLiteral(value = 1, numContexts = 1)
    val zero = new BVLiteral(value = 0, numContexts = 1)
    val signedOne = new BVLiteral(value = -1, numContexts = 1)
    val bvShlNode: BVNode = new BVShiftLeft(lhs,rhs)
    val bvSimple0: BVNode = new BVShiftLeft(one,one)
    val bvSimple1: BVNode = new BVShiftLeft(one,zero)
    val bvSigned0: BVNode = new BVShiftLeft(one,signedOne)
    assertEquals(Types.BitVector, bvShlNode.nodeType)
    assertEquals(1, bvShlNode.height)
    assertEquals(3, bvShlNode.terms)
    assertEquals("(bv.shl 23 8)", bvShlNode.code)
    assertEquals(List(5888),bvShlNode.values)
    //assertEquals(List(11),bvSigned0.values)
    assertEquals(List(2),bvSimple0.values)
    assertEquals(List(1),bvSimple1.values)
    assertEquals(List(lhs,rhs), bvShlNode.children)
  }

  @Test def includesVarWithName: Unit = {
    val variable = new IntVariable("x",Map("x" -> 2) :: Nil)
    assertTrue(variable.includes("x"))
    assertFalse(variable.includes("y"))

    val expr = new IntAddition(new IntLiteral(2,2), new IntLiteral(4,2))
    assertFalse(expr.includes("x"))
    assertFalse(expr.includes("y"))

    assertTrue(new IntSubtraction(new IntLiteral(1,1),variable).includes("x"))
    assertFalse(new IntSubtraction(new IntLiteral(1,1),variable).includes("y"))
    assertTrue(new IntSubtraction(variable,new IntLiteral(1,1)).includes("x"))
    assertFalse(new IntSubtraction(variable,new IntLiteral(1,1)).includes("z"))
    assertFalse(new StringReplace(new StringLiteral("a",1),new StringLiteral("a",1),new StringLiteral("a",1)).includes("x"))
    assertFalse(new StringReplace(new StringVariable("a",Map("a"->"")::Nil),new StringLiteral("a",1),new StringLiteral("a",1)).includes("x"))
    assertTrue(new StringReplace(new StringVariable("a",Map("a"->"")::Nil),new StringLiteral("a",1),new StringLiteral("a",1)).includes("a"))
    assertFalse(new StringReplace(new StringLiteral("a",1),new StringVariable("a",Map("a"->"")::Nil),new StringLiteral("a",1)).includes("x"))
    assertTrue(new StringReplace(new StringLiteral("a",1),new StringVariable("a",Map("a"->"")::Nil),new StringLiteral("a",1)).includes("a"))
    assertFalse(new StringReplace(new StringLiteral("a",1),new StringLiteral("a",1),new StringVariable("a",Map("a"->"")::Nil)).includes("x"))
    assertTrue(new StringReplace(new StringLiteral("a",1),new StringLiteral("a",1),new StringVariable("a",Map("a"->"")::Nil)).includes("a"))
    assertTrue(new IntToString(variable).includes("x"))
    assertFalse(new IntToString(variable).includes("a"))
  }
}
