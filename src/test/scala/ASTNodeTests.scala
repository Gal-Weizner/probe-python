//import ast._
//import org.junit.Test
//import org.scalatestplus.junit.JUnitSuite
//import org.junit.Assert._
//import sygus.{ExamplePredicate, Predicates}
//
//import scala.collection.BitSet
//
//class ASTNodeTests extends JUnitSuite {
//  @Test def stringLiteralNode(): Unit = {
//    val input = Map("inp1" -> "'Welcome to USA. usa usa usa usa!'", "inp2" -> "'USA'", "rs" -> "5")
//    val example_predicate = ExamplePredicate(input)
//    val predicates = Predicates(List(example_predicate), 1)
//    val stringLiteral: StringNode = new StringLiteral("abc", 1, predicates)
//    assertEquals(1, stringLiteral.values.length)
//    assertEquals("abc", stringLiteral.values(0))
//    assertEquals(Types.String, stringLiteral.nodeType)
//    assertEquals("\"abc\"", stringLiteral.code)
//    assertEquals(0, stringLiteral.height)
//    assertEquals(1, stringLiteral.terms)
//    assertTrue(stringLiteral.children.isEmpty)
//  }
//
//  @Test def intLiteralNode(): Unit = {
//    val input = Map("inp1" -> "'Welcome to USA. usa usa usa usa!'", "inp2" -> "'USA'", "rs" -> "5")
//    val example_predicate = ExamplePredicate(input)
//    val predicates = Predicates(List(example_predicate), 1)
//    val intLiteral: IntNode = new IntLiteral(2, 2, predicates)
//    assertEquals(List(2, 2), intLiteral.values)
//    assertEquals(Types.Int, intLiteral.nodeType)
//    assertEquals("2", intLiteral.code)
//    assertEquals(0, intLiteral.height)
//    assertEquals(1, intLiteral.terms)
//    assertTrue(intLiteral.children.isEmpty)
//  }
//
//  @Test def boolLiteralNode(): Unit = {
//    val input = Map("inp1" -> "'Welcome to USA. usa usa usa usa!'", "inp2" -> "'USA'", "rs" -> "5")
//    val example_predicate = ExamplePredicate(input)
//    val predicates = Predicates(List(example_predicate), 1)
//    val boolLiteral: BoolNode = new BoolLiteral(true, 1, predicates)
//    assertEquals(true, boolLiteral.values(0))
//    assertEquals(Types.Bool, boolLiteral.nodeType)
//    assertEquals("true", boolLiteral.code)
//    assertEquals(0, boolLiteral.height)
//    assertEquals(1, boolLiteral.terms)
//    assertTrue(boolLiteral.children.isEmpty)
//  }
//
//  @Test def bvLiteralNode(): Unit = {
//    val input = Map("inp1" -> "'Welcome to USA. usa usa usa usa!'", "inp2" -> "'USA'", "rs" -> "5")
//    val example_predicate = ExamplePredicate(input)
//    val predicates = Predicates(List(example_predicate), 1)
//    val bvLiteral: BVNode = new BVLiteral(2, 1, predicates)
//    assertEquals(2, bvLiteral.values(0))
//    assertEquals(Types.BitVec64, bvLiteral.nodeType)
//    assertEquals("#x0000000000000002", bvLiteral.code)
//    assertEquals(0, bvLiteral.height)
//    assertEquals(1, bvLiteral.terms)
//    assertTrue(bvLiteral.children.isEmpty)
//  }
//
//  @Test def variableNode(): Unit = {
//    val string_example_predicate = ExamplePredicate(Map("inp1" -> "'Welcome to USA. usa usa usa usa!'", "inp2" -> "'USA'", "rs" -> "5"))
//    val string_predicates = Predicates(List(string_example_predicate), 1)
//    val stringVariableNode: StringNode = new StringVariable("x", string_predicates)
//    assertEquals(Types.String, stringVariableNode.nodeType)
//    assertEquals("x", stringVariableNode.code)
//    assertEquals(0, stringVariableNode.height)
//    assertEquals(1, stringVariableNode.terms)
//    assertEquals("abc", stringVariableNode.values(0))
//    assertEquals("", stringVariableNode.values(1))
//    assertTrue(stringVariableNode.children.isEmpty)
//
//    val int_example_predicate = ExamplePredicate(Map("y" -> 2))
//    val int_predicates = Predicates(List(int_example_predicate), 1)
//    val intVariableNode: IntNode = new IntVariable("y", int_predicates)
//    assertEquals(Types.Int, intVariableNode.nodeType)
//    assertEquals("y", intVariableNode.code)
//    assertEquals(0, intVariableNode.height)
//    assertEquals(1, intVariableNode.terms)
//    assertEquals(1, intVariableNode.values.length)
//    assertEquals(List(2), intVariableNode.values)
//    assertTrue(intVariableNode.children.isEmpty)
//
//    val bool_example_1 = ExamplePredicate(Map("x" -> "abc", "z" -> true))
//    val bool_example_2 = ExamplePredicate(Map("x" -> "", "y" -> "abcd", "z" -> false))
//    val bool_example_3 = ExamplePredicate(Map("z" -> true))
//
//    val bool_predicates = Predicates(List(bool_example_1, bool_example_2, bool_example_3), 3)
//    val boolVariableNode: BoolNode = new BoolVariable("z", bool_predicates)
//    assertEquals(Types.Bool, boolVariableNode.nodeType)
//    assertEquals(0, boolVariableNode.height)
//    assertEquals(1, boolVariableNode.terms)
//    assertEquals(3, boolVariableNode.values.length)
//    assertEquals(List(true, false, true), boolVariableNode.values)
//    assertTrue(boolVariableNode.children.isEmpty)
//  }
//
//  @Test def stringConcatNode: Unit = {
//    val example1 = ExamplePredicate(Map("x" -> "123"))
//    val example2 = ExamplePredicate(Map("x" -> "456"))
//    val predicates = Predicates(List(example1, example2), 2)
//    val lhs = new StringLiteral("abc", 2, predicates)
//    val rhs = new StringVariable("x", predicates)
//    val strConcat: StringNode = new StringConcat(lhs, rhs, predicates)
//    assertEquals(Types.String, strConcat.nodeType)
//    assertEquals(1, strConcat.height)
//    assertEquals(3, strConcat.terms)
//    assertEquals("(str.++ \"abc\" x)", strConcat.code)
//    assertEquals(List("abc123", "abc456"), strConcat.values)
//    assertEquals(List(lhs, rhs), strConcat.children)
//  }
//
//  @Test def stringReplaceNode: Unit = {
//    val example1 = ExamplePredicate(Map("x" -> "123"))
//    val example2 = ExamplePredicate(Map("x" -> "456"))
//    val predicates = Predicates(List(example1, example2), 2)
//    val arg0 = new StringVariable("x", predicates)
//    val arg1 = new StringLiteral("12", 2, predicates)
//    val arg2 = new StringLiteral("2", 2, predicates)
//    val strReplace: StringNode = new StringReplace(arg0, arg1, arg2, predicates)
//    assertEquals(Types.String, strReplace.nodeType)
//    assertEquals(1, strReplace.height)
//    assertEquals(4, strReplace.terms)
//    assertEquals("(str.replace x \"12\" \"2\")", strReplace.code)
//    assertEquals(List("2312", "456"), strReplace.values)
//    assertEquals(List(arg0, arg1, arg2), strReplace.children)
//  }
//
//  @Test def stringAtNode: Unit = {
//    val example1 = ExamplePredicate(Map("str" -> "abc"))
//    val example2 = ExamplePredicate(Map("str" -> "abcd"))
//    val predicates = Predicates(List(example1, example2), 2)
//    val lhs = new StringVariable("str", predicates)
//    val rhs = new IntLiteral(1, 2, predicates)
//    val strAt: StringNode = new StringAt(lhs, rhs, predicates)
//    assertEquals(Types.String, strAt.nodeType)
//    assertEquals(1, strAt.height)
//    assertEquals(3, strAt.terms)
//    assertEquals("(str.at str 1)", strAt.code)
//    assertEquals(List("b", "b"), strAt.values)
//    assertEquals(List(lhs, rhs), strAt.children)
//
//    val strAt2 = new StringAt(strAt, rhs, predicates)
//    assertEquals("(str.at (str.at str 1) 1)", strAt2.code)
//    assertEquals(2, strAt2.height)
//    assertEquals(5, strAt2.terms)
//    assertEquals(List("", ""), strAt2.values)
//    assertEquals(List(strAt, rhs), strAt2.children)
//  }
//
//  @Test def intToStringNode: Unit = {
//    val example1 = ExamplePredicate(Map("i" -> 1))
//    val example2 = ExamplePredicate(Map("i" -> -1))
//    val example3 = ExamplePredicate(Map("i" -> 0))
//    val predicates = Predicates(List(example1, example2, example3), 3)
//    val arg = new IntVariable("i", predicates)
//    val intToString: StringNode = new IntToString(arg, predicates)
//    assertEquals(Types.String, intToString.nodeType)
//    assertEquals(1, intToString.height)
//    assertEquals(2, intToString.terms)
//    assertEquals("(int.to.str i)", intToString.code)
//    assertEquals(List("1", "", "0"), intToString.values)
//    assertEquals(List(arg), intToString.children)
//  }
//
//    @Test def stringITENode: Unit = {
//      val example1 = ExamplePredicate(Map("b" -> true))
//      val example2 = ExamplePredicate(Map("b" -> false))
//      val predicates = Predicates(List(example1, example2), 2)
//      val cond = new BoolVariable("b",predicates)
//      val exp1 = new StringLiteral("true",2, predicates)
//      val exp2 = new StringLiteral("false",2, predicates)
//      val ite: StringNode = new StringITE(cond,exp1,exp2, predicates)
//      assertEquals(Types.String,ite.nodeType)
//      assertEquals(1,ite.height)
//      assertEquals(4, ite.terms)
//      assertEquals("(ite b \"true\" \"false\")", ite.code)
//      assertEquals(List("true","false"),ite.values)
//      assertEquals(List(cond,exp1,exp2), ite.children)
//    }
//
//    @Test def substringNode: Unit = {
//      val example1 = ExamplePredicate(Map("str" -> "a"))
//      val example2 = ExamplePredicate(Map("str" -> "ab"))
//      val example3 = ExamplePredicate(Map("str" -> "abc"))
//      val example4 = ExamplePredicate(Map("str" -> "abcde"))
//      val predicates = Predicates(List(example1, example2, example3, example4), 4)
//      val str = new StringVariable("str", predicates)
//      val from = new IntLiteral(1,4,predicates)
//      val to = new IntLiteral(3,4, predicates)
//      val substring : StringNode = new Substring(str,from,to, predicates)
//
//      assertEquals(Types.String, substring.nodeType)
//      assertEquals(1, substring.height)
//      assertEquals(4, substring.terms)
//      assertEquals("(str.substr str 1 3)",substring.code)
//      //Desired behavior from CVC4/Z3, checked by z3.
//      assertEquals(List("","b","bc","bcd"),substring.values)
//      assertEquals(List(str,from,to),substring.children)
//
//    }
//
//    @Test def intAddNode: Unit = {
//      val example1 = ExamplePredicate(Map("i" -> 1))
//      val example2 = ExamplePredicate(Map("i" -> 2))
//      val predicates = Predicates(List(example1, example2), 2)
//      val lhs = new IntLiteral(1,1, predicates)
//      val rhs = new IntLiteral(2,1, predicates)
//      val add :IntNode = new IntAddition(lhs,rhs, predicates)
//      assertEquals(Types.Int, add.nodeType)
//      assertEquals(1, add.height)
//      assertEquals(3, add.terms)
//      assertEquals("(+ 1 2)", add.code)
//      assertEquals(List(3), add.values)
//      assertEquals(List(lhs,rhs),add.children)
//    }
//
//    @Test def intSubNode: Unit = {
//      val example1 = ExamplePredicate(Map("i" -> 1))
//      val example2 = ExamplePredicate(Map("i" -> 2))
//      val predicates = Predicates(List(example1, example2), 2)
//      val lhs = new IntLiteral(1,1, predicates)
//      val rhs = new IntLiteral(2,1, predicates)
//      val sub :IntNode = new IntSubtraction(lhs,rhs, predicates)
//      assertEquals(Types.Int, sub.nodeType)
//      assertEquals(1, sub.height)
//      assertEquals(3, sub.terms)
//      assertEquals("(- 1 2)", sub.code)
//      assertEquals(List(-1), sub.values)
//      assertEquals(List(lhs,rhs),sub.children)
//    }
//
//    @Test def strelnNode: Unit = {
//      val example1 = ExamplePredicate(Map("s" -> ""))
//      val example2 = ExamplePredicate(Map("s" -> " "))
//      val predicates = Predicates(List(example1, example2), 2)
//      val str = new StringVariable("s", predicates)
//      val strlen: IntNode = new StringLength(str,predicates)
//      assertEquals(Types.Int, strlen.nodeType)
//      assertEquals(1,strlen.height)
//      assertEquals(2,strlen.terms)
//      assertEquals("(str.len s)", strlen.code)
//      assertEquals(List(0,1), strlen.values)
//      assertEquals(List(str),strlen.children)
//    }
//
//    @Test def stringToIntNode: Unit = {
//      val example1 = ExamplePredicate(Map("88" -> 88))
//      val predicates1 = Predicates(List(example1), 1)
//      val str = new StringLiteral("88",1, predicates1)
//      val strToInt: IntNode = new StringToInt(str, predicates1)
//
//      assertEquals(Types.Int,strToInt.nodeType)
//      assertEquals(1, strToInt.height)
//      assertEquals(2, strToInt.terms)
//      assertEquals("(str.to.int \"88\")",strToInt.code)
//      assertEquals(List(88), strToInt.values)
//      assertEquals(List(str),strToInt.children)
//
//      val example2 = ExamplePredicate(Map("a" -> 97))
//      val predicates2 = Predicates(List(example2), 1)
//      val str2 = new StringLiteral("a",1, predicates2)
//      val strToInt2: IntNode = new StringToInt(str2, predicates2)
//
//      assertEquals(Types.Int,strToInt2.nodeType)
//      assertEquals(1, strToInt2.height)
//      assertEquals(2, strToInt2.terms)
//      assertEquals("(str.to.int \"a\")",strToInt2.code)
//      assertEquals(List(-1), strToInt2.values)
//      assertEquals(List(str2),strToInt2.children)
//    }
//
//    @Test def intITENode: Unit = {
//      val example1 = ExamplePredicate(Map("b" -> true))
//      val example2 = ExamplePredicate(Map("b" -> false))
//      val predicates = Predicates(List(example1, example2), 2)
//      val cond = new BoolVariable("b",predicates)
//      val exp1 = new IntLiteral(5,2, predicates)
//      val exp2 = new IntLiteral(-10,2, predicates)
//      val ite: IntNode = new IntITE(cond,exp1,exp2, predicates)
//      assertEquals(Types.Int,ite.nodeType)
//      assertEquals(1,ite.height)
//      assertEquals(4,ite.terms)
//      assertEquals("(ite b 5 -10)", ite.code)
//      assertEquals(List(5,-10),ite.values)
//      assertEquals(List(cond,exp1,exp2), ite.children)
//    }
//
//    @Test def indexOfNode: Unit = {
//      val example1 = ExamplePredicate(Map("s" -> "a"))
//      val example2 = ExamplePredicate(Map("s" -> "cd"))
//      val example3 = ExamplePredicate(Map("s" -> "def"))
//      val predicates = Predicates(List(example1, example2, example3), 3)
//      val arg0 = new StringLiteral("abcd",3, predicates)
//      val arg1 = new StringVariable("s", predicates)
//      val arg2 = new IntLiteral(1,3, predicates)
//      val indexOf : IntNode = new IndexOf(arg0,arg1,arg2, predicates)
//
//      assertEquals(Types.Int,indexOf.nodeType)
//      assertEquals(1,indexOf.height)
//      assertEquals(4, indexOf.terms)
//      assertEquals("(str.indexof \"abcd\" s 1)",indexOf.code)
//      assertEquals(List(-1,2,-1),indexOf.values)
//      assertEquals(List(arg0,arg1,arg2), indexOf.children)
//    }
//
//    @Test def lteNode: Unit = {
//      val example1 = ExamplePredicate(Map("s" -> 1))
//      val example2 = ExamplePredicate(Map("s" -> 1))
//      val predicates = Predicates(List(example1, example2), 2)
//      val lhs = new IntLiteral(1,1, predicates)
//      val rhs = new IntLiteral(1,1, predicates)
//      val lte :BoolNode = new IntLessThanEq(lhs,rhs, predicates)
//      assertEquals(Types.Bool, lte.nodeType)
//      assertEquals(1, lte.height)
//      assertEquals(3, lte.terms)
//      assertEquals("(<= 1 1)", lte.code)
//      assertEquals(List(true), lte.values)
//      assertEquals(List(lhs,rhs), lte.children)
//    }
//
//    @Test def eqNode: Unit = {
//      val examples = List(ExamplePredicate(Map("i" -> 5, "j" -> 6)), ExamplePredicate(Map("i" -> 5, "j" -> 5)))
//      val predicates = Predicates(examples, 2)
//      val lhs = new IntVariable("i",predicates)
//      val rhs = new IntVariable("j",predicates)
//      val eq: BoolNode = new IntEquals(lhs,rhs, predicates)
//      assertEquals(Types.Bool,eq.nodeType)
//      assertEquals(1,eq.height)
//      assertEquals(3, eq.terms)
//      assertEquals("(= i j)", eq.code)
//      assertEquals(List(false,true),eq.values)
//      assertEquals(List(lhs,rhs),eq.children)
//    }
//
//    @Test def prefixOfNode: Unit = {
//      val examples = List(ExamplePredicate(Map("x" -> "ab")), ExamplePredicate(Map("x" -> "c")))
//      val predicates = Predicates(examples, 2)
//      val lhs = new StringLiteral("abc",2, predicates)
//      val rhs = new StringVariable("x", predicates)
//      //CVC4 has (str.prefixof possible_prefix full_string), so we will too.
//      val prefixOf: BoolNode = new PrefixOf(rhs,lhs, predicates)
//      assertEquals(Types.Bool,prefixOf.nodeType)
//      assertEquals(1,prefixOf.height)
//      assertEquals(3, prefixOf.terms)
//      assertEquals("(str.prefixof x \"abc\")", prefixOf.code)
//      assertEquals(List(true,false),prefixOf.values)
//      assertEquals(List(rhs,lhs),prefixOf.children)
//    }
//
//    @Test def suffixOfNode: Unit = {
//      val examples = List(ExamplePredicate(Map("x" -> "ab")), ExamplePredicate(Map("x" -> "c")))
//      val predicates = Predicates(examples, 2)
//      val lhs = new StringLiteral("abc",2, predicates)
//      val rhs = new StringVariable("x", predicates)
//      //CVC4 has (str.prefixof possible_prefix full_string), so we will too.
//      val suffixOf: BoolNode = new SuffixOf(rhs,lhs, predicates)
//      assertEquals(Types.Bool,suffixOf.nodeType)
//      assertEquals(1,suffixOf.height)
//      assertEquals(3, suffixOf.terms)
//      assertEquals("(str.suffixof x \"abc\")", suffixOf.code)
//      assertEquals(List(false,true),suffixOf.values)
//      assertEquals(List(rhs, lhs),suffixOf.children)
//    }
//
////    @Test def strContains: Unit = {
////      val lhs = new StringLiteral("abc",2)
////      val rhs = new StringVariable("x", Map("x" -> "d"):: Map("x" -> "c") :: Nil)
////      val contains: BoolNode = new Contains(lhs,rhs)
////      assertEquals(Types.Bool,contains.nodeType)
////      assertEquals(1,contains.height)
////      assertEquals(3, contains.terms)
////      assertEquals("(str.contains \"abc\" x)", contains.code)
////      assertEquals(List(false,true),contains.values)
////      assertEquals(List(lhs,rhs),contains.children)
////      //second iteration
////      assertEquals(List(lhs,rhs),contains.children)
////    }
//
//  //  @Test def bvAndNode: Unit = {
//  //    val lhs = new BVLiteral(23,1)
//  //    val rhs = new BVLiteral(8,1)
//  //    val bvAndNode: BVNode = new BVAnd(lhs,rhs)
//  //    assertEquals(Types.BitVec64, bvAndNode.nodeType)
//  //    assertEquals(1, bvAndNode.height)
//  //    assertEquals(3, bvAndNode.terms)
//  //    assertEquals("(bvand #x0000000000000017 #x0000000000000008)", bvAndNode.code)
//  //    assertEquals(List(0),bvAndNode.values)
//  //    assertEquals(List(lhs,rhs), bvAndNode.children)
//  //  }
//  //
//  //  @Test def bvOrNode: Unit = {
//  //    val lhs = new BVLiteral(23,1)
//  //    val rhs = new BVLiteral(8,1)
//  //    val bvOrNode: BVNode = new BVOr(lhs,rhs)
//  //    assertEquals(Types.BitVec64, bvOrNode.nodeType)
//  //    assertEquals(1, bvOrNode.height)
//  //    assertEquals(3, bvOrNode.terms)
//  //    assertEquals("(bvor #x0000000000000017 #x0000000000000008)", bvOrNode.code)
//  //    assertEquals(List(31),bvOrNode.values)
//  //    assertEquals(List(lhs,rhs), bvOrNode.children)
//  //  }
//  //
//  //  @Test def bvXOrNode: Unit = {
//  //    val lhs = new BVLiteral(23,1)
//  //    val rhs = new BVLiteral(8,1)
//  //    val bvXOrNode: BVNode = new BVXor(lhs,rhs)
//  //    assertEquals(Types.BitVec64, bvXOrNode.nodeType)
//  //    assertEquals(1, bvXOrNode.height)
//  //    assertEquals(3, bvXOrNode.terms)
//  //    assertEquals("(bvxor #x0000000000000017 #x0000000000000008)", bvXOrNode.code)
//  //    assertEquals(List(31),bvXOrNode.values)
//  //    assertEquals(List(lhs,rhs), bvXOrNode.children)
//  //  }
//  //
//  //  @Test def bvComplement: Unit = {
//  //    val arg = new BVLiteral(23,1)
//  //    val bvComplement: BVNode = new BVNot(arg)
//  //    assertEquals(Types.BitVec64, bvComplement.nodeType)
//  //    assertEquals(1, bvComplement.height)
//  //    assertEquals(2, bvComplement.terms)
//  //    assertEquals("(bvnot #x0000000000000017)", bvComplement.code)
//  //    assertEquals(List(-24),bvComplement.values)
//  //    assertEquals(List(arg), bvComplement.children)
//  //  }
//  //
//  //  @Test def bvShiftLeft: Unit = {
//  //    val lhs = new BVLiteral(23,1)
//  //    val rhs = new BVLiteral(8,1)
//  //    val one = new BVLiteral(value = 1, numContexts = 1)
//  //    val zero = new BVLiteral(value = 0, numContexts = 1)
//  //    val signedOne = new BVLiteral(value = -1, numContexts = 1)
//  //    val bvShlNode = new BVShiftLeft(lhs,rhs)
//  //    val bvSimple0 = new BVShiftLeft(one,one)
//  //    val bvSimple1 = new BVShiftLeft(one,zero)
//  //    val bvSigned0 = new BVShiftLeft(one,signedOne)
//  //    assertEquals(Types.BitVec64, bvShlNode.nodeType)
//  //    assertEquals(1, bvShlNode.height)
//  //    assertEquals(3, bvShlNode.terms)
//  //    assertEquals("(bvshl #x0000000000000017 #x0000000000000008)", bvShlNode.code)
//  //    assertEquals("(bvshl #x0000000000000001 #x0000000000000001)",bvSimple0.code)
//  //    assertEquals("(bvshl #x0000000000000001 #x0000000000000000)",bvSimple1.code)
//  //    assertEquals("(bvshl #x0000000000000001 #xffffffffffffffff)",bvSigned0.code)
//  //    assertEquals(List(5888),bvShlNode.values)
//  //    assertEquals(List(2),bvSimple0.values)
//  //    assertEquals(List(1),bvSimple1.values)
//  //    assertEquals(List(0),bvSigned0.values)
//  //    assertEquals(List(lhs,rhs), bvShlNode.children)
//  //  }
//  //
//  //  @Test def bvAShiftRight: Unit = {
//  //    val arg = new BVLiteral(2,1)
//  //    val one = new BVLiteral(value = 1, numContexts = 1)
//  //    val signedOne = new BVLiteral(value = -1, numContexts = 1)
//  //    val bvShrNode = new BVShrArithmetic(arg,one)
//  //    val bvShrNodeSigned = new BVShrArithmetic(arg,signedOne)
//  //    val bvShrNodeSigned1 = new BVShrArithmetic(signedOne,signedOne)
//  //    val bvShrNodeSigned2 = new BVShrArithmetic(signedOne,one)
//  //    assertEquals(Types.BitVec64, bvShrNode.nodeType)
//  //    assertEquals(1, bvShrNode.height)
//  //    assertEquals(3, bvShrNode.terms)
//  //    assertEquals("(bvashr #x0000000000000002 #x0000000000000001)", bvShrNode.code)
//  //    assertEquals("(bvashr #x0000000000000002 #xffffffffffffffff)",bvShrNodeSigned.code)
//  //    assertEquals("(bvashr #xffffffffffffffff #xffffffffffffffff)",bvShrNodeSigned1.code)
//  //    assertEquals("(bvashr #xffffffffffffffff #x0000000000000001)",bvShrNodeSigned2.code)
//  //    assertEquals(List(1),bvShrNode.values)
//  //    assertEquals(List(0),bvShrNodeSigned.values)
//  //    assertEquals(List(-1),bvShrNodeSigned1.values)
//  //    assertEquals(List(-1),bvShrNodeSigned2.values)
//  //    assertEquals(List(arg,one), bvShrNode.children)
//  //  }
//  //
//  //  @Test def bvLShiftRight: Unit = {
//  //    val arg = new BVLiteral(2,1)
//  //    val one = new BVLiteral(value = 1, numContexts = 1)
//  //    val signedOne = new BVLiteral(value = -1, numContexts = 1)
//  //    val bvShrNode: BVNode = new BVShrLogical(arg,one)
//  //    val bvShrNodeSigned: BVNode = new BVShrLogical(arg,signedOne)
//  //    assertEquals(Types.BitVec64, bvShrNode.nodeType)
//  //    assertEquals(1, bvShrNode.height)
//  //    assertEquals(3, bvShrNode.terms)
//  //    assertEquals("(bvlshr #x0000000000000002 #x0000000000000001)", bvShrNode.code)
//  //    assertEquals("(bvlshr #x0000000000000002 #xffffffffffffffff)", bvShrNodeSigned.code)
//  //    assertEquals(List(1),bvShrNode.values)
//  //    assertEquals(List(0),bvShrNodeSigned.values)
//  //    assertEquals(List(arg,one), bvShrNode.children)
//  //  }
//  //
//  //  @Test def bvNeg: Unit = {
//  //    val arg1 = new BVLiteral(0,1)
//  //    val arg2 = new BVLiteral(-5,1)
//  //    val arg3 = new BVLiteral(101,1)
//  //
//  //    val neg1 = new BVNeg(arg1)
//  //    val neg2 = new BVNeg(arg2)
//  //    val neg3 = new BVNeg(arg3)
//  //    val neg4 = new BVNeg(new BVLiteral(Long.MinValue,1))
//  //
//  //    assertEquals(1,neg1.height)
//  //    assertEquals(2,neg1.terms)
//  //    assertEquals("(bvneg #x0000000000000000)",neg1.code)
//  //    assertEquals("(bvneg #xfffffffffffffffb)",neg2.code)
//  //    assertEquals("(bvneg #x0000000000000065)",neg3.code)
//  //    assertEquals("(bvneg #x8000000000000000)",neg4.code)
//  //
//  //    assertEquals(List(0),neg1.values)
//  //    assertEquals(List(5),neg2.values)
//  //    assertEquals(List(-101),neg3.values)
//  //    assertEquals(List(Long.MinValue),neg4.values)
//  //  }
//  //
//  //  @Test def bvVarialbe: Unit = {
//  //    val x = new BVVariable("x",Map("x" -> Long.MinValue) :: Nil)
//  //    assertEquals(0,x.height)
//  //    assertEquals(1,x.terms)
//  //    assertEquals("x",x.code)
//  //    assertEquals(List(Long.MinValue),x.values)
//  //  }
//  //
//  //  @Test def bvSub: Unit = {
//  //    val x = new BVVariable("x",Map("x" -> Long.MinValue) :: Map("x" -> 0L) :: Map("x" -> Long.MaxValue) :: Nil)
//  //    val y = new BVVariable("y",Map("y" -> Long.MinValue) :: Map("y" -> Long.MaxValue) :: Map("y" -> 0L) ::  Nil)
//  //    val sub = new BVSub(x,y)
//  //    assertEquals("(bvsub x y)",sub.code)
//  //    assertEquals(List(0,-9223372036854775807L,9223372036854775807L),sub.values)
//  //  }
//  //  @Test def bvSDiv: Unit = {
//  //    val divnode = new BVUDiv(new BVLiteral(-8,1),new BVLiteral(4,1))
//  //    assertEquals(1,divnode.height)
//  //    assertEquals(3,divnode.terms)
//  //    assertEquals("(bvudiv #xfffffffffffffff8 #x0000000000000004)", divnode.code)
//  //    assertEquals(List(0x3ffffffffffffffeL),divnode.values)
//  //  }
//  //  @Test def bvUDiv: Unit = {
//  //    val divnode = new BVSDiv(new BVLiteral(-8,1),new BVLiteral(4,1))
//  //    assertEquals(1,divnode.height)
//  //    assertEquals(3,divnode.terms)
//  //    assertEquals("(bvsdiv #xfffffffffffffff8 #x0000000000000004)", divnode.code)
//  //    assertEquals("18446744073709551614",java.lang.Long.toUnsignedString(divnode.values(0)))
//  //  }
//  //  @Test def bvMul: Unit = {
//  //    val mul = new BVMul(new BVVariable("x",Map("x" -> Long.MinValue) :: Map("x" -> 2L) :: Nil),new BVLiteral(2L,2))
//  //    assertEquals(1,mul.height)
//  //    assertEquals(3,mul.terms)
//  //    assertEquals(Types.BitVec64,mul.nodeType)
//  //    assertEquals("(bvmul x #x0000000000000002)",mul.code)
//  //    assertEquals(List(0,4),mul.values)
//  //  }
//  //
//  //  @Test def bvEquals: Unit = {
//  //    val eq = new BVEquals(new BVLiteral(4,2), new BVVariable("v", Map("v" -> 4L) :: Map("v" -> 0L) :: Nil))
//  //    assertEquals(1,eq.height)
//  //    assertEquals(3,eq.terms)
//  //    assertTrue(eq.isInstanceOf[BoolNode])
//  //    assertEquals(Types.Bool,eq.nodeType)
//  //    assertEquals("(= #x0000000000000004 v)",eq.code)
//  //    assertEquals(List(true,false), eq.values)
//  //  }
//  //
//  //  @Test def logicalAnd: Unit = {
//  //    val ctx = List(Map("x" -> true, "y" -> false), Map("x" -> true, "y" -> true), Map("x" -> false, "y" -> false))
//  //    val land = new LAnd(new BoolVariable("x",ctx), new BoolVariable("y",ctx))
//  //    assertEquals(1, land.height)
//  //    assertEquals(3,land.terms)
//  //    assertEquals(Types.Bool,land.nodeType)
//  //    assertEquals("(and x y)",land.code)
//  //    assertEquals(List(false,true,false),land.values)
//  //  }
//  //
//  //  @Test def logicalOr: Unit = {
//  //    val ctx = List(Map("x" -> true, "y" -> false), Map("x" -> true, "y" -> true), Map("x" -> false, "y" -> false))
//  //    val lor = new LOr(new BoolVariable("x",ctx), new BoolVariable("y",ctx))
//  //    assertEquals(1, lor.height)
//  //    assertEquals(3,lor.terms)
//  //    assertEquals(Types.Bool,lor.nodeType)
//  //    assertEquals("(or x y)",lor.code)
//  //    assertEquals(List(true,true,false),lor.values)
//  //  }
//  //
//  //  @Test def logicalNot: Unit = {
//  //    val ctx = List(Map("x" -> true, "y" -> false), Map("x" -> true, "y" -> true))
//  //    val lnot = new LNot(new BoolVariable("y",ctx))
//  //    assertEquals(1, lnot.height)
//  //    assertEquals(2,lnot.terms)
//  //    assertEquals(Types.Bool,lnot.nodeType)
//  //    assertEquals("(not y)",lnot.code)
//  //    assertEquals(List(true,false),lnot.values)
//  //  }
//  //
//  //  @Test def bvITE: Unit = {
//  //    val ctx = List(Map("b" -> true, "a" -> 2L, "c" -> 8L),Map("b" -> false, "a" -> 3L, "c" -> 9L))
//  //    val ite = new BVITE(new BoolVariable("b",ctx), new BVVariable("a",ctx), new BVVariable("c",ctx))
//  //    assertEquals(1,ite.height)
//  //    assertEquals(4,ite.terms)
//  //    assertEquals(Types.BitVec64, ite.nodeType)
//  //    assertEquals("(ite b a c)", ite.code)
//  //    assertEquals(List(2L,9L), ite.values)
//  //  }
//  //
//  //  @Test def bvSignedRem: Unit = {
//  //    val ctx = List(Map("a" -> 0L, "b" -> 5L),Map("a" -> 2L, "b" -> -5L),Map("a" -> -2L, "b" -> 5L), Map("a" -> Long.MaxValue, "b" -> Long.MinValue), Map("a" -> Long.MinValue, "b" -> Long.MaxValue))
//  //    val srem = new BVSRem(new BVVariable("a",ctx), new BVVariable("b",ctx))
//  //    assertEquals(1,srem.height)
//  //    assertEquals(3, srem.terms)
//  //    assertEquals(Types.BitVec64,srem.nodeType)
//  //    assertEquals("(bvsrem a b)", srem.code)
//  //    assertEquals(List(0L,2L,0xfffffffffffffffeL,9223372036854775807L,0xffffffffffffffffL),srem.values)
//  //  }
//  //  @Test def bvUnsignedRem: Unit = {
//  //    val ctx = List(Map("a" -> 0L, "b" -> 5L),Map("a" -> 2L, "b" -> -5L),Map("a" -> -2L, "b" -> 5L), Map("a" -> Long.MaxValue, "b" -> Long.MinValue), Map("a" -> Long.MinValue, "b" -> Long.MaxValue))
//  //    val srem = new BVURem(new BVVariable("a",ctx), new BVVariable("b",ctx))
//  //    assertEquals(1,srem.height)
//  //    assertEquals(3, srem.terms)
//  //    assertEquals(Types.BitVec64,srem.nodeType)
//  //    assertEquals("(bvurem a b)", srem.code)
//  //    assertEquals(List(0L,2L,4L,9223372036854775807L,1),srem.values)
//  //  }
//  //
//  //  @Test def includesVarWithName: Unit = {
//  //    val variable = new IntVariable("x",Map("x" -> 2) :: Nil)
//  //    assertTrue(variable.includes("x"))
//  //    assertFalse(variable.includes("y"))
//  //
//  //    val expr = new IntAddition(new IntLiteral(2,2), new IntLiteral(4,2))
//  //    assertFalse(expr.includes("x"))
//  //    assertFalse(expr.includes("y"))
//  //
//  //    assertTrue(new IntSubtraction(new IntLiteral(1,1),variable).includes("x"))
//  //    assertFalse(new IntSubtraction(new IntLiteral(1,1),variable).includes("y"))
//  //    assertTrue(new IntSubtraction(variable,new IntLiteral(1,1)).includes("x"))
//  //    assertFalse(new IntSubtraction(variable,new IntLiteral(1,1)).includes("z"))
//  //    assertFalse(new StringReplace(new StringLiteral("a",1),new StringLiteral("a",1),new StringLiteral("a",1)).includes("x"))
//  //    assertFalse(new StringReplace(new StringVariable("a",Map("a"->"")::Nil),new StringLiteral("a",1),new StringLiteral("a",1)).includes("x"))
//  //    assertTrue(new StringReplace(new StringVariable("a",Map("a"->"")::Nil),new StringLiteral("a",1),new StringLiteral("a",1)).includes("a"))
//  //    assertFalse(new StringReplace(new StringLiteral("a",1),new StringVariable("a",Map("a"->"")::Nil),new StringLiteral("a",1)).includes("x"))
//  //    assertTrue(new StringReplace(new StringLiteral("a",1),new StringVariable("a",Map("a"->"")::Nil),new StringLiteral("a",1)).includes("a"))
//  //    assertFalse(new StringReplace(new StringLiteral("a",1),new StringLiteral("a",1),new StringVariable("a",Map("a"->"")::Nil)).includes("x"))
//  //    assertTrue(new StringReplace(new StringLiteral("a",1),new StringLiteral("a",1),new StringVariable("a",Map("a"->"")::Nil)).includes("a"))
//  //    assertTrue(new IntToString(variable).includes("x"))
//  //    assertFalse(new IntToString(variable).includes("a"))
//  //  }
//  //}
//}
