package vocab

import sygus.Predicates
import ast.Types.Types
import ast.{ListNode, PyLength, _}
import net.liftweb.json.JsonAST.{JArray, JObject}
import net.liftweb.json.{JsonParser, prettyRender}
import vocab._

object PySynthesisVocab {
  def makeBasicVocab(): List[VocabMaker] = {
    List(
      new BasicVocabMaker {
        override val arity: Int = 2
        override val childTypes: List[Types] = List(Types.PyInt, Types.PyInt)
        override val returnType: Types = Types.PyBool
        override val nodeType: Class[_ <: ASTNode] = classOf[PyGreaterThan]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyGreaterThan(children.head.asInstanceOf[PyIntNode], children(1).asInstanceOf[PyIntNode], predicates)
      },
      new BasicVocabMaker {
        override val arity: Int = 2
        override val childTypes: List[Types] = List(Types.PyInt, Types.PyInt)
        override val returnType: Types = Types.PyBool
        override val nodeType: Class[_ <: ASTNode] = classOf[PyLessThanEq]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyLessThanEq(children.head.asInstanceOf[PyIntNode], children(1).asInstanceOf[PyIntNode], predicates)
      }
      ,
      new BasicVocabMaker {
        override val arity: Int = 2
        override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringConcat]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyStringConcat(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyStringNode], predicates)
      }
      ,
      new BasicVocabMaker {
        override val arity: Int = 2
        override val childTypes: List[Types] = List(Types.PyString, Types.PyInt)
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyBinarySubstring]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyBinarySubstring(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyIntNode], predicates)
      }
      ,
      new BasicVocabMaker {
        override val arity: Int = 2
        override val childTypes: List[Types] = List(Types.PyString, Types.PyInt)
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringStep]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyStringStep(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyIntNode], predicates)
      }
      ,
      new BasicVocabMaker {
        override val arity: Int = 2
        override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyFind]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyFind(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyStringNode], predicates)
      }
      ,
      new BasicVocabMaker {
        override val arity: Int = 2
        override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
        override val returnType: Types = Types.PyBool
        override val nodeType: Class[_ <: ASTNode] = classOf[PyContains]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyContains(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyStringNode], predicates)
      }
      ,
      new BasicVocabMaker {
        override val arity: Int = 2
        override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyCount]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyCount(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyStringNode], predicates)
      }
      ,
      new BasicVocabMaker {
        override val arity: Int = 2
        override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
        override val returnType: Types = Types.PyBool
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStartsWith]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyStartsWith(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyStringNode], predicates)
      }
      ,
      new BasicVocabMaker {
        override val arity: Int = 2
        override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
        override val returnType: Types = Types.PyBool
        override val nodeType: Class[_ <: ASTNode] = classOf[PyEndsWith]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyEndsWith(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyStringNode], predicates)
      }
      ,
      new BasicVocabMaker {
        override val arity: Int = 1
        override val childTypes: List[Types] = List(Types.Iterable(Types.Any))
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyLength]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyLength(children.head.asInstanceOf[IterableNode], predicates)
      }
      ,
      new BasicVocabMaker {
        override val arity: Int = 1
        override val childTypes: List[Types] = List(Types.IntList)
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyMin]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyMin(children.head.asInstanceOf[ListNode[Int]], predicates)
      }
      ,
      new BasicVocabMaker {
        override val arity: Int = 1
        override val childTypes: List[Types] = List(Types.IntList)
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyMax]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyMax(children.head.asInstanceOf[ListNode[Int]], predicates)
      }
      ,

      new BasicVocabMaker {
        override val arity: Int = 1
        override val childTypes: List[Types] = List(Types.PyString)
        override val returnType: Types = Types.PyBool
        override val nodeType: Class[_ <: ASTNode] = classOf[PyIsAlpha]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyIsAlpha(children.head.asInstanceOf[PyStringNode], predicates)
      }
      ,

      new BasicVocabMaker {
        override val arity: Int = 1
        override val childTypes: List[Types] = List(Types.PyString)
        override val returnType: Types = Types.PyBool
        override val nodeType: Class[_ <: ASTNode] = classOf[PyIsNumeric]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyIsNumeric(children.head.asInstanceOf[PyStringNode], predicates)
      }
      ,

      new BasicVocabMaker {
        override val arity: Int = 1
        override val childTypes: List[Types] = List(Types.PyString)
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringLower]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyStringLower(children.head.asInstanceOf[PyStringNode], predicates)
      }
      ,
      new BasicVocabMaker {
        override val arity: Int = 1
        override val childTypes: List[Types] = List(Types.PyString)
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringUpper]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyStringUpper(children.head.asInstanceOf[PyStringNode], predicates)
      }
      ,
      new BasicVocabMaker {
        override val arity: Int = 1
        override val childTypes: List[Types] = List(Types.PyString)
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringToInt]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyStringToInt(children.head.asInstanceOf[PyStringNode], predicates)
      }
      ,
      new BasicVocabMaker {
        override val arity: Int = 1
        override val childTypes: List[Types] = List(Types.PyInt)
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyIntToString]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyIntToString(children.head.asInstanceOf[PyIntNode], predicates)
      }
      ,
      new BasicVocabMaker {
        override val arity: Int = 3
        override val childTypes: List[Types] = List(Types.PyString, Types.PyInt, Types.PyInt)
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[TernarySubstring]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new TernarySubstring(
            children.head.asInstanceOf[PyStringNode],
            children(1).asInstanceOf[PyIntNode],
            children(2).asInstanceOf[PyIntNode], predicates)
      },
      new BasicVocabMaker {
        override val arity: Int = 2
        override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
        override val returnType: Types = Types.StringList
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringSplit]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyStringSplit(children.head.asInstanceOf[PyStringNode], children.tail.head.asInstanceOf[PyStringNode], predicates)
      }
      ,
      new BasicVocabMaker {
        override val arity: Int = 2
        override val childTypes: List[Types] = List(Types.PyString, Types.StringList)
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringJoin]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyStringJoin(children.head.asInstanceOf[PyStringNode], children.tail.head.asInstanceOf[ListNode[String]], predicates)
      }
      ,
      new BasicVocabMaker {
        override val arity: Int = 1
        override val childTypes: List[Types] = List(Types.StringList)
        override val returnType: Types = Types.StringList
        override val nodeType: Class[_ <: ASTNode] = classOf[PySortedStringList]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PySortedStringList(children.head.asInstanceOf[ListNode[String]], predicates)
      },
      new BasicVocabMaker {
        override val arity: Int = 2
        override val childTypes: List[Types] = List(Types.Map(Types.PyString, Types.PyInt), Types.PyString)
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyMapGet]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyMapGet(children.head.asInstanceOf[MapNode[String, Int]], children(1).asInstanceOf[PyStringNode], predicates)

      },
      new BasicVocabMaker {
        override val arity: Int = 2
        override val childTypes: List[Types] = List(Types.PyInt, Types.PyInt)
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyIntAddition]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyIntAddition(children.head.asInstanceOf[PyIntNode], children(1).asInstanceOf[PyIntNode], predicates)
      },
      new BasicVocabMaker {
        override val arity: Int = 2
        override val childTypes: List[Types] = List(Types.PyInt, Types.PyInt)
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyIntMultiply]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyIntMultiply(children.head.asInstanceOf[PyIntNode], children(1).asInstanceOf[PyIntNode], predicates)
      },
      new BasicVocabMaker {
        override val arity: Int = 2
        override val childTypes: List[Types] = List(Types.PyInt, Types.PyInt)
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyIntSubtraction]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyIntSubtraction(children.head.asInstanceOf[PyIntNode], children(1).asInstanceOf[PyIntNode], predicates)
      },
      new BasicVocabMaker {
        override val arity: Int = 2
        override val childTypes: List[Types] = List(Types.PyInt, Types.PyInt)
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyIntDivision]
        override val head: String = ""

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyIntDivision(children.head.asInstanceOf[PyIntNode], children(1).asInstanceOf[PyIntNode], predicates)
      }
    )

  }

  def makeComprehensions(predicates: Predicates, size: Boolean): List[VocabMaker] = {
    List(
      new ListCompVocabMaker(Types.PyString, Types.PyString, size, predicates) {
        override val nodeType: Class[_ <: ASTNode] = classOf[StringToStringListCompNode]
        override def makeNode(lst: ASTNode, map: ASTNode): ASTNode =
          new StringToStringListCompNode(
            lst.asInstanceOf[ListNode[String]],
            map.asInstanceOf[PyStringNode],
            this.varName, predicates)

        override val returnType: Types = Types.StringList
        override val childTypes: List[Types] = List(Types.PyString)
        override val head: String = ""
      },
      new ListCompVocabMaker(Types.PyString, Types.PyInt, size, predicates) {
        override val nodeType: Class[_ <: ASTNode] = classOf[StringToIntListCompNode]

        override def makeNode(lst: ASTNode, map: ASTNode): ASTNode =
          new StringToIntListCompNode(
            lst.asInstanceOf[ListNode[String]],
            map.asInstanceOf[PyIntNode],
            this.varName, predicates)

        override val returnType: Types = Types.IntList
        override val childTypes: List[Types] = List(Types.PyString)
        override val head: String = ""
      },
      new ListCompVocabMaker(Types.PyInt, Types.PyString, size, predicates) {
        override val nodeType: Class[_ <: ASTNode] = classOf[IntToStringListCompNode]

        override def makeNode(lst: ASTNode, map: ASTNode): ASTNode =
          new IntToStringListCompNode(
            lst.asInstanceOf[ListNode[Int]],
            map.asInstanceOf[PyStringNode],
            this.varName, predicates)

        override val returnType: Types = Types.StringList
        override val childTypes: List[Types] = List(Types.PyInt)
        override val head: String = ""
      },
      new ListCompVocabMaker(Types.PyInt, Types.PyInt, size, predicates) {
        override val nodeType: Class[_ <: ASTNode] = classOf[IntToIntListCompNode]

        override def makeNode(lst: ASTNode, map: ASTNode): ASTNode =
          new IntToIntListCompNode(
            lst.asInstanceOf[ListNode[Int]],
            map.asInstanceOf[PyIntNode],
            this.varName, predicates)

        override val returnType: Types = Types.IntList
        override val childTypes: List[Types] = List(Types.PyInt)
        override val head: String = ""
      },
      new MapCompVocabMaker(Types.PyString, Types.PyString, size, predicates) {
        override val nodeType: Class[_ <: ASTNode] = classOf[StringStringMapCompNode]

        override def makeNode(lst: ASTNode, key: ASTNode, value: ASTNode): ASTNode =
          new StringStringMapCompNode(lst.asInstanceOf[PyStringNode], key.asInstanceOf[PyStringNode], value.asInstanceOf[PyStringNode],
            this.varName, predicates)

        override val returnType: Types = Types.Unknown
        override val childTypes: List[Types] = List(Types.Unknown)
        override val head: String = ""
      },
      new MapCompVocabMaker(Types.PyString, Types.PyInt, size, predicates) {
        override val nodeType: Class[_ <: ASTNode] = classOf[StringIntMapCompNode]

        override def makeNode(lst: ASTNode, key: ASTNode, value: ASTNode): ASTNode =
          new StringIntMapCompNode(lst.asInstanceOf[PyStringNode], key.asInstanceOf[PyStringNode], value.asInstanceOf[PyIntNode],
            this.varName, predicates)

        override val returnType: Types = Types.Unknown
        override val childTypes: List[Types] = List(Types.Unknown)
        override val head: String = ""
      },
      new FilteredMapVocabMaker(Types.PyString, Types.PyString, size, predicates) {
        override val nodeType: Class[_ <: ASTNode] = classOf[StringStringFilteredMapNode]

        override def makeNode(map: ASTNode, filter: PyBoolNode): ASTNode =
          new StringStringFilteredMapNode(map.asInstanceOf[StringStringMapNode], filter, this.keyName, predicates)

        override val returnType: Types = Types.Unknown
        override val childTypes: List[Types] = List(Types.Unknown)
        override val head: String = ""
      },
      new FilteredMapVocabMaker(Types.PyString, Types.PyInt, size, predicates) {
        override val nodeType: Class[_ <: ASTNode] = classOf[StringIntFilteredMapNode]

        override def makeNode(map: ASTNode, filter: PyBoolNode): ASTNode =
          new StringIntFilteredMapNode(map.asInstanceOf[MapNode[String, Int]], filter, this.keyName, predicates)

        override val returnType: Types = Types.StringList
        override val childTypes: List[Types] = List(Types.Unknown)
        override val head: String = ""
      },
    )
  }

  def makeVariables(variables: List[(String, Types.Value)]): List[VocabMaker] =
  {
    variables.
      map {
        case (name, Types.PyString) => new BasicVocabMaker {
          override val arity: Int = 0
          override val childTypes: List[Types] = Nil
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStringVariable]
          override val head: String = ""

          override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
            new PyStringVariable(name, predicates)

        }
        case (name, Types.PyInt) => new BasicVocabMaker {
          override val arity: Int = 0
          override val childTypes: List[Types] = Nil
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIntVariable]
          override val head: String = ""

          override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
            new PyIntVariable(name, predicates)
        }
        case (name, Types.PyBool) => new BasicVocabMaker {
          override val arity: Int = 0
          override val childTypes: List[Types] = Nil
          override val returnType: Types = Types.PyBool
          override val nodeType: Class[_ <: ASTNode] = classOf[PyBoolVariable]
          override val head: String = ""

          override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
            new PyBoolVariable(name, predicates)
        }
        case (name, Types.List(childType)) => new BasicVocabMaker {
          override val arity: Int = 0
          override val childTypes: List[Types] = Nil
          override val returnType: Types = Types.List(childType)
          override val nodeType: Class[_ <: ASTNode] = classOf[ListVariable[Any]]
          override val head: String = ""

          override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
            new ListVariable(name, childType, predicates)
        }
        case (name, Types.Map(keyType, valType)) => new BasicVocabMaker {
          override val arity: Int = 0
          override val childTypes: List[Types] = Nil
          override val returnType: Types = Types.Map(keyType, valType)
          override val nodeType: Class[_ <: ASTNode] = classOf[MapVariable[Any, Any]]
          override val head: String = ""

          override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
            new MapVariable(name, keyType, valType, predicates)

        }
        case (name, typ) =>
          assert(assertion = false, s"Input type $typ not supported for input $name")
          null
      }
  }

  def makeLiterals(stringLiterals: List[String], intLiterals: List[Int]): List[VocabMaker] = {

    stringLiterals.map { str =>
      new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringLiteral]
        override val head: String = ""
        //override val predicates: Predicates = predicates_t

        override def apply(children: List[ASTNode], predicates_t: Predicates): ASTNode =
          new PyStringLiteral(str, predicates.num_of_examples, predicates)
      }
    } ++ intLiterals.map { n =>
      new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyIntLiteral]
        override val head: String = ""
        //override val predicates: Predicates = predicates_t

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyIntLiteral(n, predicates.num_of_examples, predicates)
      }
    }
  }
}

