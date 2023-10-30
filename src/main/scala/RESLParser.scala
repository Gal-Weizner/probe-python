package sygus

import ast.{StringStringMapCompNode, _}
import org.antlr.v4.runtime.tree.{ParseTree, TerminalNode}
import org.antlr.v4.runtime.{BailErrorStrategy, BufferedTokenStream, CharStreams}
import sygus.{Predicates, Python3BaseVisitor, Python3Lexer, Python3Parser, RetainPredicate, UsesVariablesPredicate}
import sygus.Python3Parser._

import scala.collection.JavaConverters._
import scala.collection.mutable

class RESLParser(val predicates: Predicates, val type_clue: String) extends Python3BaseVisitor[(Int, Types.Types)] {
  val v_map: mutable.Map[(Class[_], List[Int]), Int] = mutable.Map[(Class[_], List[Int]), Int]()
  val leaves_map: mutable.Map[(Class[_], Any), Int] = mutable.Map[(Class[_], Any), Int]()
  var counter: Int = 2

  def parse(code: String): (mutable.Map[(Class[_], List[Int]), Int], mutable.Map[(Class[_], Any), Int]) = {
    val lexer = new Python3Lexer(CharStreams.fromString(code.trim))
    lexer.removeErrorListeners()
    val parser = new Python3Parser(new BufferedTokenStream(lexer))
    parser.removeErrorListeners()
    parser.setErrorHandler(new BailErrorStrategy)
    this.visit(parser.comparison())
    if(leaves_map.size == 1)
      {
        val new_leaves_map = leaves_map.map(key => (key._1, 1))
        (v_map, new_leaves_map)
      }
    else
      {
        val max_counter_pair = v_map.find{case (key, value) => value == counter - 1}.get
        v_map.remove(max_counter_pair._1)
        v_map += max_counter_pair._1 -> 1
        (v_map, leaves_map)
      }
  }



  override def visitComparison(ctx: ComparisonContext): (Int, Types.Types) = {
    if (ctx.getChildCount == 1) {
      this.visitChildren(ctx)
    }
    else {
      assert(ctx.getChildCount == 3)
      val lhs_counter = this.visit(ctx.getChild(0))
      val rhs_counter = this.visit(ctx.getChild(2))
      val node_class: Class[_] = ctx.comp_op(0).getText match {
        case ">" => PyGreaterThan.getClass
        case "<=" => PyLessThanEq.getClass
        case "in" => PyContains.getClass
      }
      if (!v_map.contains((node_class, List(lhs_counter._1, rhs_counter._1)))) {
        v_map += (node_class, List(lhs_counter._1, rhs_counter._1)) -> counter
        this.counter = counter + 1
      }
      (v_map((node_class, List(lhs_counter._1, rhs_counter._1))), Types.PyInt)
    }
  }

  def arithmetic(lhs: (Int, Types.Types), rhs: (Int, Types.Types), op: String): (Int, Types.Types) = {
    val op_class: Class[_] = op match {
      case "+" =>
        lhs._2 match {
          case Types.PyInt => PyIntAddition.getClass
          case Types.PyString => PyStringConcat.getClass
        }
      case "-" => PyIntSubtraction.getClass
      case "*" => PyIntMultiply.getClass
      case "//" => PyIntDivision.getClass
    }

    if (!v_map.contains((op_class, List(lhs._1, rhs._1)))) {
      v_map += (op_class, List(lhs._1, rhs._1)) -> counter
      this.counter = counter + 1
      (v_map.get((op_class, List(lhs._1, rhs._1))), op_class)
    }
    lhs._2 match {
      case Types.PyInt => (v_map((op_class, List(lhs._1, rhs._1))), lhs._2)
      case Types.PyString => (v_map((op_class, List(lhs._1, rhs._1))), lhs._2)
    }
  }

  def arith_expression_handler(children_list: mutable.Buffer[ParseTree]): (Int, Types.Types) = {
    if (children_list.length > 1) {
      val op_index = if (children_list(children_list.length / 2).isInstanceOf[TerminalNode])
        children_list.length / 2
      else children_list.length / 2 - 1
      assert(children_list(op_index).isInstanceOf[TerminalNode])
      val op = children_list(op_index).getText
      val lhs = arith_expression_handler(children_list.take(op_index))
      val rhs = arith_expression_handler(children_list.takeRight(children_list.length - op_index - 1))
      arithmetic(lhs, rhs, op)
    }
    else {
      this.visit(children_list.head)
    }
  }

  override def visitArith_expr(ctx: Arith_exprContext): (Int, Types.Types) = {
    arith_expression_handler(ctx.children.asScala)
  }

  override def visitTerm(ctx: TermContext): (Int, Types.Types) = {
    arith_expression_handler(ctx.children.asScala)
  }

  override def visitTestlist_comp(ctx: Testlist_compContext): (Int, Types.Types) = {
    val res = this.visitChildren(ctx)
    res
  }

  override def visitDictorsetmaker(ctx: DictorsetmakerContext): (Int, Types.Types) = {
    assert(ctx.comp_for() != null)
    val list = this.visit(ctx.comp_for().or_test())
    val var_name = ctx.comp_for().exprlist().getText
//    val new_example_predicates = for (((example_predicate, listVal), idx) <- predicates.getExamplePredicates().zip(list.values.take(predicates.num_of_examples)).zipWithIndex;
//                                      elem <- listVal.toString.toList) yield {
//      example_predicate.updatePredicate(var_name, elem.toString, idx)
//    }
//    val new_num_examples = new_example_predicates.length
//    val new_non_example_predicates_list = for ((pred, idx) <- predicates.getNonExamplePredicates().zipWithIndex) yield pred match {
//      case predicate: RetainPredicate => predicate.clonePredicate(idx + new_num_examples)
//      case predicate: UsesVariablesPredicate => new UsesVariablesPredicate(idx + new_num_examples)
//    }
//    val new_predicates = new_example_predicates ++ new_non_example_predicates_list
//    val newPredicatesClass = new Predicates(new_predicates, new_num_examples)
//    val new_parser = new RESLParser(newPredicatesClass, type_clue)
    val key = this.visit(ctx.test(0))
    val value = this.visit(ctx.test(1))
    val map_node_type = (list._2, value._2) match {
      case (Types.PyString, Types.PyString) =>
        new StringStringMapCompNode(list.asInstanceOf[PyStringNode], key.asInstanceOf[PyStringNode],
          value.asInstanceOf[PyStringNode], var_name, predicates).getClass
      case (Types.PyString, Types.PyInt) => new StringIntMapCompNode(list.asInstanceOf[PyStringNode], key.asInstanceOf[PyStringNode],
        value.asInstanceOf[PyIntNode], var_name, predicates).getClass
      case (Types.StringList, Types.PyString) => new StringListStringMapCompNode(list.asInstanceOf[ListNode[String]], key.asInstanceOf[PyStringNode],
        value.asInstanceOf[PyStringNode], var_name, predicates).getClass
      case (Types.StringList, Types.PyInt) => new StringListIntMapCompNode(list.asInstanceOf[ListNode[String]], key.asInstanceOf[PyStringNode],
        value.asInstanceOf[PyIntNode], var_name, predicates).getClass
      case (Types.PyInt, Types.PyString) => new IntStringMapCompNode(list.asInstanceOf[ListNode[Int]], key.asInstanceOf[PyIntNode],
        value.asInstanceOf[PyStringNode], var_name, predicates).getClass
      case (Types.PyInt, Types.PyInt) => new IntIntMapCompNode(list.asInstanceOf[ListNode[Int]], key.asInstanceOf[PyIntNode],
        value.asInstanceOf[PyIntNode], var_name, predicates).getClass
    }
    if (!v_map.contains((map_node_type, List(list._1, key._1, value._1)))) {
      v_map += (map_node_type, List(list._1, key._1, value._1)) -> counter
      this.counter = counter + 1
    }
    (v_map((map_node_type, List(list._1, key._1, value._1))),
      list._2)

  }

  def Atom_expression_trailer_handler(atom: AtomContext, trailers: List[TrailerContext]): (Int, Types.Types) = {
    if (trailers.isEmpty) {
      this.visit(atom)
    }
    else {

      trailers.last.getChild(0).getText match {
        case "[" =>
          val tests = if (trailers.last.subscriptlist().subscript(0).test() == null) Nil
          else trailers.last.subscriptlist().subscript(0).test().asScala.map(arg => this.visit(arg))

          val sliceop = if (trailers.last.subscriptlist().subscript(0).sliceop() == null) None
          else Some(this.visit(trailers.last.subscriptlist().subscript(0).sliceop().test()))

          val lhs_tuple = Atom_expression_trailer_handler(atom, trailers.dropRight(1))
          val tests_numbers: List[Int] = tests.map(test => test._1).toList
          (tests.length, sliceop) match {
            case (1, None) =>
              val node_type = lhs_tuple._2 match {
                case ast.Types.PyString =>
                  PyBinarySubstring.getClass
                case ast.Types.StringList => PyStringListLookup.getClass
                case ast.Types.IntList => PyIntListLookup.getClass
                case ast.Types.Map(Types.PyString, Types.Int) => PyMapGet.getClass
              }
              if (!v_map.contains((node_type, List(lhs_tuple._1, tests_numbers.head)))) {
                v_map += (node_type, List(lhs_tuple._1, tests_numbers.head)) -> counter
                this.counter = counter + 1
              }
              (v_map((node_type, List(lhs_tuple._1, tests_numbers.head))),
                lhs_tuple._2)
            case (2, None) =>
              assert(lhs_tuple._2 == ast.Types.PyString)
              val node_type = TernarySubstring.getClass
              if (!v_map.contains((node_type, List(lhs_tuple._1, tests_numbers.head, tests_numbers.last)))) {
                v_map += (node_type, List(lhs_tuple._1, tests_numbers.head, tests_numbers.last)) -> counter
                this.counter = counter + 1
              }
              (v_map((node_type, List(lhs_tuple._1, tests_numbers.head, tests_numbers.last))),
                lhs_tuple._2)
            case (2, Some(slice)) =>
              assert(lhs_tuple._2 == ast.Types.PyString)
              val node_type = QuaternarySubstring.getClass
              if (!v_map.contains((node_type, List(lhs_tuple._1, tests_numbers.head, tests_numbers(1), slice._1)))) {
                v_map += (node_type, List(lhs_tuple._1, tests_numbers.head, tests_numbers(1), slice._1)) -> counter
                this.counter = counter + 1
              }
              (v_map((node_type, List(lhs_tuple._1, tests_numbers.head, tests_numbers.last))),
                lhs_tuple._2)
            case (0, Some(slice)) =>
              val node_type = PyStringStep.getClass
              if (!v_map.contains((node_type, List(lhs_tuple._1, slice._1)))) {
                v_map += (node_type, List(lhs_tuple._1, slice._1)) -> counter
                this.counter = counter + 1
              }
              (v_map((node_type, List(lhs_tuple._1, slice._1))),
                Types.PyString)

          }
        case "(" =>

          val args = if (trailers.last.arglist() == null) Nil
          else trailers.last.arglist().argument().asScala.map(arg => this.visit(arg))
          if (trailers.length == 1) {
            val func_name = atom.NAME().getText
            val node_type = func_name match {
              case "len" => assert(args.length == 1)
                PyLength.getClass
              case "max" => assert(args.length == 1)
                PyMax.getClass
              case "str" => assert(args.length == 1)
                PyIntToString.getClass
              case "int" => assert(args.length == 1)
                PyStringToInt.getClass
              case "min" => assert(args.length == 1)
                PyMin.getClass
              case "sorted" => assert(args.length == 1)
                PySortedStringList.getClass
            }
            if (!v_map.contains((node_type, List(args.head._1)))) {
              v_map += (node_type, List(args.head._1)) -> counter
              this.counter = counter + 1
            }
            (v_map((node_type, List(args.head._1))),
              args.head._2)
          }
          else {
            val lhs = Atom_expression_trailer_handler(atom, trailers.dropRight(2))
            val func_name = trailers.dropRight(1).last.NAME().getText
            val node_type = func_name match {
              case "isalpha" =>
                assert(args.isEmpty)
                PyIsAlpha.getClass
              case "split" => PyStringSplit.getClass
              case "isnumeric" => PyIsNumeric.getClass
              case "upper" => PyStringUpper.getClass
              case "lower" => PyStringLower.getClass
              case "find" => PyFind.getClass
              case "count" => PyCount.getClass
              case "startswith" => PyStartsWith.getClass
              case "endswith" => PyEndsWith.getClass
              case "join" => PyStringJoin.getClass
            }
            args.length match {
              case 0 =>
                if (!v_map.contains((node_type, List(lhs._1)))) {
                  v_map += (node_type, List(args.head._1)) -> counter
                  this.counter = counter + 1
                }
                (v_map((node_type, List(args.head._1))),
                  Types.PyString)
              case 1 =>
                if (!v_map.contains((node_type, List(lhs._1, args.head._1)))) {
                  v_map += (node_type, List(lhs._1, args.head._1)) -> counter
                  this.counter = counter + 1
                }
                (v_map((node_type, List(lhs._1, args.head._1))),
                  args.head._2)
            }
          }
        case "." => ??? /// purposely not implemented, not in grammar
      }
    }
  }

  override def visitAtom_expr(ctx: Atom_exprContext): (Int, Types.Types) = {
    if (ctx.trailer().isEmpty) {
      this.visitChildren(ctx)
    }
    else {
      Atom_expression_trailer_handler(ctx.atom(), ctx.trailer().asScala.toList)
    }
  }
  
  def check_and_add_leaves_map(node_class: Class[_], value: Any): Int = {
    if (!leaves_map.contains(node_class, value))
      {
        leaves_map += (node_class, value) -> counter
      }F
    leaves_map((node_class, value))
  }
  

  override def visitAtom(ctx: AtomContext): (Int, Types.Types) = {
    val strs = ctx.STRING()
      if (!strs.isEmpty) {
        // TODO Is there a more robust way of removing string quotes?
        val value = strs.stream().map(_.getSymbol.getText).map((v1: String) => v1.substring(1, v1.length - 1))
          .reduce("", (t: String, u: String) => t + u)
        (check_and_add_leaves_map(PyStringLiteral.getClass, value),
          PyStringLiteral.getClass.asInstanceOf[Types.Types])
      }
      else if (ctx.TRUE() != null) (check_and_add_leaves_map(PyBoolLiteral.getClass, true), Types.PyBool)
      else if (ctx.FALSE() != null) (check_and_add_leaves_map(PyBoolLiteral.getClass, false),  Types.PyBool)
      else if (ctx.NUMBER() != null) (check_and_add_leaves_map(PyIntLiteral.getClass, ctx.getText.toInt),  Types.PyInt)
      else if (ctx.OPEN_BRACE() != null) {
        // We have a dict or set!
        val thing = ctx.dictorsetmaker()
        if (thing != null) {
          thing.accept(this)
        } else {
          // Empty dictionary
          ???
        }
      }
      else if (ctx.OPEN_BRACK() != null) {
        // We have a dict or set!
        val thing = ctx.testlist_comp()
        if (thing != null) {
          thing.accept(this)
        } else {
          // Empty dictionary
          ???
        }
      }
      else if (ctx.OPEN_PAREN() != null) {
        this.visit(ctx.testlist_comp())
      }
      else {
        ///Assuming it's a variable
        val var_name = ctx.getText
        /// Inferring type from example
        if (predicates.getExamplePredicates().head.context.contains(var_name)) {
          val value = predicates.getExamplePredicates().head.context(var_name)
          val (node_class, value_type) = value match {
            case _: Int => (PyIntVariable.getClass, Types.PyInt)
            case _: Boolean => (PyBoolVariable.getClass, Types.PyBool)
            case _: String => (PyStringVariable.getClass, Types.PyString)
            case l: List[_] =>
              val t = if (l.isEmpty) Types.Int else l.head match {
                case _: Int => Types.Int
                case _: String => Types.PyString
              }
              (ListVariable.getClass, t)
            case m: Map[_, _] =>
              /// check key value types
              val key_t = if (m.isEmpty) Types.Int else m.head._1 match {
                case _: Int => Types.Int
                case _: String => Types.PyString
              }
              val value_t = if (m.isEmpty) Types.Int else m.head._2 match {
                case _: Int => Types.Int
                case _: String => Types.PyString
              }
              (MapVariable.getClass, value_t)
          }
          if (!leaves_map.contains((node_class, var_name))) {
            leaves_map += (node_class, var_name) -> counter
            this.counter = counter + 1
          }
          (leaves_map((node_class, var_name)), value_type)
        }
        else {
          val (node_class, var_type) = type_clue match {
            case "Int" => (PyIntVariable.getClass, Types.PyInt)
            case "String" => (PyBoolVariable.getClass, Types.PyString)
            case "Boolean" => (PyStringVariable.getClass, Types.PyBool)
          }
          if (!leaves_map.contains((node_class, var_name))) {
            leaves_map += (node_class, var_name) -> counter
            this.counter = counter + 1
          }
          (leaves_map((node_class, var_name)), var_type)
        }
      }
    }



  override def visitFactor(ctx: FactorContext): (Int, Types.Types) = {
    if (ctx.MINUS() != null) {
      // Probably a negative number?
      val child = this.visitChildren(ctx)
      val node_type: Class[_] = child match {
        case a if a._2 == PyIntLiteral.getClass.asInstanceOf[Types.Types] => PyIntLiteral.getClass
      }
      if (!v_map.contains((node_type, List(child._1)))) {
        v_map += (node_type, List(child._1)) -> counter
        this.counter = counter + 1
      }
      (v_map((node_type, List(child._1))),
        Types.PyInt)
    } else {
      this.visitChildren(ctx)
    }
  }
}