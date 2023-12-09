package sygus

import ast.{ASTNode, BoolListNode, IntAddition, IntIntMapCompNode, IntListNode, IntNode, IntStringMapCompNode, IntSubtraction, IntToIntListCompNode, IntToStringListCompNode, IterableNode, ListNode, ListVariable, MapNode, MapVariable, PyBinarySubstring, PyBoolLiteral, PyBoolNode, PyBoolVariable, PyContains, PyCount, PyEndsWith, PyFind, PyGreaterThan, PyIntAddition, PyIntDivision, PyIntListLookup, PyIntLiteral, PyIntMultiply, PyIntNode, PyIntSubtraction, PyIntToString, PyIntVariable, PyIsAlpha, PyIsNumeric, PyLength, PyLessThanEq, PyMapGet, PyMax, PyMin, PySortedStringList, PyStartsWith, PyStringConcat, PyStringJoin, PyStringListLookup, PyStringLiteral, PyStringLower, PyStringNode, PyStringSplit, PyStringStep, PyStringToInt, PyStringUpper, PyStringVariable, QuaternarySubstring, StringIntMapCompNode, StringListIntMapCompNode, StringListNode, StringListStringMapCompNode, StringLiteral, StringStringMapCompNode, StringToIntListCompNode, StringToStringListCompNode, TernarySubstring, Types}
import org.antlr.v4.runtime.tree.{ParseTree, TerminalNode}
import org.antlr.v4.runtime.{BailErrorStrategy, BufferedTokenStream, CharStreams}
import sygus.Python3Parser._
import ast.Types.{PyString, Types}

import scala.collection.JavaConverters._
import scala.collection.mutable


/// TODO: When we have "var" or inner variable, change the order of children visited and visit first
/// the list/dict in order to infer the type of the var

class ExpressionParser(val predicates: Predicates) extends Python3BaseVisitor[ASTNode] {
  def parse(code: String): ASTNode = {
    val lexer = new Python3Lexer(CharStreams.fromString(code.trim))
    lexer.removeErrorListeners()
    val parser = new Python3Parser(new BufferedTokenStream(lexer))
    parser.removeErrorListeners()
    parser.setErrorHandler(new BailErrorStrategy)
    this.visit(parser.comparison())
  }

  override def visitComparison(ctx: ComparisonContext): ASTNode =
    {
      if (ctx.getChildCount == 1)
        {
          this.visitChildren(ctx)
        }
      else
        {
          assert(ctx.getChildCount == 3)
          val lhs = this.visit(ctx.getChild(0))
          val rhs = this.visit(ctx.getChild(2))
          ctx.comp_op(0).getText match {
            case ">" => PyGreaterThan(lhs.asInstanceOf[PyIntNode], rhs.asInstanceOf[PyIntNode], predicates)
            case "<=" => PyLessThanEq(lhs.asInstanceOf[PyIntNode], rhs.asInstanceOf[PyIntNode], predicates)
            case "in" => PyContains(lhs.asInstanceOf[PyStringNode], rhs.asInstanceOf[PyStringNode], predicates)
        }
      }
    }

  def arithmetic(lhs: ASTNode, rhs: ASTNode, op: String): ASTNode = {
    op match {
      case "+" =>
        lhs match {
          case i: PyIntNode => PyIntAddition(i, rhs.asInstanceOf[PyIntNode], this.predicates)
          case s: PyStringNode => PyStringConcat(s, rhs.asInstanceOf[PyStringNode], this.predicates)
          case v: PyStringVariable => predicates.getExamplePredicates().head.context(v.name) match {
            case _: Int => PyIntAddition(v.asInstanceOf[PyIntVariable], rhs.asInstanceOf[PyIntNode], this.predicates)
            case _: String => PyStringConcat(v, rhs.asInstanceOf[PyStringNode], this.predicates)
          }
        }
      case "-" => PyIntSubtraction(lhs.asInstanceOf[PyIntNode], rhs.asInstanceOf[PyIntNode], this.predicates)
      case "*" => PyIntMultiply(lhs.asInstanceOf[PyIntNode], rhs.asInstanceOf[PyIntNode], this.predicates)
      case "//" => PyIntDivision(lhs.asInstanceOf[PyIntNode], rhs.asInstanceOf[PyIntNode], this.predicates)
    }

  }

  def arith_expression_handler(children_list: mutable.Buffer[ParseTree]): ASTNode =
  {
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
  override def visitArith_expr(ctx: Arith_exprContext): ASTNode = {
    arith_expression_handler(ctx.children.asScala)
  }

  override def visitTerm(ctx: TermContext): ASTNode = {
    arith_expression_handler(ctx.children.asScala)
  }

  def parse_inner_list(ctx: Testlist_compContext): Unit =
  {
    val n_type = this.visit(ctx.test(0)).nodeType
    val res = ctx.test().asScala.map(child => this.visit(child)).toList
    n_type match {
      case Types.PyInt => res.asInstanceOf[ListNode[PyIntNode]]
      case Types.PyBool => res.asInstanceOf[ListNode[PyBoolNode]]
      case Types.PyString => res.asInstanceOf[ListNode[PyStringNode]]
    }
  }

  override def visitTestlist_comp(ctx: Testlist_compContext): ASTNode =
    {
      if(ctx.comp_for() == null)
        {
          return this.visitChildren(ctx)
        }
      val list = this.visit(ctx.comp_for().or_test())
      val var_name = ctx.comp_for().exprlist().getText
      val new_example_predicates = for (((example_predicate, listVal), idx) <- predicates.getExamplePredicates().zip(list.values.take(predicates.num_of_examples)).zipWithIndex;
                                        elem <- listVal.toString.toList) yield {
        list.asInstanceOf[ListNode[_]].childType match {
          case Types.PyInt | Types.Int => example_predicate.updatePredicate(var_name, 1, idx)
          case Types.PyString | Types.String => example_predicate.updatePredicate(var_name,"s", idx)
          case Types.PyBool | Types.Bool => example_predicate.updatePredicate(var_name, true, idx)
        }
      }
      val new_num_examples = new_example_predicates.length
      val new_non_example_predicates_list = for ((pred, idx) <- predicates.getNonExamplePredicates().zipWithIndex) yield pred match {
        case predicate: RetainPredicate => predicate.clonePredicate(idx + new_num_examples)
        case predicate: UsesVariablesPredicate => new UsesVariablesPredicate(idx + new_num_examples)
      }
      val new_predicates = new_example_predicates ++ new_non_example_predicates_list
      val newPredicatesClass = new Predicates(new_predicates, new_num_examples)
      val new_parser = new ExpressionParser(newPredicatesClass)
      val value = new_parser.visit(ctx.test(0))
      val var_type = list.values.head.asInstanceOf[List[_]].head match
        {
        case _ : Int => Types.PyInt
        case _ : String =>Types.PyString
        case _ : Boolean => Types.PyBool
      }
      (var_type, value.nodeType) match {
        case (Types.Int, Types.PyString) =>
          new StringToStringListCompNode(list.asInstanceOf[ListNode[String]], value.asInstanceOf[PyStringNode],
            var_name, predicates)
        case (Types.PyString, Types.PyInt) =>
          new StringToIntListCompNode(list.asInstanceOf[ListNode[String]],
            value.asInstanceOf[PyIntNode], var_name, predicates)
        case (Types.PyInt, Types.PyString) => new IntToStringListCompNode(list.asInstanceOf[ListNode[Int]],
          value.asInstanceOf[PyStringNode], var_name, predicates)
        case (Types.PyInt, Types.PyInt) => new IntToIntListCompNode(list.asInstanceOf[ListNode[Int]],
          value.asInstanceOf[PyIntNode], var_name, predicates)
        case(Types.PyString, Types.PyString) => new StringToStringListCompNode(list.asInstanceOf[ListNode[String]],
          value.asInstanceOf[PyStringNode], var_name, predicates)
      }
    }
  override def visitDictorsetmaker(ctx: DictorsetmakerContext): ASTNode =
  {
    assert(ctx.comp_for() != null)
    val list = this.visit(ctx.comp_for().or_test())
    val var_name = ctx.comp_for().exprlist().getText
    val new_example_predicates = for (((example_predicate, listVal), idx) <- predicates.getExamplePredicates().zip(list.values.take(predicates.num_of_examples)).zipWithIndex;
      elem <-listVal.toString.toList) yield {
      example_predicate.updatePredicate(var_name, elem.toString, idx)
    }
    val new_num_examples = new_example_predicates.length
    val new_non_example_predicates_list = for((pred, idx) <- predicates.getNonExamplePredicates().zipWithIndex) yield pred match {
      case predicate: RetainPredicate => predicate.clonePredicate(idx + new_num_examples)
      case predicate: UsesVariablesPredicate => new UsesVariablesPredicate(idx + new_num_examples)
    }
    val new_predicates = new_example_predicates ++ new_non_example_predicates_list
    val newPredicatesClass = new Predicates(new_predicates, new_num_examples)
    val new_parser = new ExpressionParser(newPredicatesClass)
    val key = new_parser.visit(ctx.test(0))
    val value = new_parser.visit(ctx.test(1))
    (list.nodeType, value.nodeType) match {
      case (Types.PyString, Types.PyString) =>
        new StringStringMapCompNode(list.asInstanceOf[PyStringNode], key.asInstanceOf[PyStringNode],
          value.asInstanceOf[PyStringNode], var_name, predicates)
      case (Types.PyString, Types.PyInt) => new StringIntMapCompNode(list.asInstanceOf[PyStringNode], key.asInstanceOf[PyStringNode],
        value.asInstanceOf[PyIntNode], var_name, predicates)
      case (Types.StringList, Types.PyString) => new StringListStringMapCompNode(list.asInstanceOf[ListNode[String]], key.asInstanceOf[PyStringNode],
        value.asInstanceOf[PyStringNode], var_name, predicates)
      case (Types.StringList, Types.PyInt) => new StringListIntMapCompNode(list.asInstanceOf[ListNode[String]], key.asInstanceOf[PyStringNode],
        value.asInstanceOf[PyIntNode], var_name, predicates)
      case (Types.PyInt, Types.PyString) => new IntStringMapCompNode(list.asInstanceOf[ListNode[Int]], key.asInstanceOf[PyIntNode],
        value.asInstanceOf[PyStringNode], var_name, predicates)
      case (Types.PyInt, Types.PyInt) => new IntIntMapCompNode(list.asInstanceOf[ListNode[Int]], key.asInstanceOf[PyIntNode],
        value.asInstanceOf[PyIntNode], var_name, predicates)
    }
  }
  def Atom_expression_trailer_handler(atom: AtomContext, trailers: List[TrailerContext]): ASTNode =
  {
    if (trailers.isEmpty) {
      this.visit(atom)
    }
    else {

      trailers.last.getChild(0).getText match
        {
        case "[" =>
          val tests = if (trailers.last.subscriptlist().subscript(0).test() == null) Nil
          else trailers.last.subscriptlist().subscript(0).test().asScala.map(arg => this.visit(arg))

          val sliceop = if (trailers.last.subscriptlist().subscript(0).sliceop() == null) None
          else Some(this.visit(trailers.last.subscriptlist().subscript(0).sliceop().test()))

          val lhs = Atom_expression_trailer_handler(atom, trailers.dropRight(1))
          (tests.length, sliceop) match
            {
            case (1, None) =>
              lhs.nodeType match {
                case ast.Types.PyString =>
                  PyBinarySubstring(lhs.asInstanceOf[PyStringNode], tests.head.asInstanceOf[PyIntNode], this.predicates)
                case ast.Types.StringList => PyStringListLookup(lhs.asInstanceOf[ListNode[PyStringNode]],tests.head.asInstanceOf[PyIntNode], this.predicates)
                case ast.Types.IntList => PyIntListLookup(lhs.asInstanceOf[ListNode[PyIntNode]],tests.head.asInstanceOf[PyIntNode], this.predicates)
                case ast.Types.Map(Types.PyString, Types.Int) => PyMapGet(lhs.asInstanceOf[MapNode[String,Int]],tests.head.asInstanceOf[PyStringNode], predicates)
              }
            case (2, None) =>
              assert(lhs.nodeType == ast.Types.PyString)
              TernarySubstring(lhs.asInstanceOf[PyStringNode], tests.head.asInstanceOf[PyIntNode], tests.last.asInstanceOf[PyIntNode], predicates )
            case (2, Some(slice)) =>assert(lhs.nodeType == ast.Types.PyString)
              new QuaternarySubstring(lhs.asInstanceOf[PyStringNode], tests(0).asInstanceOf[PyIntNode], tests(1).asInstanceOf[PyIntNode],
                slice.asInstanceOf[PyIntNode], predicates)
            case (0, Some(slice)) =>
              PyStringStep(lhs.asInstanceOf[PyStringNode], slice.asInstanceOf[PyIntNode], predicates)

          }
        case "(" =>

          val args = if (trailers.last.arglist() == null)  Nil
                     else trailers.last.arglist().argument().asScala.map(arg => this.visit(arg))
          if(trailers.length == 1) {
            val func_name = atom.NAME().getText
            func_name match {
              case "len" => assert(args.length == 1)
                PyLength(args.head.asInstanceOf[IterableNode], this.predicates)
              case "max" => assert(args.length == 1)
                PyMax(args.head.asInstanceOf[ListNode[Int]], this.predicates)
              case "str" => assert(args.length == 1)
                PyIntToString(args.head.asInstanceOf[PyIntNode], this.predicates)
              case "int" => assert(args.length == 1)
                PyStringToInt(args.head.asInstanceOf[PyStringNode], this.predicates)
              case "min" => assert(args.length == 1)
                PyMin(args.head.asInstanceOf[ListNode[Int]], this.predicates)
              case "sorted" => assert(args.length == 1)
                PySortedStringList(args.head.asInstanceOf[ListNode[String]], this.predicates)
            }
          }
          else
            {
              val lhs = Atom_expression_trailer_handler(atom, trailers.dropRight(2))
              val func_name = trailers.dropRight(1).last.NAME().getText
              func_name match {
                case "isalpha" =>
                  assert(args.isEmpty)
                  PyIsAlpha(lhs.asInstanceOf[PyStringNode], this.predicates)
                case "split" => PyStringSplit(lhs.asInstanceOf[PyStringNode], args.head.asInstanceOf[PyStringNode], this.predicates)
                case "isnumeric" => PyIsNumeric(lhs.asInstanceOf[PyStringNode], predicates)
                case "upper" => PyStringUpper(lhs.asInstanceOf[PyStringNode], predicates)
                case "lower" => PyStringLower(lhs.asInstanceOf[PyStringNode], predicates)
                case "find" => PyFind(lhs.asInstanceOf[PyStringNode], args.head.asInstanceOf[PyStringNode], predicates)
                case "count" => PyCount(lhs.asInstanceOf[PyStringNode], args.head.asInstanceOf[PyStringNode], predicates)
                case "startswith" => PyStartsWith(lhs.asInstanceOf[PyStringNode], args.head.asInstanceOf[PyStringNode], predicates)
                case "endswith" => PyEndsWith(lhs.asInstanceOf[PyStringNode], args.head.asInstanceOf[PyStringNode], predicates)
                case "join" => PyStringJoin(lhs.asInstanceOf[PyStringNode], args.head.asInstanceOf[ListNode[String]], predicates)
              }
            }
        case "." => ??? /// purposely not implemented, not in grammar
      }
    }
  }

  override def visitAtom_expr(ctx: Atom_exprContext): ASTNode = {
    if (ctx.trailer().isEmpty) {
      this.visitChildren(ctx)
    }
    else
      {
        Atom_expression_trailer_handler(ctx.atom(), ctx.trailer().asScala.toList)
      }
  }
  override def visitAtom(ctx: AtomContext): ASTNode = {
    val strs = ctx.STRING()
    if (!strs.isEmpty) {
      // TODO Is there a more robust way of removing string quotes?
      PyStringLiteral(
        strs.stream()
          .map(_.getSymbol.getText)
          .map((v1: String) => v1.substring(1, v1.length - 1))
          .reduce("", (t: String, u: String) => t + u), predicates.num_of_examples, predicates)
    }
    else if (ctx.TRUE() != null) PyBoolLiteral(true, predicates.num_of_examples, predicates)
    else if (ctx.FALSE() != null) PyBoolLiteral(false, predicates.num_of_examples, predicates)
    else if (ctx.NUMBER() != null) PyIntLiteral(ctx.getText.toInt, predicates.num_of_examples, predicates)
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
    else if(ctx.OPEN_PAREN() != null)
      {
        this.visit(ctx.testlist_comp())
      }
    else {
      ///Assuming it's a variable
      val var_name = ctx.getText
      /// Inferring type from example
      assert(predicates.getExamplePredicates().head.context.contains(var_name))
      val value = predicates.getExamplePredicates().head.context(var_name)
      value match {
        case _: Int => PyIntVariable(var_name, predicates)
        case _: Boolean => PyBoolVariable(var_name, predicates)
        case _: String => PyStringVariable(var_name, predicates)
        case l: List[_] => {
          val t = if (l.isEmpty) Types.Int else l.head match {
            case _: Int => Types.Int
            case _: String => Types.PyString
          }
          ListVariable(var_name, t, predicates)
        }
        case m: Map[_, _] => {
          /// check key value types
          val key_t = if (m.isEmpty) Types.Int else m.head._1 match {
            case _: Int => Types.Int
            case _: String => Types.PyString
          }
          val value_t = if (m.isEmpty) Types.Int else m.head._2 match {
            case _: Int => Types.Int
            case _: String => Types.PyString
          }
          MapVariable(var_name, key_t, value_t, predicates)
        }
      }
    }
  }



  override def visitFactor(ctx: FactorContext): ASTNode = {
    if (ctx.MINUS() != null) {
      // Probably a negative number?
      this.visitChildren(ctx) match {
        case a if a.isInstanceOf[PyIntLiteral] => PyIntLiteral((-1) * a.code.toInt, a.predicates.num_of_examples, a.predicates)
      }
    } else {
      this.visitChildren(ctx)
    }
  }
}