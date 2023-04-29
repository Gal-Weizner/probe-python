package sygus

import ast.{ASTNode, IntAddition, IntNode, IntSubtraction, IterableNode, ListNode, ListVariable, MapVariable, PyBoolLiteral, PyBoolVariable, PyIntAddition, PyIntLiteral, PyIntNode, PyIntSubtraction, PyIntVariable, PyLength, PyMax, PyStringLiteral, PyStringVariable, StringLiteral, Types}
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{BailErrorStrategy, BufferedTokenStream, CharStreams}
import sygus.Python3Parser._

import scala.collection.JavaConverters._

class ExpressionParser(val predicates: Predicates) extends Python3BaseVisitor[ASTNode] {
  def parse(code: String): ASTNode = {
    val lexer = new Python3Lexer(CharStreams.fromString(code.trim))
    lexer.removeErrorListeners()
    val parser = new Python3Parser(new BufferedTokenStream(lexer))
    parser.removeErrorListeners()
    parser.setErrorHandler(new BailErrorStrategy)
    this.visit(parser.expr())
  }

  override def visitArith_expr(ctx: Arith_exprContext): ASTNode = {
    if (ctx.getChildCount == 3) {
      val op = ctx.getChild(1).getText
      val lhs = this.visit(ctx.getChild(0)).asInstanceOf[PyIntNode]
      val rhs = this.visit(ctx.getChild(2)).asInstanceOf[PyIntNode]
      op match {
        case "+" => PyIntAddition(lhs, rhs, this.predicates)
        case "-" => PyIntSubtraction(lhs, rhs, this.predicates)
      }
    }
    else {
      this.visitChildren(ctx)
    }
  }

  override def visitTerm(ctx: TermContext): ASTNode = {
    this.visitChildren(ctx)
  }

  override def visitAtom_expr(ctx: Atom_exprContext): ASTNode = {
    if (ctx.trailer().isEmpty) {
      this.visitChildren(ctx)
    }
    else
      {
        ctx.trailer(0).getChild(0).getText match {
          case "(" =>
            val func_name = ctx.atom().NAME().getText
            val args = ctx.trailer(0).arglist().argument().asScala.map(arg => this.visit(arg))
            func_name match {
              case "len" => assert(args.length == 1)
                PyLength(args.head.asInstanceOf[IterableNode], this.predicates)
              case "max" => assert(args.length == 1)
                PyMax(args.head.asInstanceOf[ListNode[Int]], this.predicates)

            }


          case "[" => this.visitChildren(ctx)
          case "." => this.visitChildren(ctx)
        }

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

  override def visitTestlist_comp(ctx: Testlist_compContext): ASTNode = {
    ???
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