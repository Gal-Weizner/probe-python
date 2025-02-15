package vocab

import java.io.FileOutputStream
import ast.Types.Types
import ast._
import enumeration.{InputsValuesManager, PyEnumerator, PyProbEnumerator}
import sygus.{ExamplePredicate, ExcludePredicate, Predicates, RetainPredicate, UsesVariablesPredicate}
import trace.DebugPrints

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class ListCompVocabMaker(inputListType: Types, outputListType: Types, size: Boolean,
                                  val predicates_t: Predicates) extends VocabMaker with Iterator[ASTNode]
{

  var size_log = new FileOutputStream("output.txt", true)

  override val arity: Int = 2
  def apply(children: List[ASTNode], predicates: Predicates): ASTNode = null

  var listIter: Iterator[ASTNode] = _
  var mapVocab: VocabFactory = _
  var predicates: Predicates = _

  var costLevel: Int = _
  var enumerator: Iterator[ASTNode] = _
  var currList: ASTNode = _
  var childHeight: Int = _
  var varName: String = _
  var nextProg: Option[ASTNode] = None
  var miniBank: mutable.Map[(Class[_], ASTNode), mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]] = _
  var mainBank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]] = _

  assert(inputListType.equals(Types.PyInt) || inputListType.equals(Types.PyString),
    s"List comprehension input type not supported: $inputListType")

  assert(outputListType.equals(Types.PyInt) || outputListType.equals(Types.PyString),
    s"List comprehension output type not supported: $inputListType")

  def makeNode(lst: ASTNode, map: ASTNode) : ASTNode

  override def init(programs: List[ASTNode], predicates: Predicates, vocabFactory: VocabFactory, height: Int) : Iterator[ASTNode] = {
    this.listIter = programs.filter(n => n.nodeType.equals(Types.listOf(this.inputListType))).iterator

    this.childHeight = height - 1
    this.varName = "var"
    this.predicates = predicates_t

    // Make sure the name is unique
    // TODO We need a nicer way to generate this
    while (predicates.predicates.take(predicates.num_of_examples).head.
      asInstanceOf[ExamplePredicate].context.contains(this.varName)) this.varName = "_" + this.varName

    // Filter the vocabs for the map function
    // TODO There has to be a more efficient way
    val newVarVocab = this.inputListType match {
      case Types.PyString => new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringVariable]
        override val head: String = ""
        //override val predicates: Predicates = predicates_t

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyStringVariable(varName, predicates)
      }
      case Types.PyInt => new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyIntVariable]
        override val head: String = ""
        //override val predicates: Predicates = predicates_t

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyIntVariable(varName, predicates)
      }
    }

    // We don't support nested list comprehensions
    val vocabs = newVarVocab ::
      vocabFactory.leavesMakers :::
      vocabFactory.nodeMakers.filter(c => c.isInstanceOf[BasicVocabMaker])

    this.mapVocab = VocabFactory(vocabs, predicates)
    this.nextList()
    this
  }

  override def probe_init(vocabFactory: VocabFactory,
                          costLevel: Int, predicates: Predicates,
                          bank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]],
                          nested: Boolean,
                          miniBank: mutable.Map[(Class[_], ASTNode), mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]],
                          mini: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]) : Iterator[ASTNode] = {

    this.costLevel = costLevel - 1
    this.mainBank = bank.map(n => (n._1, n._2.filter(c => !c.includes(this.varName))))
    this.listIter = this.mainBank.dropRight(1).values.flatten.toList
      .filter(n => n.nodeType.equals(Types.listOf(this.inputListType))).iterator
    this.varName = "var"
    this.miniBank = miniBank
    this.predicates = predicates_t
    // Make sure the name is unique
    // TODO We need a nicer way to generate this

    while (predicates.predicates.take(predicates.num_of_examples).head.asInstanceOf[ExamplePredicate].context.contains(this.varName))
      this.varName = "_" + this.varName

    // Filter the vocabs for the map function
    // TODO There has to be a more efficient way
    val newVarVocab = this.inputListType match {
      case Types.PyString => new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringVariable]
        override val head: String = ""
        //override val predicates: Predicates = predicates_t

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyStringVariable(varName, predicates)
      }
      case Types.PyInt => new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyIntVariable]
        override val head: String = ""
        //override val predicates: Predicates = predicates_t

        override def apply(children: List[ASTNode], predicates: Predicates): ASTNode =
          new PyIntVariable(varName, predicates)
      }
    }

    val vocabs = newVarVocab ::
      vocabFactory.leavesMakers :::
      vocabFactory.nodeMakers.filter(c => c.isInstanceOf[BasicVocabMaker]
        && c.returnType.equals(this.outputListType)) // We don't support nested list comprehensions

    this.mapVocab = VocabFactory(vocabs, predicates)
    this.nextList()
    this
  }

  override def hasNext: Boolean = {
    if (this.nextProg.isEmpty && !size) nextProgram()
     else if (this.nextProg.isEmpty && size) nextProgramSize()
    this.nextProg.isDefined
  }

  override def next: ASTNode =
  {
    if (this.nextProg.isEmpty && !size) nextProgram()
    else if (this.nextProg.isEmpty && size) nextProgramSize()
    val rs = this.nextProg.get
    this.nextProg = None
    rs
  }

  private def nextProgram() : Unit =
  {
    if (this.enumerator == null) return

    while (this.nextProg.isEmpty) {
      if (!this.enumerator.hasNext) return

      val next = this.enumerator.next()
      if (next.height > this.childHeight) {
        // We are out of map functions to synthesize for this list.
        if (!this.nextList()) {
          // We are also out of lists!
          return
        }
      } else if (next.nodeType.eq(this.outputListType) && next.includes(this.varName)) {
        // next is a valid program
        val node = this.makeNode(this.currList, next)
        this.nextProg = Some(node)
      }
    }
  }

  private def nextProgramSize() : Unit =
  {
    if (this.enumerator == null) return

    while (this.nextProg.isEmpty) {

      while (!this.enumerator.hasNext) { if (!this.nextList()) return }

      val next = this.enumerator.next()

      if (next.cost < this.costLevel - this.currList.cost) {
        updateMiniBank((this.nodeType, this.currList), next) // TODO: update miniBank with only variable program
      }

      if (next.cost > this.costLevel - this.currList.cost) {
        // We are out of map functions to synthesize for this list.
        if (!this.nextList()) {
          // We are also out of lists!
          return
        }
      } else if (next.nodeType.eq(this.outputListType) && next.includes(this.varName)) {
        // next is a valid program
        val node = this.makeNode(this.currList, next)
        this.nextProg = Some(node)
    }
    }
  }

  private def updateMiniBank(key: (Class[_], ASTNode), value: ASTNode): Unit = {
    if (!this.miniBank.contains(key))
      this.miniBank(key) = mutable.Map(value.cost -> ArrayBuffer(value))
    else if (!this.miniBank(key).contains(value.cost))
      this.miniBank(key)(value.cost) = ArrayBuffer(value)
    else
      this.miniBank(key)(value.cost) += value
  }

  private def nextList() : Boolean =
  {
    var done = false

    while (!done && listIter.hasNext) {
      val lst = listIter.next()
      if (lst.values.head.asInstanceOf[List[_]].nonEmpty) {
        this.currList = lst
        val example_predicates = for (((predicate, listVal), idx) <- this.predicates.getExamplePredicates().zip(lst.values.take(this.predicates.num_of_examples)).zipWithIndex;
                                      elem <- if (listVal.asInstanceOf[Iterable[Any]].isEmpty) List("") else listVal.asInstanceOf[Iterable[Any]]) yield {
          predicate.updatePredicate(varName, elem.toString, idx)
        }
        val new_num_examples = example_predicates.length
        val new_non_example_predicates_list = for ((pred, idx) <- predicates.getNonExamplePredicates().zipWithIndex) yield pred match {
          case predicate: RetainPredicate => predicate.clonePredicate(idx + new_num_examples)
          case predicate: ExcludePredicate => predicate.clonePredicate(idx + new_num_examples)
          case predicate: UsesVariablesPredicate => new UsesVariablesPredicate(idx + new_num_examples)
        }
        val new_predicates = example_predicates ++ new_non_example_predicates_list
        val newPredicatesClass = new Predicates(new_predicates, example_predicates.length)
        val oeValuesManager = new InputsValuesManager()
        this.enumerator = if (!size) {
       new PyEnumerator(this.mapVocab, oeValuesManager, newPredicatesClass)
      } else {

//          Contexts.contextLen = null //TODO: If context changes, recompute the values
//          Contexts.contexts = null
          val bankCost = this.costLevel - this.currList.cost
          val mainBank = this.mainBank.take(bankCost - 1)

          val miniBank = if (this.miniBank.contains((this.nodeType, this.currList)))
            this.miniBank((this.nodeType, this.currList)).take(bankCost) else null

          val nestedCost = if (this.miniBank.contains((this.nodeType, this.currList)))
            this.miniBank((this.nodeType, this.currList)).keys.last else 0

//          this.mapVocab.predicates.predicates = new_predicates
//          this.mapVocab.predicates.num_of_examples = example_predicates.length
          new PyProbEnumerator(this.mapVocab, oeValuesManager, newPredicatesClass, true,
            nestedCost, mainBank, miniBank)
        }
        done = true
      }
    }
    done

  }
}