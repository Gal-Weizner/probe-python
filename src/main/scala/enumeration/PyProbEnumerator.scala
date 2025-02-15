package enumeration

import java.io.FileOutputStream
import ast.{ASTNode, IntIntFilteredMapNode, IntIntMapCompNode, IntStringFilteredMapNode, IntStringMapCompNode, IntToIntListCompNode, IntToStringListCompNode, PyMapGet, StringIntFilteredMapNode, StringIntMapCompNode, StringListIntMapCompNode, StringListStringMapCompNode, StringStringFilteredMapNode, StringStringMapCompNode, StringToIntListCompNode, StringToStringListCompNode}
import sygus.{Predicate, Predicates}
import vocab.{VocabFactory, VocabMaker}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class PyProbEnumerator(val vocab: VocabFactory,
                       val oeManager: OEValuesManager,
                       val predicates_t: Predicates,
                       var nested: Boolean,
                       var initCost: Int,
                       outerMainBank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]],
                       outerVars: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]) extends Iterator[ASTNode] {

  override def toString(): String = "enumeration.Enumerator"
  var nextProgram: Option[ASTNode] = None

  override def hasNext: Boolean =
    if (nextProgram.isDefined) {
      true
    } else {
      nextProgram = getNextProgram()
      nextProgram.isDefined
    }

  override def next(): ASTNode = {
    if (nextProgram.isEmpty) {
      nextProgram = getNextProgram()
    }
    val res = nextProgram.get
    nextProgram = None
    res
  }
  var costLevel = initCost
  var currIterator: Iterator[VocabMaker] = _
  var currLevelPrograms: mutable.ArrayBuffer[ASTNode] = mutable.ArrayBuffer()
  var varBank = mutable.Map[(Class[_], ASTNode), mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]]()
  val totalLeaves = vocab.leaves().toList.distinct ++ vocab.nonLeaves().toList.distinct
  var size_log = new FileOutputStream("output.txt", true)

  ProbUpdate.probMap = ProbUpdate.createPyProbMap(vocab)
  ProbUpdate.priors = ProbUpdate.createPyPrior(vocab)
  resetEnumeration()
//  Contexts.contextLen = this.predicates.num_of_examples
//  Contexts.contexts = this.predicates.predicates.take(this.predicates.num_of_examples)

  val mainBank = for ((cost,nodeList) <- outerMainBank) yield (cost,
      for (node <- nodeList if !node.includes("key") && !node.includes("var")) yield {
        val nodeWithNewPreds = node.updateValues(predicates_t)
        oeManager.isRepresentative(nodeWithNewPreds)
        nodeWithNewPreds
      }
    )
//  mainBank.map(n => (n._1, n._2.filter(c => (!c.includes("key") && !c.includes("var"))))).
//  values.flatten.toList.map(p =>
//  if (p.predicates != predicates_t)
//    oeManager.isRepresentative(p.updateValues(predicates_t))
//  else oeManager.isRepresentative(p)) // OE

  val vars = if (outerVars == null) null
    else for ((cost,varlist) <- outerVars) yield (cost,
      for (v <- varlist) yield {
        val withNewPreds = v.updateValues(predicates_t)
        oeManager.isRepresentative(withNewPreds)
        withNewPreds
      }
    )
  //if (vars != null) vars.values.flatten.toList.map(p => oeManager.isRepresentative(p)) // OE
  var rootMaker: Iterator[ASTNode] = currIterator.next().
    probe_init(vocab, costLevel, predicates_t, mainBank, nested, varBank, vars)

  def resetEnumeration(): Unit = {
    currIterator = totalLeaves.sortBy(_.rootCost).iterator
    rootMaker = currIterator.next().probe_init(vocab, costLevel, predicates_t, mainBank, nested, varBank, vars)
    currLevelPrograms.clear()
    oeManager.clear()
  }

  /**
   * This method moves the rootMaker to the next possible non-leaf. Note that this does not
   * change the level/height of generated programs.
   *
   * @return False if we have exhausted all non-leaf AST nodes.
   */
  def advanceRoot(): Boolean = {
    rootMaker = null
    while (rootMaker == null || !rootMaker.hasNext) {
      if (!currIterator.hasNext) { return false }
      val next = currIterator.next()
      rootMaker = next.probe_init(vocab, costLevel, predicates_t, mainBank, nested, varBank, vars)
      if ((next.nodeType == classOf[StringToStringListCompNode]) || (next.nodeType == classOf[StringToIntListCompNode])
      || (next.nodeType == classOf[IntToStringListCompNode]) || (next.nodeType == classOf[IntToIntListCompNode])
      || (next.nodeType == classOf[StringStringMapCompNode]) || (next.nodeType == classOf[StringIntMapCompNode])
      || (next.nodeType == classOf[StringListStringMapCompNode]) || (next.nodeType == classOf[StringListIntMapCompNode])
      || (next.nodeType == classOf[IntStringMapCompNode]) || (next.nodeType == classOf[IntIntMapCompNode])
        || (next.nodeType == classOf[StringStringFilteredMapNode]) || (next.nodeType == classOf[StringIntFilteredMapNode])
        || (next.nodeType == classOf[IntStringFilteredMapNode]) || (next.nodeType == classOf[IntIntFilteredMapNode]))
       nested = false
    }
    true
  }

  def updateBank(program: ASTNode): Unit = { //TODO: Add check to only add non-variable programs,
    // TODO: aren't only var programs being generated except for arity 0 programs?
    if (!mainBank.contains(program.cost))
      mainBank(program.cost) = ArrayBuffer(program)
    else
      mainBank(program.cost) += program
  }

  def changeLevel(): Boolean = {
    currIterator = totalLeaves.sortBy(_.rootCost).iterator //todo: more efficient
    if (!nested) for (p <- currLevelPrograms) updateBank(p)
    costLevel += 1
    currLevelPrograms.clear()
    advanceRoot()
  }


  def getNextProgram(): Option[ASTNode] = {
    var res: Option[ASTNode] = None
    // Iterate while no non-equivalent program is found
    while (res.isEmpty) {
      if (rootMaker.hasNext) {
        val program = rootMaker.next
        if (program.values.nonEmpty && oeManager.isRepresentative(program)
        && !oeManager.irrelevant(program)
        ) res = Some(program)
      }
      else if (currIterator.hasNext) {
        if (!advanceRoot()) {
          if (!changeLevel()) return None
        }
      }
      else if (!changeLevel()) {
        return None
      }
    }
    currLevelPrograms += res.get
    //Console.withOut(size_log) { println(nested, currLevelPrograms.takeRight(1).map(c => (c.code, c.values))) }
    res
  }
}
