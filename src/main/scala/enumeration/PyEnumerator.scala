package enumeration

import java.io.FileOutputStream
import ast.ASTNode
import trace.DebugPrints
import vocab.{VocabFactory, VocabMaker}
import sygus.Predicates

import scala.collection.mutable

class PyEnumerator(val vocab: VocabFactory, val oeManager: OEValuesManager, val predicates: Predicates) extends Iterator[ASTNode]{
  override def toString(): String = "enumeration.Enumerator"

  var nextProgram: Option[ASTNode] = None

  var currIter: Iterator[VocabMaker] = vocab.leaves
  var prevLevelProgs: mutable.ListBuffer[ASTNode] = mutable.ListBuffer()
  var currLevelProgs: mutable.ListBuffer[ASTNode] = mutable.ListBuffer()
  var height = 0
  var rootMaker: Iterator[ASTNode] =
    currIter.next().init(currLevelProgs.toList, predicates, vocab, height)

  ProbUpdate.probMap = ProbUpdate.createPyProbMap(vocab)
  ProbUpdate.priors = ProbUpdate.createPyPrior(vocab)
  override def hasNext: Boolean = if (nextProgram.isDefined) true
  else {
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

  /**
   * This method moves the rootMaker to the next possible non-leaf. Note that this does not
   * change the level/height of generated programs.
   * @return False if we have exhausted all non-leaf AST nodes.
   */
  def advanceRoot(): Boolean = {
    rootMaker = null
    while (rootMaker == null || !rootMaker.hasNext) {
      // We are out of programs!
      if (!currIter.hasNext) return false
      val next = currIter.next()
      rootMaker = next.init(prevLevelProgs.toList, predicates, this.vocab, height)
    }

    true
  }

  /**
   * This method resets the variables to begin enumerating the next level (taller) trees.
   * @return False if the current level failed to generate any new programs.
   */
  def changeLevel(): Boolean = {
    //dprintln(currLevelProgs.length)
    if (currLevelProgs.isEmpty) return false

    currIter = vocab.nonLeaves()
    height += 1
    prevLevelProgs ++= currLevelProgs
    currLevelProgs.clear()
    advanceRoot()
  }

  def getNextProgram(): Option[ASTNode] = {
    var res : Option[ASTNode] = None

    // Iterate while no non-equivalent program is found
    while (res.isEmpty) {
      if (rootMaker.hasNext) {
        val prog = rootMaker.next

        if (prog.values.nonEmpty && oeManager.isRepresentative(prog)) {
          res = Some(prog)
        }
      }
      else if (currIter.hasNext) {
        if (!advanceRoot()) {
          if (!changeLevel()) return None
        }
      }
      else if (!changeLevel()) {
        return None
      }
    }
    currLevelProgs += res.get
    res
  }
}

