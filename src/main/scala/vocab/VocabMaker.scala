package vocab

import ast.{ASTNode, BVLiteral, BVVariable, BoolLiteral, BoolVariable, IntLiteral, IntVariable, StringLiteral, StringVariable}
import ast.Types.Types
import enumeration.ProbUpdate
import sygus.{Predicate, Predicates}

import scala.collection.mutable

class VocabFactory(val leavesMakers: List[VocabMaker], val nodeMakers: List[VocabMaker], val predicates: Predicates) {
  def leaves(): Iterator[VocabMaker] = leavesMakers.iterator
  def nonLeaves(): Iterator[VocabMaker] = nodeMakers.iterator
}

object VocabFactory {
  def apply(vocabMakers: Seq[VocabMaker], predicates: Predicates): VocabFactory = {
    val (leavesMakers, nodeMakers) = vocabMakers.toList.partition(m => m.arity == 0)
    new VocabFactory(leavesMakers, nodeMakers, predicates)
  }
}

trait VocabMaker {
  val arity: Int
  val childTypes: List[Types]
  val nodeType: Class[_ <: ASTNode]
  val head: String
  val returnType: Types
  val predicates: Predicates

  def apply(children: List[ASTNode], predicates: Predicates): ASTNode
  def canMake(children: List[ASTNode]): Boolean = children.length == arity && children.zip(childTypes).forall(pair => pair._1.nodeType == pair._2)

  def rootCost: Int = if (nodeType == classOf[IntLiteral] || nodeType == classOf[StringLiteral] || nodeType == classOf[BoolLiteral]
    || nodeType == classOf[StringVariable] || nodeType == classOf[BoolVariable] || nodeType == classOf[IntVariable]
    || nodeType == classOf[BVLiteral] || nodeType == classOf[BVVariable])
    ProbUpdate.priors(nodeType, Some(head)) else ProbUpdate.priors(nodeType, None)

  def init(programs: List[ASTNode], predicates: Predicates, vocabFactory: VocabFactory, height: Int) : Iterator[ASTNode]
  def probe_init(vocabFactory: VocabFactory,
                 costLevel: Int, predicates: Predicates,
                 bank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]],
                 nested: Boolean,
                 miniBank: mutable.Map[(Class[_], ASTNode), mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]],
                 mini: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]) : Iterator[ASTNode]
}