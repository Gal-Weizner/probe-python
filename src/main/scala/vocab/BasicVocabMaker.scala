package vocab

import ast.Types.Types
import ast.{ASTNode, BVLiteral, BVVariable, BoolLiteral, BoolVariable, IntLiteral, IntVariable, StringLiteral, StringVariable}
import enumeration.{ChildrenIterator, NestedChildrenIterator, ProbChildrenIterator, ProbUpdate}
import sygus.Predicates

import java.io.FileOutputStream
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait BasicVocabMaker extends VocabMaker with Iterator[ASTNode] {
  val returnType: Types
  var childIterator: Iterator[List[ASTNode]] = _
  var size_log = new FileOutputStream("output.txt", true)
  //override val predicates: Predicates
  var predicates: Predicates = _

  override def hasNext: Boolean = childIterator != null && childIterator.hasNext
  def apply(children: List[ASTNode], predicates: Predicates): ASTNode

  override def next: ASTNode =
    this(this.childIterator.next(), predicates)

  override def rootCost: Int = if (nodeType == classOf[IntLiteral] || nodeType == classOf[StringLiteral]
    || nodeType == classOf[BoolLiteral] || nodeType == classOf[StringVariable]
    || nodeType == classOf[BoolVariable] || nodeType == classOf[IntVariable]
    || nodeType == classOf[BVLiteral] || nodeType == classOf[BVVariable])
    ProbUpdate.priors(nodeType, Some(head)) else ProbUpdate.priors(nodeType, None)

  override def init(programs: List[ASTNode], predicates_t: Predicates, vocabFactory: VocabFactory, height: Int) : Iterator[ASTNode] = {
    this.predicates = predicates_t

    this.childIterator = if (this.arity == 0) {
      // No children needed, but we still return 1 value
      Iterator.single(Nil)
    } else if (this.childTypes.map(t => programs.filter(c => t.equals(c.nodeType))).exists(_.isEmpty)) {
      Iterator.empty
    } else {
      new ChildrenIterator(programs, childTypes, height)
    }
    this
  }

   def probe_init(vocabFactory: VocabFactory,
                  costLevel: Int,
                  predicates: Predicates,
                  bank: mutable.Map[Int, ArrayBuffer[ASTNode]],
                  nested: Boolean,
                  miniBank: mutable.Map[(Class[_], ASTNode), mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]],
                  mini: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]) : Iterator[ASTNode] = {

     this.predicates = predicates
     this.childIterator = if (this.arity == 0 && this.rootCost == costLevel) {
       // No children needed, but we still return 1 value
       Iterator.single(Nil)
     }
     else if (mini == null && nested) Iterator.empty
     else if (this.rootCost < costLevel && !nested) { //TODO: add condition (arity != 0)
       val childrenCost = costLevel - this.rootCost
       val children = new ProbChildrenIterator(this.childTypes, childrenCost, bank)
       children
     }
     else if (this.rootCost < costLevel && nested) { //TODO: add condition (arity != 0)
       val childrenCost = costLevel - this.rootCost
       val children = new NestedChildrenIterator(this.childTypes, childrenCost, bank, mini, predicates)
       children
     }
    else {
      Iterator.empty
    }
     this
  }
}