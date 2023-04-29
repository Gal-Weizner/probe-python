package sygus

import ast.ASTNode
import enumeration.InputsValuesManager
import org.antlr.v4.runtime.{BufferedTokenStream, CharStreams, RecognitionException, Token}
import pcShell.ConsolePrints.{consoleEnabled, cprint, cprintln, in, infoColor, showFit}

import util.control.Breaks._
import scala.concurrent.duration._
import trace.DebugPrints.{dprintln, iprintln}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source.fromFile

object Main extends App {
  val filename = {
//  "src/test/resources/new_benchmarks/rotate_left.examples.json"
//  "src/test/resources/old_benchmarks/count_characters.examples.json"
//  "src/test/resources/new_benchmarks/count_substring.examples.json"
//    "src/test/resources/new_benchmarks/check_uses_variables.examples.json"
//    "src/test/resources/new_benchmarks/modulo_3.examples.json"
//  "src/test/resources/new_benchmarks/divide_by_3.examples.json"
  //"src/test/resources/new_benchmarks/rotate_concat.examples.json"
//  "src/test/resources/benchmarks/abbreviate_1_ex.examples.json"
//   "src/test/resources/old_benchmarks/string_length.examples.json"
//    "src/test/resources/new_benchmarks/upper.examples.json"
//    "src/test/resources/benchmarks/empty_map.examples.json"
//    "src/test/resources/bester_benchmarks/count_characters_wrong_example.examples.json"
//    "src/test/resources/bester_benchmarks/string_constant_does_not_exist.examples.json"
    "src/test/resources/new_benchmarks/get_first_characters.examples.json"
  }

  //"src/test/resources/old_benchmarks/vowel_count.examples.json"


  case class RankedProgram(program: ASTNode, rank: Double) extends Ordered[RankedProgram] {
    override def compare(that: RankedProgram): Int = this.rank.compare(that.rank)
  }
  case class ExpectedEOFException() extends Exception

  def synthesizePython(task: PySynthesisTask, sizeBased: Boolean, timeout: Int = 40): List[RankedProgram] = {
    var rs: List[RankedProgram] = Nil
    val oeManager = new InputsValuesManager()
    var bank = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
    var mini = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()

    val enumerator = if (!sizeBased) new enumeration.PyEnumerator(
      task.vocab,
      oeManager,
      task.predicates) else
      new enumeration.PyProbEnumerator(task.vocab, oeManager, task.predicates, false, 0, bank,
        mini)
    val deadline = timeout.seconds.fromNow
    val ranks = mutable.ListBuffer[RankedProgram]()

    breakable {
      for ((program, i) <- enumerator.zipWithIndex) {
        if (!deadline.hasTimeLeft) { //TODO: fix this!
//          rs = Some(("None", timeout * 1000 - deadline.timeLeft.toMillis.toInt))
          rs = ranks.takeRight(5).toList

          break
        }
        if (program.nodeType == task.returnType) {
          if (task.predicates.someHolds(program)) {
            val rank = ProgramRanking.ranking(program, task.examples.map(_.output), task.parameters.map(_._1))
            val ranked = RankedProgram(program, rank)
            val ip = ranks.search(ranked)
            if (ip.insertionPoint > 0 || ranks.length < 50)
              ranks.insert(ip.insertionPoint, ranked)
            if (ranks.length > 50) ranks.remove(0)
            if (task.predicates.allHolds(program)) {
              iprintln(program.code)
              println(s"\rCurrent best: ${ranks.takeRight(1).map { r => showFit(task.fit(r.program)) }.mkString("")}")
              rs = ranks.takeRight(5).toList
              println(program.code, program.height, program.cost, bank.values.toList.length)
              break
              break
            }
          }
        }
        if (i % 1000 == 0) {
          iprintln(i + ": " + program.code)
          iprintln(s"\rCurrent best: ${ranks.takeRight(1).map { r => showFit(task.fit(r.program)) }.mkString("")}")
        }
        if (trace.DebugPrints.debug) {
          val p = PostProcessor.clean(program)
          println(s"[$i] (${program.height}) ${p.code}")
        }
      }
    }
    iprintln(s"\rReturning best 5 programs:\n${rs.reverse.map(prog => (prog.program.code, prog.rank,
      showFit(task.fit(prog.program)))).mkString("\n")}")
    rs.map(program =>program.copy(program = PostProcessor.clean(program.program)))
  }

  def pySynthesize(filename: String, sizeBased: Boolean = true): List[RankedProgram] = {
    val task: PySynthesisTask = PythonPBETask.fromString(fromFile(filename).mkString, sizeBased)
    synthesizePython(task, sizeBased)
  }

  trace.DebugPrints.setInfo()
  if (filename.endsWith(".json"))
    pySynthesize(filename)
}