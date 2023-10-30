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
//  "src/test/resources/new_benchmarks/divide_by_3.examples.json"
//  "src/test/resources/new_benchmarks/rotate_concat.examples.json"
//  "src/test/resources/new_benchmarks/abbreviate_1_ex.examples.json"
//   "src/test/resources/old_benchmarks/string_length.examples.json"
//    "src/test/resources/new_benchmarks/upper.examples.json"
//    "src/test/resources/benchmarks/empty_map.examples.json"
//    "src/test/resources/bester_benchmarks/count_characters_wrong.examples.json"
//    "src/test/resources/bester_benchmarks/string_constant_does_not_exist.examples.json"
//    "src/test/resources/bester_benchmarks/count_sub_strings_wrong.examples.json"
//    "src/test/resources/new_benchmarks/get_first_characters.examples.json"
//    "src/test/resources/retain_and_exclude_benchmarks/count_substring_resl.examples.json"
//    "src/test/resources/retain_and_exclude_benchmarks/count_sub_strings_wrong_exclude.examples.json"
//    "src/test/resources/bester_benchmarks/divide_by_3_bester.examples.json"
//    "src/test/resources/retain_and_exclude_benchmarks/divide_by_3_exclude.examples.json"
//    "src/test/resources/bester_benchmarks/rotate_left_bester2.examples.json"
//    "src/test/resources/bester_benchmarks/abbreviate_3_bester.examples.json"
//    "src/test/resources/new_benchmarks/remove_outer_characters.examples.json"
//    "src/test/resources/new_benchmarks/reverse_string.examples.json"
//    "src/test/resources/bester_benchmarks/reverse_string_bester.examples.json"
//    "src/test/resources/new_benchmarks/max_int_in_string.examples.json"
//    "src/test/resources/new_benchmarks/map_characters_count.examples.json"
//    "src/test/resources/bester_benchmarks/map_characters_count_bester.examples.json"
//    "src/test/resources/bester_benchmarks/map_characters_count_bester2.examples.json"
//    "src/test/resources/bester_benchmarks/reverse_words_bester.examples.json"
//    "src/test/resources/retain_and_exclude_benchmarks/reverse_words_retain.examples.json"
//    "src/test/resources/new_benchmarks/reverse_list.examples.json"
//    "src/test/resources/bester_benchmarks/reverse_list_bester.examples.json"
//    "src/test/resources/retain_and_exclude_benchmarks/reverse_list_exclude.examples.json"
    "src/test/resources/retain_and_exclude_benchmarks/count_substring_retain.examples.json"
//    "src/test/resources/retain_and_exclude_benchmarks/count_substring_exclude_part_of_correct_solution.examples.json"
//    "src/test/resources/retain_and_exclude_benchmarks/count_substring_wrong_retain.examples.json"
//    "src/test/resources/retain_and_exclude_benchmarks/remove_outer_characters_retain.examples.json"
//    "src/test/resources/retain_and_exclude_benchmarks/reverse_list_retain.examples.json"
  }




  case class RankedProgram(program: ASTNode, rank: Double) extends Ordered[RankedProgram] {
    override def compare(that: RankedProgram): Int = this.rank.compare(that.rank)
  }
  case class ExpectedEOFException() extends Exception

  def interpret(task: PySynthesisTask, str: String): ASTNode = {
    val parser = new ExpressionParser(task.predicates)
    parser.parse(str)
  }

  def synthesizePython(task: PySynthesisTask, sizeBased: Boolean, timeout: Int = 40): (List[RankedProgram], Long) = {
    var rs: (List[RankedProgram], Long) = (Nil, 0)
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
    var time_left = deadline.time.toMillis
    val ranks = mutable.ListBuffer[RankedProgram]()

    breakable {
      for ((program, i) <- enumerator.zipWithIndex) {
        if (!deadline.hasTimeLeft) { //TODO: fix this!

          rs = (ranks.takeRight(5).toList, timeout.toLong)
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
              time_left = timeout.seconds.toMillis - deadline.timeLeft.toMillis
              iprintln(program.code)
              println(s"\rCurrent best: ${ranks.takeRight(1).map { r => showFit(task.fit(r.program)) }.mkString("")}")
              rs = (ranks.takeRight(5).toList, time_left)
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
    iprintln(s"\rReturning best 5 programs:\n${rs._1.reverse.map(prog => (prog.program.code, prog.rank,
      showFit(task.fit(prog.program)))).mkString("\n")}")
    (rs._1.reverse.map(program =>program.copy(program = PostProcessor.clean(program.program))), time_left)
  }

  def pySynthesize(filename: String, sizeBased: Boolean = true): (List[RankedProgram], Long) = {
    val task: PySynthesisTask = PythonPBETask.fromString(fromFile(filename).mkString, sizeBased)
    synthesizePython(task, sizeBased)
  }

  trace.DebugPrints.setInfo()
  if (filename.endsWith(".json"))
    pySynthesize(filename)
}