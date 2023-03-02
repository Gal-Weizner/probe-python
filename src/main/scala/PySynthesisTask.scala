package sygus

import ast.Types.Types
import ast._
import net.liftweb.json.JsonAST.{JArray, JObject}
import net.liftweb.json.{JsonParser, prettyRender}
import vocab._

trait PySynthesisTask
{
  val returnType: ast.Types.Value
  val parameters: List[(String, ast.Types.Value)]
  val vocab: VocabFactory
  val examples: List[Example]
  val predicates: Predicates

  def fit(program: ASTNode): (Int, Int)

  override def toString: String =
  {
    s"\treturnType: $returnType\n" +
      s"\tparameters: $parameters\n" +
      "\tvocab: [...]\n" +
      s"\texamples: $examples"
  }
}

class PythonExample(var env: Map[String, String])
{
  env = env.filter(pair => PythonExample.reserved_names.contains(pair._1))
}

object PythonExample
{
  val reserved_names: Set[String] =
    Set("#", "$", "__run_py__")
}

class PythonPBETask(
                     val returnType: ast.Types.Value,
                     val parameters: List[(String, ast.Types.Value)],
                     val vocab: VocabFactory,
                     val examples: List[Example],
                     val predicates: Predicates,
                     val outputVar: String) extends PySynthesisTask
{
  override def fit(program: ASTNode): (Int, Int) =
  {
    val expectedResults = examples.map(_.output)
    val k = program.values.zip(expectedResults).count(pair => pair._1 == pair._2)
    val n = expectedResults.length
    (k, n)
  }
}

object PythonPBETask
{
  private def cleanupInputs(input: Map[String, Any]): Map[String, Any] = {
    val parser = new InputParser
    input
      .filter(v => !PythonExample.reserved_names.contains(v._1))
      // TODO Is there a cleaner way to do this?
      .filter(_._2.isInstanceOf[String])
      .map(variable => parser.parse(variable._2.asInstanceOf[String]) match {
        case None =>
          trace.DebugPrints.eprintln(s"Input not recognized: $variable")
          (variable._1, null)
        case Some(v) =>
          (variable._1, v)
      })
      .filter(v => v._2 != null)
  }

  private def getTypeOfAll(values: List[Any]): Types = {
    val (empty, nonempty) = values.partition(v => v.isInstanceOf[Iterable[_]] && v.asInstanceOf[Iterable[_]].isEmpty)
    val neType = if (nonempty.isEmpty) Types.Unknown else nonempty.map(v => Types.typeof(v)).reduce((acc,t) => if (acc == t) t else Types.Unknown)
    if (!empty.isEmpty) {
      if (nonempty.isEmpty){
        val defaultTypes: Set[Types] = empty.map( v => v match {
          case l: List[_] => Types.StringList
          case m: Map[_,_] => Types.Map(Types.PyString,Types.PyInt)
        }).toSet
        return if (defaultTypes.size == 1) defaultTypes.head else Types.Unknown
      }
      else  for (v <- empty) {
        if (neType match {
          case Types.StringList | Types.IntList => !v.isInstanceOf[List[_]]
          case Types.Map(kt, vt) => !v.isInstanceOf[Map[_, _]]
          case _ => false //nonempties are not a list/map, fail.
        }) return Types.Unknown
      }
      neType
    }
    else neType
  }

  def fromString(jsonString: String, size: Boolean): PythonPBETask =
  {
    val input = JsonParser.parse(jsonString).asInstanceOf[JObject].values
    val outputVarName: String = input("varName").asInstanceOf[String]
    val examples = input("env").asInstanceOf[List[Map[String,Any]]]
      .map(cleanupInputs)
      .map(env => Example(env.filter(_._1 != outputVarName), env(outputVarName)))

    val returnType = getTypeOfAll(examples.map(_.output))
    val parameters =
      examples.head.input
        .map { inputVar =>
          val varValueOpts = examples.map(ex => ex.input.find(kv => kv._1 == inputVar._1))
          (inputVar._1, if (varValueOpts.exists(_.isEmpty)) Types.Unknown else getTypeOfAll(varValueOpts.flatten.map(_._2)))
        }
        // TODO Handle empty sets
        .filter(!_._2.equals(Types.Unknown))
        .toList
    val predicates_list = examples.map(ex => ExamplePredicate(ex.input, Some(ex.output)))
    val predicates = Predicates(predicates = predicates_list, num_of_examples = predicates_list.length)
    val additionalLiterals = getStringLiterals(examples)
    val vocab = PythonPBETask.vocabFactory(parameters,additionalLiterals, size, predicates)

    val rs = new PythonPBETask(returnType, parameters, vocab, examples,predicates, outputVarName)
    trace.DebugPrints.dprintln(s"Solving Python PBE Task:\n\n$rs")
    rs
  }

  private def getStringLiterals(examples: List[Example]): List[String] = {
    if (examples.exists(ex => Types.typeof(ex.output) != Types.PyString)) //this is only for strings
      return Nil

    val opts = examples.map{ex =>
      val outputVal = ex.output.asInstanceOf[String]
      val stringInputs = for ((_,inputVal) <- ex.input; if(Types.typeof(inputVal) == Types.PyString))
        yield inputVal.asInstanceOf[String];
      val chars : Iterable[String] =
        for (char <- outputVal; if (stringInputs.forall(inputVal => !inputVal.contains(char.toLower) && !inputVal.contains(char.toUpper))))
          yield char.toString
      chars.toSet
    }
    val intersection = opts.reduce((a,b) => a.intersect(b))
    intersection.toList
  }

  private def vocabFactory(variables: List[(String, Types.Value)], additionalLiterals: List[String], size: Boolean,
                           predicates_t: Predicates): VocabFactory =
  {
    val defaultStringLiterals = List(" ")
    val stringLiterals = (defaultStringLiterals ++ additionalLiterals).distinct

    val vocab: List[VocabMaker] = {
      PySynthesisVocab.makeLiterals(stringLiterals, List(0,1,-1,3)) ++ PySynthesisVocab.makeBasicVocab() ++
        PySynthesisVocab.makeComprehensions(predicates_t, size) ++ PySynthesisVocab.makeVariables(variables)
    }

    VocabFactory(vocab, predicates_t)
  }
}