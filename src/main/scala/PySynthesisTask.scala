package sygus

import ast.Types.Types
import ast._
import net.liftweb.json.JsonAST.{JArray, JObject}
import net.liftweb.json.{JsonParser, prettyRender}
import vocab._

import scala.language.postfixOps

trait PySynthesisTask
{
  val returnType: ast.Types.Value
  val parameters: List[(String, ast.Types.Value)]
  val vocab: VocabFactory
  val examples: List[Example]
  val predicates: Predicates
//  private val parsed = new InputParser(new BufferedTokenStream(new SyGuSLexer(CharStreams.fromString(content)))).syGuS()
//  private val synthFun = parsed.cmd().asScala.filter(cmd => cmd.getChild(1) != null && cmd.getChild(1).getText == "synth-fun").head
//  val functionParameters = synthFun.sortedVar().asScala.map(svar => (svar.Symbol().getText -> Types.withName(svar.sort().identifier().getText))).toList


  def fit(program: ASTNode): (Int, Int, Int)

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
  override def fit(program: ASTNode): (Int, Int, Int) =
  {
    val expectedResults = examples.map(_.output)
    val k = program.values.zip(expectedResults).count(pair => pair._1 == pair._2)
    val preds = program.values.takeRight(program.values.length - predicates.num_of_examples).count(pred => pred == true)
    val n = expectedResults.length
    (k, n, preds)
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
    val example_predicates_list = examples.zipWithIndex.map{
      case (ex,idx) => ExamplePredicate(ex.input, Some(ex.output), idx)}
    val temp_predicates = Predicates(predicates = example_predicates_list , num_of_examples = example_predicates_list.length)
    var retains_list: List[RetainPredicate] = Nil
    var excludes_list: List[ExcludePredicate] = Nil
    val num_examples = example_predicates_list.length
    if (input.contains("retain")) {
      retains_list = input("retain").asInstanceOf[List[Map[String, String]]].map(
        retain_exp => new RESLParser(temp_predicates, retain_exp("type")).parse(retain_exp("expression"))).zipWithIndex.map{
        case ((v_map, l_map), idx) => RetainPredicate(idx + num_examples, l_map.toMap, v_map.toMap)
      }
    }
    if (input.contains("exclude")) {
        excludes_list = input("exclude").asInstanceOf[List[Map[String, String]]].map(
        exclude_exp => new RESLParser(temp_predicates, exclude_exp("type")).parse(exclude_exp("expression"))).zipWithIndex.map {
        case ((v_map, l_map), idx) => ExcludePredicate(idx + + retains_list.length + num_examples,
          l_map.toMap, v_map.toMap)
      }
    }
    val uses_variable_pred = UsesVariablesPredicate(num_examples + retains_list.length + excludes_list.length) // make sure it is always the last predicate
    val final_predicates = Predicates(example_predicates_list ++ retains_list ++ excludes_list ++ List(uses_variable_pred), example_predicates_list.length)
    val additionalLiterals = getStringLiterals(examples)
    val vocab = PythonPBETask.vocabFactory(parameters,additionalLiterals, size, final_predicates)

    val rs = new PythonPBETask(returnType, parameters, vocab, examples, final_predicates, outputVarName)
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