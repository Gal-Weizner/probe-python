package sygus
import ast.ASTNode

object ProgramRanking {

  def ranking(program: ASTNode, expectedResults: List[Any], parameters: List[String]) : Double = {
    old_ranking(program, expectedResults, parameters)
//    new_ranking(program, expectedResults, parameters, 2)
  }

  def old_ranking(program: ASTNode, expectedResults: List[Any], parameters: List[String]) : Double = {
    val (foundResults,distanceFromNotFound) = program.values.zip(expectedResults).foldLeft((0,0.0)) { case ((found, distances), (elem1, elem2)) =>
      if (elem1 == elem2) (found + 1, distances)
      else (found, distances)
    }
    val fittingTheData = foundResults.toDouble / expectedResults.length
    val relevancy = parameters.count(argName => program.includes(argName)).toDouble / parameters.length
    val height = 1.0 / (program.height + 1)
    val size = 1.0 / program.terms
    3 * fittingTheData + 2 * relevancy + size + height
  }

  def new_ranking(program: ASTNode, expectedResults: List[Any], parameters: List[String],
                  restraints_weight: Double): Double = {
    val (exampleResults, distanceFromWrongExamples) = program.values.take(program.predicates.num_of_examples)
      .zip(expectedResults).foldLeft((0, 0.0)) { case ((found, distances), (elem1, elem2)) =>
      if (elem1 == elem2) (found + 1, distances)
      else (found, distances)
    }

    val (restraintsResults, distanceFromWrongRestraints) =
      program.values.takeRight(program.predicates.predicates.length - program.predicates.num_of_examples).dropRight(1)
        .zip(expectedResults).foldLeft((0, 0.0)) { case ((found, distances), (elem1, elem2)) =>
        if (elem1 == elem2) (found + 1, distances)
        else (found, distances)
      }
    val number_of_restraints = program.predicates.predicates.length - program.predicates.num_of_examples - 1
    val FittingTheExamples = exampleResults.toDouble / expectedResults.length
    val FittingTheRestraints = if (number_of_restraints > 0)
      restraintsResults.toDouble / number_of_restraints else 0

    val relevancy = parameters.count(argName => program.includes(argName)).toDouble / parameters.length
    val height = 1.0 / (program.height + 1)
    val size = 1.0 / program.terms
    3 * FittingTheExamples + restraints_weight * FittingTheRestraints + 2 * relevancy + size + height
  }
  def levenshtein(str1: String, str2: String): Int = {
    val lenStr1 = str1.length
    val lenStr2 = str2.length

    val d: Array[Array[Int]] = Array.ofDim(lenStr1 + 1, lenStr2 + 1)

    for (i <- 0 to lenStr1) d(i)(0) = i
    for (j <- 0 to lenStr2) d(0)(j) = j

    for (i <- 1 to lenStr1; j <- 1 to lenStr2) {
      val cost = if (str1(i - 1) == str2(j - 1)) 0 else 1

      d(i)(j) = min(
        d(i-1)(j  ) + 1,     // deletion
        d(i  )(j-1) + 1,     // insertion
        d(i-1)(j-1) + cost   // substitution
      )
    }

    d(lenStr1)(lenStr2)
  }

  def min(nums: Int*): Int = nums.min

}
