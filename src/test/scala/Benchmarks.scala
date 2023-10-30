import java.io.File
import sygus.Main
import sygus.Main.RankedProgram

object Benchmarks extends App
{
  println(
      "-------------------------------------------\n" +
      "| Probe-Python Synthesizer |\n" +
      "-------------------------------------------\n")
  println("Index Name                 Program")

  val benchmarks = new File("src/test/resources/")
  assert(benchmarks.isDirectory)

  benchmarks.listFiles().filter(_.isDirectory).foreach(
    dir => {
      println("----- -------------------- --------------------------------------")
      dir.listFiles()
        .filter(_.getName.contains(".examples.json"))
        .filter(!_.getName.contains(".out"))
        .sorted
        .zipWithIndex
        .foreach(benchmark => {
          val file = benchmark._1
          val index = benchmark._2 + 1
          val name: String = file.getName.substring(0,file.getName.indexOf('.'))
          print(f"($index%2d)  [$name%18s] ")

          try {
                val (prog_list, time) = Main.pySynthesize(file.getAbsolutePath)
            for(prog <- prog_list)
              {

                println(f" time: [${time / 1000.0}%1.3f], program code: [${prog.program.code}], rank: [${prog.rank}]")
               /// ADD CODE FROM BESTER TO INDICATE RANK OF EXPECTED PROGRAM
            }
          } catch {
            case e: Throwable => println(e.getMessage)
          }
        })
    })
}
