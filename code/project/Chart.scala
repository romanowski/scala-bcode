import java.nio.file.Path

import pl.project13.scala.sbt.JmhPlugin.JmhKeys
import sbt.Keys._
import sbt._
import spray.json.DefaultJsonProtocol._
import spray.json._

object Charts {
  val warmupChart = TaskKey[Unit]("drawCharts")
  val runBenchmark = TaskKey[Unit]("runBechmarks")

  case class Percentiles(`50.0`: Double, `90.0`: Double, `95.0`: Double, `99.0`: Double)

  case class MetricData(score: Double, rawData: Option[Array[Array[Double]]], scorePercentiles: Percentiles)

  case class Benchmark(benchmark: String, primaryMetric: MetricData){
    def name = benchmark.split('.').last
  }

  private implicit val percentilesFormat = jsonFormat4(Percentiles)
  private implicit val mdFormat = jsonFormat3(MetricData)
  private implicit val benchFormat = jsonFormat2(Benchmark)

  def chartBase = file("charts").getAbsoluteFile
  def jsonLocation = file("out.json").toPath.toAbsolutePath
  def txtLocation = file("out.txt").toPath.toAbsolutePath

  private def drawWarmingCharts(data: Array[Benchmark], base: File): Unit = {
    (chartBase / "warming-full").mkdir()
    (chartBase / "warming-partial").mkdir()

    data.zipWithIndex.foreach { case (benchmark, index) =>
      val rawTimes = benchmark.primaryMetric.rawData.get.head
      val avg = benchmark.primaryMetric.scorePercentiles.`50.0`
      val strAvg = "%.4f".format(avg)

      def draw(rawData: Array[Double]): Unit = {
        import benchmark._
        val csv = rawData.zipWithIndex.map { case (t, i) => s"$i\t$t" }.mkString("\n")
        val dataFile = base / s"data-$name.csv"
        IO.write(dataFile, csv)
        val file = (base / s"plot-$name.sh").getAbsoluteFile
        val fullChart = rawData == rawTimes
        val ls = index % 9
        val command =
          s"""set term png size ${if (fullChart) 1920 else 1920 / 2}, 1080
             |set output '$chartBase/warming-${if(fullChart) "full" else "partial"}/${name.replace(" ", "-").toLowerCase}.png'
             |set title '$name'
             |${if(fullChart) "" else "set logscale y 2"}
             |set yrange [1:${if (fullChart) 28 else 256}]
             |plot '$dataFile' with lines lt $ls title 'times', $avg title 'p0.50 = $strAvg' lt ${ls + 1} lw 3
             |""".stripMargin

        IO.write(file, command)
        import scala.sys.process._
        s"gnuplot $file".!
      }

      draw(rawTimes)
      draw(rawTimes.take(500))
    }
  }

  private def summary(baseData: Array[Benchmark], base: File, chartName: String, charKind: String): Unit = {
    val filename = chartName.replace(" ", "-")
    def values(b: Benchmark) = {
      val p = b.primaryMetric.scorePercentiles
      Seq(b.name, p.`99.0`, p.`95.0`, p.`90.0`, p.`50.0`)
    }

    val data = baseData.toList
    val baseline = data.filter(_.name  == "baseline")

    val csv = data.sortBy(_.primaryMetric.scorePercentiles.`90.0`).map(values(_).mkString("\t")).mkString("\n")
    val dataFile = base / s"$filename.csv"
    IO.write(dataFile, csv)

    def plotSet(name: String, column: Int) =
      s"'$dataFile' using ${column + 2}:xtic(1) title '$name'"

    val names = Seq("p99.0", "p95.0", "p90.0", "p50.0")

    val baselineCmd = baseline.map(_.primaryMetric.scorePercentiles.`95.0`)
      .map(v => s"$v ls 6 lw 3 title 'baseline'")

    val plotCmd = names.zipWithIndex.map((plotSet _).tupled) ++ baselineCmd

    val command =
      s"""
         |set term png size 1920, 1080 font "Helvetica,22"
         |set output '$chartBase/$filename-$charKind.png'
         |set key left
         |set grid y
         |${if(data.size < 6) "#" else ""}set xtics rotate
         |set style data histograms
         |#set style histogram rowstacked
         |set boxwidth 0.80
         |set style fill solid 1.0 border -1
         |set title '$chartName [us/op] - $charKind'
         |plot ${plotCmd.mkString(", ")}
      """.stripMargin

    val scriptFile = (base / s"plot-$filename.sh").getAbsoluteFile

    def niceValues(b: Benchmark) = values(b).map {
      case d: Double =>
        "%.4f".format(d)
      case other =>
        other.toString
    }

    IO.write(chartBase / "data" / s"$filename-$charKind.csv", data.map(niceValues).map(_.mkString("\t")).mkString("\n"))
    IO.write(scriptFile, command)
    import scala.sys.process._
    s"gnuplot $scriptFile".!
  }

  val charts = Map[String, String => Boolean](
    "All" -> (_ => true),
    "Baselines" -> (_.contains("baseline")),
    "Java vs. Scalac" -> Set("baseline", "javaOOO", "scalacCake"),
    "Java, OOO in Scala and Cake" ->
      Set("baseline", "javaOOO", "scalaOOO", "scalacCake"),
    "Scala code styles #1" ->
      Set("baseline", "scalaOOO", "scalacCake", "oooLambdas", "typeclass", "patMat", "wrappedInTry"),
    "Scala code styles #2" ->
      Set("baseline", "scalaOOO", "scalacCake", "oooLambdas", "typeclass", "patMat", "wrappedInTry"),
    "Scala code styles #3" ->
      Set("baseline", "scalaOOO", "scalacCake", "oooLambdas", "typeclass", "patMat", "wrappedInTry"),
    "Scala code styles #4" ->
      Set("baseline", "scalaOOO", "scalacCake", "oooLambdas", "typeclass", "patMat", "wrappedInTry")
  )

  def implementWarmupChart = warmupChart := {
    val base = streams.value.cacheDirectory
    val jsonData = jsonLocation.toFile
    assert(jsonData.exists(), "Run benchmarks before!")
    val data = IO.read(jsonData).parseJson.convertTo[Array[Benchmark]]
    //streams.value.log.success(s"Saving to $base using: ${data.toSeq.mkString("\n")}")
    val (warming, hot) = data.partition(_.primaryMetric.rawData.isDefined)

    chartBase.mkdir()

    drawWarmingCharts(warming, base)

    for ((title, isApplicable) <- charts) {
      summary(hot.filter(b => isApplicable(b.name)), base, title, "Hot")
      summary(warming.filter(b => isApplicable(b.name)), base, title, "Warming")
    }
  }

  private val benchmarkCmd = s" .*Benchmark.* -rff $jsonLocation -rf json -o $txtLocation"

  def settings = Seq(
    runBenchmark := run.in(JmhKeys.Jmh).toTask(benchmarkCmd).value,
    implementWarmupChart
  )

}
