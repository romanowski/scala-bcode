import pl.project13.scala.sbt.JmhPlugin.JmhKeys
import sbt.Keys._
import sbt._
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.util.control.NonFatal

object Charts {
  val warmupChart = TaskKey[Unit]("drawCharts")
  val runBenchmark = TaskKey[Unit]("runBechmarks")

  case class Percentiles(`50.0`: Double, `90.0`: Double, `95.0`: Double, `99.0`: Double)

  case class MetricData(score: Double, rawData: Option[Array[Array[Double]]], scorePercentiles: Percentiles)

  case class Benchmark(benchmark: String, primaryMetric: MetricData) {
    def name = benchmark.split('.').last
  }

  private implicit val percentilesFormat = jsonFormat4(Percentiles)
  private implicit val mdFormat = jsonFormat3(MetricData)
  private implicit val benchFormat = jsonFormat2(Benchmark)

  def chartBase = file("charts").getAbsoluteFile

  def jsonLocation = file("out.json").toPath.toAbsolutePath

  def txtLocation = file("out.txt").toPath.toAbsolutePath

  val combinedCharts = Seq(
    "Java vs. Scalac" -> Seq("scalaOOO", "scalacCake") ,
	  "Patternmatching?" -> Seq("scalaOOO", "scalacCake", "nestedPatMat") ,
	  "Pattern matching" -> Seq("patMat", "nestedPatMat", "nestedClassPatMat"),
	  "Scala styles" -> Seq("scalaOOO", "scalacCake", "oooLambdas", "typeclass", "patMat", "wrappedInTry"),
    "Typeclasses and Patern matching" -> Seq("scalaOOO", "typeclass", "patMat"),
    "Lambdas and wrapping" -> Seq("scalaOOO", "oooLambdas", "wrappedInTry")
  )


  private def drawWarmupTimes(data: Array[Benchmark], base: File): Unit = try {
    val csvs: Map[String, File] = {
      var fullData = Map[String, Seq[Double]]()
      var currentName = ""
      var currentData = List.empty[Double]
      val Name = "# Benchmark: rpg.bench.WarmingJitBenchmark.(.+)".r
      val DataPoint = "# Warmup Iteration.+: (.+) ±.+".r
      IO.readLines(txtLocation.toFile).foreach {
        case Name(name) =>
          if(currentData.nonEmpty) fullData += (currentName -> currentData.reverse)
          currentName = name
          currentData = Nil
        case DataPoint(str) =>
          currentData ::= str.toDouble
        case _ =>
      }
      if(currentData.nonEmpty) fullData += (currentName -> currentData.reverse)
      fullData.map {
        case (name , v) =>
          val points = v.zipWithIndex.map{case (n, i) => s"$n,$i"}.mkString("\n")
          val file = base / "warming" / s"$name.csv"
          IO.write(file, points)
          name -> file
      }
    }
    (chartBase/ "warmup").mkdirs()

    val chartToPrint =
	    data.map(b => b.name -> Seq(b.name)) ++
		    combinedCharts ++
		    Seq("All" -> data.map(_.name).toSeq)

    (chartToPrint).foreach{ case (title, names) =>

      val cmds = names.map { name =>
          s"'${csvs(name)}' with lines title '$name'"
      }
      (base / "warming").mkdir()
      val fileName = title.replace(" ", "-").toLowerCase
      val file = (base / "warmup" / s"plot-$fileName.sh").getAbsoluteFile

      val command =
        s"""set term png size 910, 520 font "Helvetica,12"
           |set output '$chartBase/warmup/$fileName.png'
           |set title '$title' font "Helvetica,21"
           |
           |plot ${cmds.mkString(", ")}
           |""".stripMargin

      IO.write(file, command)
      import scala.sys.process._
      s"gnuplot $file".!
    }
  } catch {
    case e =>
      e.printStackTrace()
  }

  private def drawWarmingCharts(data: Array[Benchmark], base: File): Unit = {
    (chartBase / "warming-full").mkdir()
    (chartBase / "warming-partial").mkdir()

    val csvs = data.map { bench =>
      import bench._
      val dataFile = base / s"data-$name.csv"
      val csv = bench.primaryMetric.rawData.get.flatten.zipWithIndex.map { case (t, i) => s"$i\t$t" }.mkString("\n")
      IO.write(dataFile, csv)
      name -> dataFile
    }.toMap

    val avgs = data.map(bench => bench.name -> bench.primaryMetric.scorePercentiles.`50.0`).toMap

    def drawChart(names: Seq[String], title: String, full: Boolean = true): Unit ={
      val fileName = title.replace(" ", "-").toLowerCase
      val file = (base / s"plot-$fileName.sh").getAbsoluteFile

      val limit = if(full) "" else "every ::0::650"

      val cmds = names.zipWithIndex.flatMap { case (name, index) =>
        val ls = if(names.size > 1) s"ls ${index + 1}" else ""
        Seq(
          s"'${csvs(name)}' $limit with lines title '$name' $ls",
          s"${avgs(name)} title 'p0.50 = ${"%.4f".format(avgs(name))}' $ls lw 3"
        )
      }

      val maxAvg = names.map(avgs).max
      val shift = if(maxAvg > 32) 32 else 0
      val command =
        s"""set term png size 910, 520 font "Helvetica,12"
           |set output '$chartBase/warming-${if (full) "full" else "partial"}/$fileName.png'
           |set title '$title' font "Helvetica,21"
           |#set key outside;
           |#set key right top;
           |${if (full) "" else "set logscale y 2"}
           |set yrange [3:${if (full) 42 + shift else 150}]
           |plot ${cmds.mkString(", ")}
           |""".stripMargin

      IO.write(file, command)
      import scala.sys.process._
      s"gnuplot $file".!
    }
    val toDraw =  data.map(d => d.name -> Seq(d.name)) ++ combinedCharts
    toDraw.foreach { case (title, data) =>
        drawChart(data, title)
        drawChart(data, title, full = false)
    }
  }

  private def summary(baseData: Array[Benchmark], base: File,
                      chartName: String, charKind: String, baseline: Seq[Benchmark]): Unit = {
    val filename = chartName.replace(" ", "-")

    def values(b: Benchmark) = {
      val p = b.primaryMetric.scorePercentiles
      Seq(b.name, p.`99.0`, p.`95.0`, p.`90.0`, p.`50.0`)
    }

    val data = baseData.toList

    val csv = data.sortBy(_.primaryMetric.scorePercentiles.`90.0`).map(values(_).mkString("\t")).mkString("\n")
    val dataFile = base / s"$filename.csv"
    IO.write(dataFile, csv)

    def plotSet(name: String, column: Int) = Seq(
      s"'$dataFile' using ${column + 2}:xtic(1) title '$name'",
      s"""'$dataFile' using ($$0-0.25+${column}./5):(0):(fn($$${column + 2})) """ +
        """with labels font "Helvetica,9" rotate by 90 left notitle"""
    )

    val names = Seq("p99.0", "p95.0", "p90.0", "p50.0")

    val baselineCmd = baseline.map(_.primaryMetric.scorePercentiles.`95.0`)
      .map(v => s"$v ls 6 lw 3 title 'baseline'")

    val plotCmd = baselineCmd ++ names.zipWithIndex.flatMap((plotSet _).tupled)

    val command =
      s"""
         |set term png size 910, 520 font "Helvetica,14"
         |set output '$chartBase/$filename-$charKind.png'
         |set key left
         |set grid y
         |fn(v) = sprintf(" %.3f", v)
         |${if (data.size < 6) "#" else ""} set xtics rotate by -15
         |set style data histograms
         |set style histogram clustered gap 1
         |set style fill solid 1.0 border -1
         |set title '$chartName [us/op] - $charKind' font "Helvetica,21"
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
      Set("javaOOO", "scalaOOO", "scalacCake"),
    "Scala code styles 0..4" ->
      Set("scalaOOO", "scalacCake"),
    "Scala code styles 1..4" ->
      Set("scalaOOO", "scalacCake", "patMat"),
    "Scala code styles 2..4" ->
      Set("scalaOOO", "scalacCake", "patMat", "wrappedInTry"),
    "Scala code styles 3..4" ->
      Set("scalaOOO", "scalacCake", "oooLambdas", "patMat", "wrappedInTry"),
    "Scala code styles 4..4" ->
      Set("scalaOOO", "scalacCake", "oooLambdas", "typeclass", "patMat", "wrappedInTry"),
    "Fixing pattern matching" ->
      Set("scalaOOO", "oooLambdas", "patMat", "nestedPatMat"),
    "Classes pattern matching" ->
      Set("scalaOOO", "oooLambdas", "patMat", "nestedPatMat", "nestedClassPatMat"),
    "Optimized pattern matching" ->
      Set("scalaOOO", "oooLambdas", "patMat", "nestedPatMat", "nestedClassPatMat", "nestedOptPatMat")
  )

  def implementWarmupChart = warmupChart := (try {
    val base = streams.value.cacheDirectory
    val jsonData = jsonLocation.toFile
    assert(jsonData.exists(), "Run benchmarks before!")
    val data = IO.read(jsonData).parseJson.convertTo[Array[Benchmark]]
    //streams.value.log.success(s"Saving to $base using: ${data.toSeq.mkString("\n")}")
    val (warming, allHot) = data.partition(_.primaryMetric.rawData.isDefined)
    val (hot, warmingJit) = allHot.partition(_.benchmark.startsWith("rpg.bench.HotBenchmark"))

    IO.delete(chartBase)
    chartBase.mkdir()

    drawWarmingCharts(warming, base)

    for ((title, isApplicable) <- charts) {
      summary(hot.filter(b => isApplicable(b.name)), base, title, "Hot",hot.filter(_.name == "baseline"))
      summary(warming.filter(b => isApplicable(b.name)), base, title, "Warming", warming.filter(_.name == "baseline"))
    }

    drawWarmupTimes(warmingJit, base)
  } catch {
		case NonFatal(e) =>
			e.printStackTrace()
	})

  private val benchmarkCmd = s" .*Benchmark.* -rff $jsonLocation -rf json -o $txtLocation"

  def settings = Seq(
    runBenchmark := run.in(JmhKeys.Jmh).toTask(benchmarkCmd).value,
    implementWarmupChart
  )

}
