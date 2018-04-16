# How to run benchmarks

To run all benchmarks you need to run `runBechmarks` task in sbt. It is simple wrapper around [sbt-jmh] plugin so you can use 'jvm:rum' command directly. `runBechmarks` just create few dirs and runs 'jmh:run  .*Benchmark.* -rff $jsonLocation -rf json -o $txtLocation').

`runBench` will produce human readable output in `out.txt` file and json data in `out.json`. Both files are commited to repository you can just play with my data :)

# How to draw charts for benchmarks

Charts uses `gnuplot` so you need to install it first. I've used `gnuplot-X11` package.

To draw chart all you need to do is run `drawCharts` task in sbt.

## What charts are drawn?

 - Results on [HotBenchmarks](). Drawn as histogram for main percentiles (.50, .90, .95 and .99)
 - Results on [WarmingBenchmarks]() benchmark. Drawn line chart for all and first 600 iteration.
 - Results on [WarmupBenchmarks]() benchmark. Drawn line chart for all and first 600 iteration.

Except for all/single results you can define your own combined charts (to compare e.g. pattern match with ooo). Those groups are defined in [Chart.scala](project/Chart.scala)
 - in [`charts` value](Chart.scala) for HotBenchmarks
 - in [`combinedCharts` value](Chart.scala) for [WarmingBenchmarks]() and [WarmupBenchmarks]()
 