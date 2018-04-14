package rpg.bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.CompilerControl
import org.openjdk.jmh.annotations.CompilerControl.Mode.DONT_INLINE
import org.openjdk.jmh.annotations.Fork
import org.openjdk.jmh.annotations.Measurement
import org.openjdk.jmh.annotations.Mode.SampleTime
import org.openjdk.jmh.annotations.Mode.SingleShotTime
import org.openjdk.jmh.annotations.OutputTimeUnit
import org.openjdk.jmh.annotations.Param
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.annotations.Warmup
import rpg._
import rpg.cake.scalac.Cake2SkillTrees
import rpg.cake.simple.CakeSkillTrees
import rpg.ooo.OOOSkillTrees
import rpg.patmat._
import rpg.patmat.opt.SteppedOptPatMatSkillTrees
import rpg.patmat.stepped.SteppedPatMatSkillTrees
import rpg.patmat.wrapped.WrappedTrySkillTrees
import rpg.typeclass.naive.TypeclassSkillTrees

class BaselinePatMatSkillTrees() extends SkillTrees {

  override def archeryTree(traits: PlayerTraits): SkillTreeRepr[_] = new SkillTreeRepr {
    @CompilerControl(DONT_INLINE)
    override def totalCost(): Int =
      traits.dexCost + traits.strCost + traits.wisCost +
        traits.dexCost + traits.strCost + traits.wisCost +
        traits.dexCost + traits.strCost + traits.wisCost +
        traits.dexCost
  }

  override def charismaTree(traits: PlayerTraits): SkillTreeRepr[_] = new SkillTreeRepr {
    @CompilerControl(DONT_INLINE)
    override def totalCost(): Int =
      traits.dexCost + traits.strCost + traits.wisCost +
        traits.dexCost + traits.strCost + traits.wisCost +
        traits.dexCost + traits.strCost + traits.wisCost +
        traits.wisCost
  }
}

@State(Scope.Benchmark)
class BenchmarkState(val playerTraits: PlayerTraits) {
  def this() {
    this(new PlayerTraits(100, 100, 100))
  }

  @Param(Array("0"))
  var archeryIndex: Int = 0
  @Param(Array("1"))
  var charismaIndex: Int = 1

  private def trees(provider: SkillTrees): Array[SkillTreeRepr[_]] = Array(provider.archeryTree(playerTraits), provider.charismaTree(playerTraits))

  val patMat = trees(new PatMatSkillTrees)
  val slowPatMat = trees(new SlowPathMatSkillTrees)
  val classesPatMat = trees(new classes.PathMatSkillTrees)
  val steppedPatMat = trees(new SteppedPatMatSkillTrees)
  val steppedClassPatMat = trees(new steppedClass.ImplSkillTrees)
  val steppedOptPatMat = trees(new SteppedOptPatMatSkillTrees)


  val typeclass = trees(new TypeclassSkillTrees)
  val simpleCake = trees(new CakeSkillTrees)
  val scalacCake = trees(new Cake2SkillTrees)

  val scalaOOO = trees(new OOOSkillTrees)
  val oooSubcalls = trees(new ooo.subcalls.OOOSkillTrees)
  val oooLambdas = trees(new ooo.lambdas.LambdasSkillTrees)

  val oooNested = trees(new ooo.nested.OOOSkillTrees)
  val javaOOO = trees(new java.ooo.SkillTrees)
  val javafast = trees(new java.fast.SkillTrees)

  val wrappedInTry = trees(new WrappedTrySkillTrees)

  val baseline = trees(new BaselinePatMatSkillTrees)
}

class BenchDefinitions {
  private type Trees = Array[SkillTreeRepr[_]]

  def run(trees: Trees, bs: BenchmarkState) =
    Array(
      trees(bs.archeryIndex).totalCost(),
      trees(bs.charismaIndex).totalCost(),
      trees(bs.archeryIndex).totalCost(),
      trees(bs.charismaIndex).totalCost()
    )

  def runBaselineProof1(bench: Trees, bs: BenchmarkState) =
    Array(bs.archeryIndex, bs.charismaIndex, bs.archeryIndex, bs.charismaIndex)

  def runBaselineProof2(bench: Trees, bs: BenchmarkState) = Array(
          bench(bs.archeryIndex).totalCost(),
          bench(bs.charismaIndex).totalCost(), bs.archeryIndex, bs.charismaIndex)

  @Benchmark def baseline(bs: BenchmarkState) = run(bs.baseline, bs)  // in Chart.scala

  @Benchmark def baselineProof1(bs: BenchmarkState) = runBaselineProof1(bs.baseline, bs)

  @Benchmark def baselineProof2(bs: BenchmarkState) = runBaselineProof2(bs.baseline, bs)


  @Benchmark def patMat(bs: BenchmarkState) = run(bs.patMat, bs) // in Chart.scala

  @Benchmark def slowPatMat(bs: BenchmarkState) = run(bs.slowPatMat, bs)

  @Benchmark def classesPatMat(bs: BenchmarkState) = run(bs.classesPatMat, bs)

  @Benchmark def steppedPatMat(bs: BenchmarkState) = run(bs.steppedPatMat, bs)

  @Benchmark def steppedClassPatMat(bs: BenchmarkState) = run(bs.steppedClassPatMat, bs)

  @Benchmark def steppedOptClassPatMat(bs: BenchmarkState) = run(bs.steppedOptPatMat, bs)

  @Benchmark def typeclass(bs: BenchmarkState) = run(bs.typeclass, bs) // in Chart.scala

  @Benchmark def simpleCake(bs: BenchmarkState) = run(bs.simpleCake, bs)

  @Benchmark def scalacCake(bs: BenchmarkState) = run(bs.scalacCake, bs) // in Chart.scala

  @Benchmark def scalaOOO(bs: BenchmarkState) = run(bs.scalaOOO, bs)  // in Chart.scala

  @Benchmark def oooSubcalls(bs: BenchmarkState) = run(bs.oooSubcalls, bs)

  @Benchmark def oooLambdas(bs: BenchmarkState) = run(bs.oooLambdas, bs) // in Chart.scala

  @Benchmark def oooNested(bs: BenchmarkState) = run(bs.oooNested, bs)

  @Benchmark def javaOOO(bs: BenchmarkState) = run(bs.javaOOO, bs)  // in Chart.scala

  @Benchmark def javaFast(bs: BenchmarkState) = run(bs.javafast, bs)

  @Benchmark def wrappedInTry(bs: BenchmarkState) = run(bs.wrappedInTry, bs) // in Chart.scala
}



@BenchmarkMode(Array(SampleTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 10, time = 100, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 10, time = 200, timeUnit = TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class HotBenchmark extends BenchDefinitions

@BenchmarkMode(Array(SingleShotTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 1, batchSize = 20)
@Measurement(iterations = 5000, batchSize = 50)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class WarmingBenchmark extends BenchDefinitions


