package rpg

import org.scalatest._
import rpg.bench.BenchmarkState

class SkillTreesTests extends FlatSpec with Matchers {

  case object EqualTraits extends PlayerTraits(100, 100, 100)

  case object StrTraits extends PlayerTraits(100, 0, 0)

  case object DexTraits extends PlayerTraits(0, 100, 0)

  case object WisTraits extends PlayerTraits(0, 0, 100)

  type Ret = Array[SkillTreeRepr[_]]
  val methods = classOf[BenchmarkState].getMethods
    .filter(_.getReturnType == classOf[Ret])

  for {
    method <- methods
    (traits, expected) <- Seq(
      EqualTraits -> (1000, 1000),
      StrTraits -> (300, 300),
      DexTraits -> (400, 300),
      WisTraits -> (300, 400)
    )
  } s"Benchmarks named ${method.getName} for $traits" should "work" in {
    val state = new BenchmarkState(traits)
    val Array(a, b) = method.invoke(state).asInstanceOf[Ret]
    a.totalCost() shouldBe expected._1
    b.totalCost() shouldBe expected._2
  }

  s"Benchmarks" should "contains methids" in {
    methods should not be empty
  }
}
