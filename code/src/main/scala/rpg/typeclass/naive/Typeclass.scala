package rpg.typeclass.naive

import rpg.PlayerTraits
import rpg.SkillTreeRepr
import rpg.SkillTrees

sealed trait Node

case class DexTree[L, R](left: L, right: R) extends Node

case class StrTree[L, R](left: L, right: R) extends Node

case class WisTree[L, R](left: L, right: R) extends Node

case class DexStep[N](to: N) extends Node

case class StrStep[N](to: N) extends Node

case class WisStep[N](to: N) extends Node

case class DexSkill(name: String) extends Node

case class StrSkill(name: String) extends Node

case class WisSkill(name: String) extends Node

object Node {
  implicit val wisSkillSummer = new Summer[WisSkill] {
    override def sumExp(tree: WisSkill, traits: PlayerTraits): Int = traits.wisCost
  }
  implicit val strSkillSummer = new Summer[StrSkill] {
    override def sumExp(tree: StrSkill, traits: PlayerTraits): Int = traits.strCost
  }
  implicit val dexSkillSummer = new Summer[DexSkill] {
    override def sumExp(tree: DexSkill, traits: PlayerTraits): Int = traits.dexCost
  }

  implicit def wisStepSummer[T: Summer] = new Summer[WisStep[T]] {
    override def sumExp(tree: WisStep[T], traits: PlayerTraits): Int =
      implicitly[Summer[T]].sumExp(tree.to, traits) + traits.wisCost
  }

  implicit def strStepSummer[T: Summer] = new Summer[StrStep[T]] {
    override def sumExp(tree: StrStep[T], traits: PlayerTraits): Int =
      implicitly[Summer[T]].sumExp(tree.to, traits) + traits.strCost
  }

  implicit def dexStepSummer[T: Summer] = new Summer[DexStep[T]] {
    override def sumExp(tree: DexStep[T], traits: PlayerTraits): Int =
      implicitly[Summer[T]].sumExp(tree.to, traits) + traits.dexCost
  }

  implicit def wisTreeSummer[L: Summer, R: Summer] = new Summer[WisTree[L, R]] {
    override def sumExp(tree: WisTree[L, R], traits: PlayerTraits): Int =
      implicitly[Summer[L]].sumExp(tree.left, traits) +
        implicitly[Summer[R]].sumExp(tree.right, traits) +
        traits.wisCost
  }

  implicit def strTreeSummer[L: Summer, R: Summer] = new Summer[StrTree[L, R]] {
    override def sumExp(tree: StrTree[L, R], traits: PlayerTraits): Int =
      implicitly[Summer[L]].sumExp(tree.left, traits) +
        implicitly[Summer[R]].sumExp(tree.right, traits) +
        traits.strCost
  }

  implicit def dexTreeSummer[L: Summer, R: Summer] = new Summer[DexTree[L, R]] {
    override def sumExp(tree: DexTree[L, R], traits: PlayerTraits): Int =
      implicitly[Summer[L]].sumExp(tree.left, traits) +
        implicitly[Summer[R]].sumExp(tree.right, traits) +
        traits.dexCost
  }

  trait Summer[T] {
    def sumExp(tree: T, traits: PlayerTraits): Int
  }

  def totalSkill[T: Summer](tree: T, traits: PlayerTraits): Int = implicitly[Summer[T]].sumExp(tree, traits)
}

class Typeclass extends SkillTrees {

  import Node._

  override def archeryTree(traits: PlayerTraits): SkillTreeRepr[_] = {
    val dexTree = DexTree(
      DexStep(WisTree(
        WisSkill("aimed"),
        WisStep(DexSkill("rapid")))),
      StrTree(
        StrSkill("power"),
        StrStep(DexSkill("double"))))

    new SkillTreeRepr(dexTree) {
      override def totalCost(): Int = Node.totalSkill(tree, traits)
    }
  }

  override def charismaTree(traits: PlayerTraits): SkillTreeRepr[_] = {
    val wisTree = WisStep(WisTree(
      WisSkill("1"),
      DexStep(DexTree(
        DexSkill("2"),
        StrStep(
          StrTree(
            StrSkill("3"),
            WisSkill("4")))))))

    new SkillTreeRepr(wisTree) {
      override def totalCost(): Int = Node.totalSkill(tree, traits)
    }

  }
}
