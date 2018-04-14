package rpg.patmat.wrapped

import rpg.PlayerTraits
import rpg.SkillTreeRepr
import rpg.SkillTrees
import rpg.patmat._

import scala.util.Try

object InTry {
  def totalSkill(node: Node, traits: PlayerTraits): Try[Int] = node match {
    case DexTree(left, right) =>
      for {
        basicCost <- Try(traits.dexCost)
        lelfCost <- totalSkill(left, traits)
        rightCost <- totalSkill(left, traits)
      } yield  basicCost + lelfCost + rightCost
    case StrTree(left, right) =>
      for {
        basicCost <- Try(traits.strCost)
        lelfCost <- totalSkill(left, traits)
        rightCost <- totalSkill(left, traits)
      } yield  basicCost + lelfCost + rightCost
    case WisTree(left, right) =>
      for {
        basicCost <- Try(traits.wisCost)
        lelfCost <- totalSkill(left, traits)
        rightCost <- totalSkill(left, traits)
      } yield  basicCost + lelfCost + rightCost
    case DexStep(next) =>
      for {
        basicCost <- Try(traits.dexCost)
        next <- totalSkill(next, traits)
      } yield  basicCost + next
    case StrStep(next) =>
      for {
        basicCost <- Try(traits.dexCost)
        next <- totalSkill(next, traits)
      } yield  basicCost + next
    case WisStep(next) =>
      for {
        basicCost <- Try(traits.dexCost)
        next <- totalSkill(next, traits)
      } yield  basicCost + next

    case DexSkill(_) =>
      Try(traits.dexCost)
    case StrSkill(_) =>
      Try(traits.strCost)
    case WisSkill(_) =>
      Try(traits.wisCost)
  }
}

class WrappedTrySkillTrees extends SkillTrees with TreeProvider {
  override def archeryTree(traits: PlayerTraits): SkillTreeRepr[_] = {
    new SkillTreeRepr(mkAcheryTree) {
      override def totalCost(): Int = InTry.totalSkill(tree, traits).get
    }
  }
  override def charismaTree(playerTraits: PlayerTraits): SkillTreeRepr[_] = {
    new SkillTreeRepr(mkCharismaTree) {
      override def totalCost(): Int = InTry.totalSkill(tree, playerTraits).get
    }
  }
}