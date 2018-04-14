package rpg.patmat

import rpg.PlayerTraits
import rpg.SkillTreeRepr
import rpg.SkillTrees

object SlowNode {
  def totalSkill(node: Node, traits: PlayerTraits): Int = node match {
    case StrSkill(_) =>
      traits.strCost
    case WisSkill(_) =>
      traits.wisCost
    case DexSkill(_) =>
      traits.dexCost

    case StrStep(next) =>
      traits.strCost + totalSkill(next, traits)
    case WisStep(next) =>
      traits.wisCost + totalSkill(next, traits)
    case DexStep(next) =>
      traits.dexCost + totalSkill(next, traits)

    case StrTree(left, right) =>
      traits.strCost + totalSkill(left, traits) + totalSkill(right, traits)
    case WisTree(left, right) =>
      traits.wisCost + totalSkill(left, traits) + totalSkill(right, traits)
    case DexTree(left, right) =>
      traits.dexCost + totalSkill(left, traits) + totalSkill(right, traits)
  }
}

class SlowPathMat extends SkillTrees with TreeProvider {
  override def archeryTree(traits: PlayerTraits): SkillTreeRepr[_] = {
    new SkillTreeRepr(mkArcheryTree) {
      override def totalCost(): Int = TailRecNode.totalSkill(tree, traits)
    }
  }
  override def charismaTree(traits: PlayerTraits): SkillTreeRepr[_] = {
    new SkillTreeRepr(mkCharismaTree) {
      override def totalCost(): Int = TailRecNode.totalSkill(tree, traits)
    }
  }
}