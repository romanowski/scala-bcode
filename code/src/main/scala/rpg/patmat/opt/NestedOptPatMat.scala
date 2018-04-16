package rpg.patmat.opt

import rpg._

sealed trait Node {
  val id: Int
}

trait Tree extends Node {
  final val id = Node.TreeType
}

case class DexTree(left: Node, right: Node) extends Tree

case class StrTree(left: Node, right: Node) extends Tree

case class WisTree(left: Node, right: Node) extends Tree

trait Step extends Node {
  final val id = Node.StepType
}

case class DexStep(to: Node) extends Step

case class StrStep(to: Node) extends Step

case class WisStep(to: Node) extends Step

trait Skill extends Node {
  final val id = Node.SkillType
}

case class DexSkill(name: String) extends Skill

case class StrSkill(name: String) extends Skill

case class WisSkill(name: String) extends Skill

object Node {
  final val TreeType = 1
  final val StepType = 2  
  final val SkillType = 4

  def totalSkill(node: Node, traits: PlayerTraits): Int = node.id match {
    case TreeType => node match {
      case DexTree(left, right) =>
        traits.dexCost + totalSkill(left, traits) + totalSkill(right, traits)
      case StrTree(left, right) =>
        traits.strCost + totalSkill(left, traits) + totalSkill(right, traits)
      case WisTree(left, right) =>
        traits.wisCost + totalSkill(left, traits) + totalSkill(right, traits)
    }
    case StepType => node match {
      case DexStep(next) =>
        traits.dexCost + totalSkill(next, traits)
      case StrStep(next) =>
        traits.strCost + totalSkill(next, traits)
      case WisStep(next) =>
        traits.wisCost + totalSkill(next, traits)
    }
    case SkillType => node match {
      case DexSkill(_) =>
        traits.dexCost
      case StrSkill(_) =>
        traits.strCost
      case WisSkill(_) =>
        traits.wisCost
    }
  }
}

class NestedOptPatMat extends SkillTrees {

  override def archeryTree(traits: PlayerTraits): SkillTreeRepr[_] = {
    val tree =
      DexTree(
        DexStep(WisTree(
          WisSkill("aimed"),
          WisStep(DexSkill("rapid")))),
        StrTree(
          StrSkill("power"),
          StrStep(DexSkill("double"))))

    new SkillTreeRepr(tree) {
      override def totalCost(): Int = Node.totalSkill(tree, traits)
    }
  }

  override def charismaTree(traits: PlayerTraits): SkillTreeRepr[_] = {
    val tree =
      WisStep(WisTree(
        WisSkill("1"),
        DexStep(DexTree(
          DexSkill("2"),
          StrStep(
            StrTree(
              StrSkill("3"),
              WisSkill("4")))))))

    new SkillTreeRepr(tree) {
      override def totalCost(): Int = Node.totalSkill(tree, traits)
    }
  }
}
