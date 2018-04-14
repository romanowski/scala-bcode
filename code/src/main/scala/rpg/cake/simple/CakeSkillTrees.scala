package rpg.cake.simple

import rpg.PlayerTraits
import rpg.SkillTreeRepr
import rpg.SkillTrees

trait WisNodes  { self: Nodes =>
  case class WisTree(left: Node, right: Node) extends Node
  case class WisStep(to: Node) extends Node
  case class WisSkill(name: String) extends Node
}

trait DexNodes { self: Nodes =>
  case class DexSkill(name: String) extends Node
  case class DexStep(to: Node) extends Node
  case class DexTree(left: Node, right: Node) extends Node
}

trait StrNodes { self: Nodes =>
  case class StrTree(left: Node, right: Node) extends Node
  case class StrStep(to: Node) extends Node
  case class StrSkill(name: String) extends Node
}

trait Nodes extends WisNodes with DexNodes with StrNodes {
  sealed trait Node

  def totalSkill(node: Node, traits: PlayerTraits): Int = node match {
    case DexTree(left, right) =>
      traits.dexCost + totalSkill(left, traits) + totalSkill(right, traits)
    case StrTree(left, right) =>
      traits.strCost + totalSkill(left, traits) + totalSkill(right, traits)
    case WisTree(left, right) =>
      traits.wisCost + totalSkill(left, traits) + totalSkill(right, traits)

    case DexStep(next) =>
      traits.dexCost + totalSkill(next, traits)
    case StrStep(next) =>
      traits.strCost + totalSkill(next, traits)
    case WisStep(next) =>
      traits.wisCost + totalSkill(next, traits)

    case DexSkill(_) =>
      traits.dexCost
    case StrSkill(_) =>
      traits.strCost
    case WisSkill(_) =>
      traits.wisCost
  }
}

object Nodes extends Nodes

class CakeSkillTrees extends SkillTrees {
  //val nodes = new  {}
  //import nodes._
  import Nodes._

  /**
    * Dex --> Dex -> Wis --> Wis("aimed")
    * \                   \-> Dex("rapid")
    * \-> Str --> Str("power")
    * \-> Str -> Dex("double")
    */
  override def archeryTree(traits: PlayerTraits): SkillTreeRepr[_] = {
    val tree = DexTree(
      DexStep(WisTree(
        WisSkill("aimed"),
        WisStep(DexSkill("rapid"))
      )),
      StrTree(
        StrSkill("power"),
        StrStep(DexSkill("double"))
      )
    )
    new SkillTreeRepr(tree) {
      override def totalCost(): Int = totalSkill(tree, traits)
    }
  }

  /** Wis -> Wis --> Wis("1")
    * \-> Dex -> Dex --> Dex("2")
    * \-> Str -> Str --> Str("3")
    * \-> Wis("4")
    */
  override def charismaTree(traits: PlayerTraits): SkillTreeRepr[_] = {
    val tree = WisStep(WisTree(
      WisSkill("1"),
      DexStep(DexTree(
        DexSkill("2"),
        StrStep(
          StrTree(
            StrSkill("3"),
            WisSkill("4")
          )
        )
      ))
    ))
    new SkillTreeRepr(tree) {
      override def totalCost(): Int = totalSkill(tree, traits)
    }
  }
}