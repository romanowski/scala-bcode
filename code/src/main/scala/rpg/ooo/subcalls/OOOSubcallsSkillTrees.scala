package rpg.ooo.subcalls

import rpg.PlayerTraits
import rpg.SkillTreeRepr
import rpg.SkillTrees

sealed trait Node {
  def cost(traits: PlayerTraits): Int
}

trait FeatBased {
	def thisCost(traits: PlayerTraits): Int
}


trait DexBased extends FeatBased {
	override def thisCost(traits: PlayerTraits): Int = traits.dexCost
}
trait StrBased extends FeatBased {
	override def thisCost(traits: PlayerTraits): Int = traits.strCost
}
trait WisBased extends FeatBased {
	override def thisCost(traits: PlayerTraits): Int = traits.wisCost
}

trait Tree extends Node with FeatBased {
	def left: Node
	def right: Node

	override def cost(traits: PlayerTraits): Int =
		left.cost(traits) + right.cost(traits) + thisCost(traits)
}

case class DexTree(left: Node, right: Node) extends Tree with DexBased

case class StrTree(left: Node, right: Node) extends Tree with StrBased

case class WisTree(left: Node, right: Node) extends Tree with WisBased

trait Step extends Node with FeatBased {
	def to: Node

	override def cost(traits: PlayerTraits): Int =
		to.cost(traits) + thisCost(traits)
}

case class DexStep(to: Node) extends Step with DexBased

case class StrStep(to: Node) extends Step with StrBased

case class WisStep(to: Node) extends Step with WisBased

trait Skill extends Node with FeatBased {
	override def cost(traits: PlayerTraits): Int = thisCost(traits)
}

case class DexSkill(name: String) extends Skill with DexBased

case class StrSkill(name: String) extends Skill with StrBased

case class WisSkill(name: String) extends Skill with WisBased

class OOOSkillTrees extends SkillTrees {
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
      override def totalCost(): Int = tree.cost(traits)
    }
  }

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
      override def totalCost(): Int = tree.cost(traits)
    }
  }
}
