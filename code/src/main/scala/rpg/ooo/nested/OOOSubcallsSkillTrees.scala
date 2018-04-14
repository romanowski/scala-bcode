package rpg.ooo.nested

import rpg.PlayerTraits
import rpg.SkillTreeRepr
import rpg.SkillTrees

trait Node {
	def cost(traits: PlayerTraits): Int
}

trait FeatTree {
	def thisCost(traits: PlayerTraits): Int

	case class Tree(left: Node, right: Node) extends Node {
		override def cost(traits: PlayerTraits): Int =
			left.cost(traits) + right.cost(traits) + thisCost(traits)
	}

	case class Step(to: Node) extends Node {
		override def cost(traits: PlayerTraits): Int =
			to.cost(traits) + thisCost(traits)
	}

	case class Skill(name: String) extends Node {
		override def cost(traits: PlayerTraits): Int = thisCost(traits)
	}
}

object Dex extends FeatTree {
	override def thisCost(traits: PlayerTraits): Int = traits.dexCost
}

object Str extends FeatTree {
	override def thisCost(traits: PlayerTraits): Int = traits.strCost
}

object Wis extends FeatTree {
	override def thisCost(traits: PlayerTraits): Int = traits.wisCost
}


class OOOSkillTrees extends SkillTrees {
  override def archeryTree(traits: PlayerTraits): SkillTreeRepr[_] = {
    val tree: Node = Dex.Tree(
      Dex.Step(Wis.Tree(
        Wis.Skill("aimed"),
        Wis.Step(Dex.Skill("rapid"))
      )),
      Str.Tree(
        Str.Skill("power"),
        Str.Step(Dex.Skill("double"))
      )
    )
    new SkillTreeRepr(tree) {
      override def totalCost(): Int = tree.cost(traits)
    }
  }

  override def charismaTree(traits: PlayerTraits): SkillTreeRepr[_] = {
    val tree: Node = Wis.Step(Wis.Tree(
      Wis.Skill("1"),
      Dex.Step(Dex.Tree(
        Dex.Skill("2"),
        Str.Step(
          Str.Tree(
            Str.Skill("3"),
            Wis.Skill("4")
          )
        )
      ))
    ))
    new SkillTreeRepr(tree) {
      override def totalCost(): Int = tree.cost(traits)
    }
  }
}
