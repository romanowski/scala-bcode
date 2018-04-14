package rpg.ooo

import rpg.PlayerTraits
import rpg.SkillTreeRepr
import rpg.SkillTrees

sealed trait Node {
  def cost(traits: PlayerTraits): Int
}

case class DexTree(left: Node, right: Node) extends Node {
  override def cost(traits: PlayerTraits): Int = left.cost(traits) + right.cost(traits) + traits.dexCost
}

case class StrTree(left: Node, right: Node) extends Node {
  override def cost(traits: PlayerTraits): Int = left.cost(traits) + right.cost(traits) + traits.strCost
}

case class WisTree(left: Node, right: Node) extends Node {
  override def cost(traits: PlayerTraits): Int = left.cost(traits) + right.cost(traits) + traits.wisCost
}

case class DexStep(to: Node) extends Node {
  override def cost(traits: PlayerTraits): Int = to.cost(traits) + traits.dexCost
}

case class StrStep(to: Node) extends Node {
  override def cost(traits: PlayerTraits): Int = to.cost(traits) + traits.strCost
}

case class WisStep(to: Node) extends Node {
  override def cost(traits: PlayerTraits): Int = to.cost(traits) + traits.wisCost
}

case class DexSkill(name: String) extends Node {
  override def cost(traits: PlayerTraits): Int = traits.dexCost
}

case class StrSkill(name: String) extends Node {
  override def cost(traits: PlayerTraits): Int = traits.strCost
}

case class WisSkill(name: String) extends Node {
  override def cost(traits: PlayerTraits): Int = traits.wisCost
}

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
