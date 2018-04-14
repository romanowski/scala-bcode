package rpg.ooo.lambdas

import rpg.PlayerTraits
import rpg.SkillTreeRepr
import rpg.SkillTrees

sealed trait Node {
  def cost(traits: PlayerTraits): Int
}

case class Tree(feat: PlayerTraits => Int, left: Node, right: Node) extends Node {
  override def cost(traits: PlayerTraits): Int =
    left.cost(traits) + right.cost(traits) + feat(traits)
}

case class Step(feat: PlayerTraits => Int, to: Node) extends Node {
  override def cost(traits: PlayerTraits): Int = to.cost(traits) + feat(traits)
}

case class Skill(feat: PlayerTraits => Int, name: String) extends Node {
  override def cost(traits: PlayerTraits): Int = feat(traits)
}

class OOOLambdas extends SkillTrees {
  override def archeryTree(traits: PlayerTraits): SkillTreeRepr[_] = {
    val tree =
      Tree(_.dexCost,
        Step(_.dexCost,
          Tree(_.wisCost,
            Skill(_.wisCost, "aimed"),
            Step(_.wisCost, Skill(_.dexCost, "rapid"))
          )),
        Tree(_.strCost,
          Skill(_.strCost, "power"),
          Step(_.strCost, Skill(_.dexCost, "double"))
        )
      )
    new SkillTreeRepr(tree) {
      override def totalCost(): Int = tree.cost(traits)
    }
  }

  override def charismaTree(traits: PlayerTraits): SkillTreeRepr[_] = {
    val tree =
      Step(_.wisCost,
        Tree(_.wisCost,
          Skill(_.wisCost, "1"),
          Step(_.dexCost,
            Tree(_.dexCost,
              Skill(_.dexCost, "2"),
              Step(_.strCost,
                Tree(_.strCost,
                  Skill(_.strCost, "3"),
                  Skill(_.wisCost, "4")
                )
              )
            ))
        ))

    new SkillTreeRepr(tree) {
      override def totalCost(): Int = tree.cost(traits)
    }
  }
}
