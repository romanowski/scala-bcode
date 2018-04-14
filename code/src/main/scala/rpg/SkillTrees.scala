package rpg

import org.openjdk.jmh.annotations.CompilerControl
import org.openjdk.jmh.annotations.CompilerControl.Mode.DONT_INLINE

class PlayerTraits(str : Int, dex : Int, wis : Int) {
  @CompilerControl(DONT_INLINE) def strCost: Int = str
  @CompilerControl(DONT_INLINE) def dexCost: Int = dex
  @CompilerControl(DONT_INLINE) def wisCost: Int = wis
}

abstract class SkillTreeRepr[T](val tree: T) {
  @CompilerControl(DONT_INLINE) def totalCost(): Int
}

abstract class SkillTrees {
  /** Dex --> Dex -> Wis --> Wis("aimed")
    *      \              \-> Wis -> Dex("rapid")
    *       \-> Str --> Str("power")
    *               \-> Str -> Dex("double") */
  def archeryTree(playerTraits: PlayerTraits): SkillTreeRepr[_]

  /** Wis -> Wis --> Wis("1")
    *            \-> Dex -> Dex --> Dex("2")
    *                           \-> Str -> Str --> Str("3")
    *                                          \-> Wis("4") */
  def charismaTree(traits: PlayerTraits): SkillTreeRepr[_]
}