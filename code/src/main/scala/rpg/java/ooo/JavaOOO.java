package rpg.java.ooo;

import rpg.PlayerTraits;
import rpg.SkillTreeRepr;

interface Node {
    int cost(PlayerTraits traits);
}

class StrTree implements Node {
    Node left;
    Node right;

    StrTree(Node left, Node right) {
        this.left = left;
        this.right = right;

    }

    @Override
    public int cost(PlayerTraits traits) {
        return left.cost(traits) + right.cost(traits) + traits.strCost();
    }
}

class DexTree implements Node {
    Node left;
    Node right;

    DexTree(Node left, Node right) {
        this.left = left;
        this.right = right;

    }

    @Override
    public int cost(PlayerTraits traits) {
        return left.cost(traits) + right.cost(traits) + traits.dexCost();
    }
}

class WisTree implements Node {
    Node left;
    Node right;

    WisTree(Node left, Node right) {
        this.left = left;
        this.right = right;
    }

    @Override
    public int cost(PlayerTraits traits) {
        return left.cost(traits) + right.cost(traits) + traits.wisCost();
    }
}


class DexStep implements Node {
    Node to;
    DexStep(Node to){
        this.to = to;
    }

    @Override
    public int cost(PlayerTraits traits) {
        return to.cost(traits) + traits.dexCost();
    }
}

class StrStep implements Node {
    Node to;
    StrStep(Node to){
        this.to = to;
    }

    @Override
    public int cost(PlayerTraits traits) {
        return to.cost(traits) + traits.strCost();
    }
}

class WisStep implements Node {
    Node to;
    WisStep(Node to){
        this.to = to;
    }

    @Override
    public int cost(PlayerTraits traits) {
        return to.cost(traits) + traits.wisCost();
    }
}


class DexSkill implements Node {
    String name;

    DexSkill(String name){
        this.name = name;
    }

    @Override
    public int cost(PlayerTraits traits) {
        return traits.dexCost();
    }
}


class StrSkill implements Node {
    String name;

    StrSkill(String name){
        this.name = name;
    }

    @Override
    public int cost(PlayerTraits traits) {
        return traits.strCost();
    }
}


class WisSkill implements Node {
    String name;

    WisSkill(String name){
        this.name = name;
    }

    @Override
    public int cost(PlayerTraits traits) {
        return traits.wisCost();
    }
}


public class JavaOOO extends rpg.SkillTrees {
    @Override
    public SkillTreeRepr<?> archeryTree(PlayerTraits playerTraits) {
        Node tree = new DexTree(
                new DexStep(new WisTree(
                        new WisSkill("aimed"),
                        new WisStep(new DexSkill("rapid"))
                )),
                new StrTree(
                        new StrSkill("power"),
                        new StrStep(new DexSkill("double"))
                )
        );


        return new SkillTreeRepr<Node>(tree) {
            @Override
            public int totalCost() {
                return tree.cost(playerTraits);
            }
        };
    }

    @Override
    public SkillTreeRepr<?> charismaTree(PlayerTraits playerTraits) {
        Node tree = new WisStep(new WisTree(
          new WisSkill("1"),
          new DexStep(new DexTree(
            new DexSkill("2"),
            new StrStep(
              new StrTree(
                new StrSkill("3"),
                new WisSkill("4")
              )
            )
          ))
        ));

        return new SkillTreeRepr<Node>(tree) {
            @Override
            public int totalCost() {
                return tree.cost(playerTraits);
            }
        };
    }
}
