package rpg.java.fast;


import rpg.PlayerTraits;
import rpg.SkillTreeRepr;

final class Node {
    private final Node left;
    private final Node right;
    private final String name;
    private final boolean dex;
    private final boolean str;

    Node(boolean dex, boolean str, Node left, Node right, String name) {
        this.left = left;
        this.right = right;
        this.name = name;
        this.str = str;
        this.dex = dex;
    }

    int cost(PlayerTraits traits) {
        int cost = 0;
        if (dex) {
            cost = traits.dexCost();
        } else if (str) {
            cost = traits.strCost();
        } else {
            cost = traits.wisCost();
        }

        if (left != null) {
            cost = cost + left.cost(traits);
            if (right != null) {
                cost = cost + right.cost(traits);
            }
        }

        return cost;
    }

}

public class SkillTrees extends rpg.SkillTrees {


    @Override
    public SkillTreeRepr<?> archeryTree(PlayerTraits playerTraits) {
        Node tree = new Node(
                true, false,
                new Node(
                        false, false,
                        new Node(
                                true, false,
                                new Node(false, false, null, null, "aimed"),
                                new Node(false, false,
                                        new Node(true, false, null, null, "rapid"),
                                        null, null)
                                , null
                        ),
                        null, null),
                new Node(false, true,
                        new Node(false, true, null, null,"power"),
                        new Node(false, true,
                                new Node(true, false, null, null,"double"), null, null),
                        null
                ),
                null
        );


        return new SkillTreeRepr<Node>(tree) {
            @Override
            public int totalCost() {
                return tree.cost(playerTraits);
            }
        };
    }

    @Override
    public SkillTreeRepr<?> charismaTree(PlayerTraits traits) {
        Node tree = new Node(
          true, false,
          new Node(
            false, false,
            new Node(
              true, false,
              new Node(false, false, null, null, "aimed"),
              new Node(false, false,
                new Node(true, false, null, null, "rapid"),
                null, null)
              , null
            ),
            null, null),
          new Node(false, true,
            new Node(false, true, null, null,"power"),
            new Node(false, true,
              new Node(false, false, null, null,"double"), null, null),
            null
          ),
          null
        );


        return new SkillTreeRepr<Node>(tree) {
            @Override
            public int totalCost() {
                return tree.cost(traits);
            }
        };
    }
}