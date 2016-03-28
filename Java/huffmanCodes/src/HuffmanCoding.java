import java.util.PriorityQueue;

abstract class Node implements Comparable<Node> {
    final int frequency;

    Node(int frequency) {
        this.frequency = frequency;
    }

    @Override
    public int compareTo(Node n) {
        return frequency - n.frequency;
    }
}

class InternalNode extends Node {
    final Node left, right;

    InternalNode(Node left, Node right, int frequency) {
        super(frequency);
        this.left = left;
        this.right = right;
    }
}

class Leaf extends Node {
    final char character;

    Leaf(char character, int frequency) {
        super(frequency);
        this.character = character;
    }
}

class CharStat {
    final int frequency;
    final String codeWord;

    CharStat(int frequency, String codeWord) {
        this.frequency = frequency;
        this.codeWord = codeWord;
    }
}

public class HuffmanCoding {
    private final CharStat[] charStats;

    HuffmanCoding(String s) {
        charStats = buildCharStats(buildHuffmanTree(s));
    }

    private Node buildHuffmanTree(String s) {
        PriorityQueue<Node> pq = buildQueue(calcFrequency(s));
        while (pq.size() > 1) {
            Node l = pq.poll();
            Node r = pq.poll();
            pq.offer(new InternalNode(l, r, l.frequency + r.frequency));
        }
        return pq.poll();
    }

    private int[] calcFrequency(String s) {
        final int maxAsciiCodes = 256;
        int[] frequencies = new int[maxAsciiCodes];

        for (char c : s.toCharArray())
            frequencies[c]++;

        return frequencies;
    }

    private CharStat[] buildCharStats(Node rootOfTree) {
        final int maxAsciiCodes = 256;
        CharStat[] stats = new CharStat[maxAsciiCodes];

        if (rootOfTree != null)
            buildCharStatsRecursive(rootOfTree, stats, "");

        return stats;
    }

    private void buildCharStatsRecursive(Node node, CharStat[] charStats, String codeWord) {
        if (node instanceof InternalNode) {
            InternalNode n = (InternalNode) node;
            buildCharStatsRecursive(n.left, charStats, codeWord + 0);
            buildCharStatsRecursive(n.right, charStats, codeWord + 1);
        } else if (node instanceof Leaf) {
            Leaf l = (Leaf) node;
            charStats[l.character] = new CharStat(l.frequency, codeWord);
        }
    }

    private void printCharacterStats() {
        final String format = "%-12s%-12s%-12s";
        System.out.format(format, "Character", "Frequency", "CodeWord");
        System.out.println();

        for (int i = 0; i < charStats.length; ++i) {
            if (charStats[i] != null) {
                String s = String.format(format, (char) i, charStats[i].frequency, charStats[i].codeWord);
                System.out.println(s);
            }
        }
    }

    private PriorityQueue<Node> buildQueue(int[] frequencies) {
        PriorityQueue<Node> pq = new PriorityQueue<>();

        for (int i = 0; i < frequencies.length; ++i)
            if (frequencies[i] > 0)
                pq.offer(new Leaf((char) i, frequencies[i]));

        return pq;
    }

    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("Provide the string to encode as an argument.");
            System.exit(1);
        }
        HuffmanCoding hc = new HuffmanCoding(args[0]);
        hc.printCharacterStats();
    }

    int getFrequency(char c) {
        if (charStats[c] != null)
            return charStats[c].frequency;

        return -1;
    }

    String getCodeWord(char c) {
        if (charStats[c] != null)
            return charStats[c].codeWord;

        return "";
    }
}
