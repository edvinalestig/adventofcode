import java.io.File;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Scanner;

public class Dec22 {
    public static void main(String[] args) throws Exception {
        Queue<Integer> p1 = new LinkedList<Integer>();
        Queue<Integer> p2 = new LinkedList<Integer>();

        ArrayList<String> lines = readLines("22dec.txt");
        boolean first = true;
        for (String l : lines) {
            if (first) {
                if (l.contains("Player 2")) first = false;
                else if (l.contains("Player 1")) continue;
                else {
                    p1.add(Integer.parseInt(l));
                }
            } else {
                p2.add(Integer.parseInt(l));
            }
        }

        System.out.println(p1.hashCode());
        System.out.println(p2.hashCode());

        System.out.println(recursiveCombat(p1, p2));
    }

    static int part1(Queue<Integer> p1, Queue<Integer> p2) {
        while (!p2.isEmpty() && !p1.isEmpty()) {
            int card1 = p1.poll();
            int card2 = p2.poll();
            if (card1 > card2) {
                // System.out.println("P1 won");
                p1.add(card1);
                p1.add(card2);
            } else {
                // System.out.println("P2 won");
                p2.add(card2);
                p2.add(card1);
            }
            // System.out.println(p1);
            // System.out.println(p2);
        }
        System.out.println("Game ended");
        Queue<Integer> winner = p2.isEmpty() ? p1 : p2;
        int points = 0;
        while (!winner.isEmpty()) {
            points += winner.size() * winner.poll();
        }

        return points;
    }

    static int recursiveCombat(Queue<Integer> p1, Queue<Integer> p2) {
        ArrayList<Integer> previousP1 = new ArrayList<Integer>();
        ArrayList<Integer> previousP2 = new ArrayList<Integer>();

        while (!p2.isEmpty() && !p1.isEmpty()) {
            // If p1/p2 has exact copy of previous p1/p2 => p1 wins
            int p1hash = p1.hashCode();
            if (previousP1.contains(p1hash)) return 1;
            else previousP1.add(p1hash);

            int p2hash = p2.hashCode();
            if (previousP2.contains(p2hash)) return 1;
            else previousP2.add(p2hash);

            int card1 = p1.poll();
            int card2 = p2.poll();
            int winner;
            if (card1 <= p1.size() && card2 <= p2.size()) {
                winner = recursiveCombat(take(card1, p1), take(card2, p2)); // new recursive game
            } else winner = card1 > card2 ? 1 : 2;

            if (winner == 1) {
                p1.add(card1);
                p1.add(card2);
            } else {
                p2.add(card2);
                p2.add(card1);
            }
        }

        // System.out.println("Game ended!");
        int w = p2.isEmpty() ? 1 : 2;
        Queue<Integer> winner = p2.isEmpty() ? p1 : p2;
        int points = 0;
        while (!winner.isEmpty()) {
            points += winner.size() * winner.poll();
        }
        System.out.println("Points: " + Integer.toString(points));
        return w;
    }

    static <T> Queue<T> take(int n, Queue<T> l) {
        // System.out.println(Integer.toString(l.size()) + " size, n: " + Integer.toString(n));
        LinkedList<T> l1 = new LinkedList<T>(l);
        Queue<T> l2 = new LinkedList<T>();
        for (int i = 0; i < n; i++) {
            l2.add(l1.get(i));
        }
        return l2;
    }

    static ArrayList<String> readLines(String filepath) throws Exception {
        File file = new File(filepath);
        Scanner scanner = new Scanner(file);
        ArrayList<String> allLines = new ArrayList<String>();

        while (scanner.hasNextLine()) {
            allLines.add(scanner.nextLine());
        }
        scanner.close();

        return allLines;
    }
}
