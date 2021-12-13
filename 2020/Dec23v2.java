import java.util.ArrayList;
import java.util.HashMap;

public class Dec23v2 {
    public static void main(String[] args) {
        new Dec23v2();
    }

    // O(n+i) (Old one: O(n*i))
    Dec23v2() {
        HashMap<Integer,Node> M = new HashMap<>();
        int[] input = {1, 9, 3, 4, 6, 7, 2, 5, 8};
        // int[] input = {3, 8, 9, 1, 2, 5, 4, 6, 7}; // Test data
        int size = 1000000;

        Node mainNode = new Node(input[0]);
        M.put(input[0], mainNode);

        // O(input)
        for (int i = 1; i < input.length; i++) {
            Node n = new Node(input[i]);
            M.put(input[i], n);
            mainNode.add(n);
        }

        // O(n-input)
        for (int i = input.length; i < size; i++) {
            Node n = new Node(i+1);
            M.put(i+1, n);
            mainNode.add(n);
        }
        // => O(n)

        System.out.println(M.get(3).size());

        // O(i*k) (k=3 => O(i))
        for (int i = 0; i < 10000000; i++) {
            ArrayList<Node> nodes = mainNode.child.take(3); // O(1)
            // System.out.println(String.format("Size: %s", nodes.size()));
            int destination = mainNode.value;
            do {
                destination = destination-1;
                if (destination == 0) destination = size;
            } while (nodes.get(0).contains(destination)); // O(k) (k=3)
            // System.out.println(destination);
            M.get(destination).insertNodesAfter(nodes); // O(1)

            mainNode = mainNode.child;

            if (i % 100000 == 0) System.out.println(String.format("%s%% done", i/100000));
        }
        System.out.println("100% done");

        // System.out.println(mainNode.toString());
        ArrayList<Node> following = M.get(1).child.take(2);
        long result = (long) following.get(0).value * (long) following.get(1).value;
        System.out.println(String.format("Result: %s", result));
    }

    class Node {
        // A circular linked list
        Node parent;
        Node child;
        int  value;

        Node(int value) {
            this.value = value;
            this.parent = this;
            this.child = this;
        }

        // O(n)
        public String toString() {
            ArrayList<Integer> arr = new ArrayList<>();
            Node current = this.child;
            arr.add(this.value);
            while (current != null && current != this) {
                arr.add(current.value);
                current = current.child;
            }

            return arr.toString();
        }

        // Remove n nodes including this one
        // O(n)
        public ArrayList<Node> take(int n) {
            ArrayList<Node> nodes = new ArrayList<>();
            Node current = this;
            for (int i = 0; i < n; i++) {
                nodes.add(current);
                current = current.child;
            }
            current = current.parent;
            this.parent.child = current.child;
            current.child.parent = this.parent;
            this.parent = null;
            current.child = null;
            // System.out.println(nodes.toString());
            return nodes;
        }

        // O(1)
        public void insertNodesAfter(ArrayList<Node> nodes) {
            this.child.parent = nodes.get(nodes.size()-1);
            nodes.get(nodes.size()-1).child = this.child;
            nodes.get(0).parent = this;
            this.child = nodes.get(0);
        }

        // O(1)
        public void add(Node n) {
            n.parent = this.parent;
            n.parent.child = n;
            n.child = this;
            this.parent = n;
        }

        // O(n)
        public boolean contains(int value) {
            if (this.value == value) return true;
            Node current = this.child;
            while (current != null && current != this) {
                if (current.value == value) return true;
                current = current.child;
            }
            return false;
        }

        // O(n)
        public int size() {
            int n = 1;
            Node current = this.child;
            while (current != null && current != this) {
                n++;
                current = current.child;
            }
            return n;
        }
    }
}
