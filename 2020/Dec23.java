public class Dec23 {
    public static void main(String[] args) {
        // int[] input = {1, 9, 3, 4, 6, 7, 2, 5, 8};
        // int[] input = {3, 8, 9, 1, 2, 5, 4, 6, 7}; // Test data
        int[] input = new int[1000000];
        input[0] = 1; input[1] = 9; input[2] = 3;
        input[3] = 4; input[4] = 6; input[5] = 7;
        input[6] = 2; input[7] = 5; input[8] = 8;
        for (int i = 9; i < input.length; i++) {
            input[i] = i+1;
        }

        int current = 0;
        // System.out.println("Start: " + Arrays.toString(input));
        for (int i = 0; i < 10000000; i++) {
            simulate(input, (current + i) % input.length);
            // System.out.println(String.format("Simulation %s done: "+ Arrays.toString(input), i+1));
            if (i % 100 == 0) System.out.println(String.format("%s%% done", i/100000.0));
        }
        int one = indexOf(input, 1);
        int t1 = input[(one+1) % input.length];
        int t2 = input[(one+2) % input.length];
        System.out.println(String.format("Index of 1: %s, t1: %s, t2: %s, product: %s", one, t1, t2, t1*t2));
    }

    static void simulate(int[] array, int current) {
        int len = array.length;

        int selFront = (current + 1) % len;
        int selBack = (current + 3) % len;

        int value = array[current] - 1;
        if (value == 0) value = array.length;
        int destination = indexOf(array, value);
        if (value == 1) value = array.length;
        else value--;

        if (selBack < selFront) { // wraps around
            // System.out.println("first");
            while (destination <= selBack || destination >= selFront) {
                destination = indexOf(array, value);
                if (value == 1) value = array.length;
                else value--;
            }
        } else {
            // System.out.println("second");
            while (destination <= selBack && destination >= selFront) {
                destination = indexOf(array, value);
                if (value == 1) value = array.length;
                else value--;
            }
        }

        // System.out.println(String.format("Destination nr %s at index %s", array[destination], destination));

        // O(n)
        for (int i = 3; i >= 1; i--) {
            int pos = (current + i) % len;
            while (pos != (destination) % len) {
                int temp = array[(pos+1) % len];
                array[(pos+1) % len] = array[pos];
                array[pos] = temp;
                pos = (pos+1) % len;
            }
        }
        int temp = array[destination];
        array[destination] = array[(destination-2+len) % len];
        array[(destination-2+len) % len]= temp;

    }

    // O(n)
    static int indexOf(int[] arr, int n) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == n) return i;
        }
        System.out.println("Not found");
        return -1;
    }
}
