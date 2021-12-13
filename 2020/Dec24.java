import java.io.File;
import java.util.ArrayList;
import java.util.Scanner;

public class Dec24 {
    public static void main(String[] args) throws Exception {
        ArrayList<int[]> blacks = new ArrayList<>();
        ArrayList<String> commands = readLines("24dec.txt");

        char storage = ' ';
        for (String cmd : commands) {
            int NS = 0;
            int EW = 0;

            for (char c : cmd.toCharArray()) {
                if (c == 'n' || c == 's') {
                    storage = c;
                    continue;
                } 
                switch (storage) {
                    case 'n':
                        NS++;
                        if (c == 'e') EW++;
                        else EW--;
                        break;
                    case 's':
                        NS--;
                        if (c == 'e') EW++;
                        else EW--;
                        break;
                    default:
                        if (c == 'e') EW+=2;
                        else EW-=2;
                        break;
                }
                storage = ' ';
            }

            int[] coords = {NS,EW};
            boolean found = false;
            for (int i = 0; i < blacks.size(); i++) {
                if (blacks.get(i)[0] == NS && blacks.get(i)[1] == EW) {
                    blacks.remove(i);
                    found = true;
                    break;
                }
            } 
            if (!found) {
                blacks.add(coords);
            }
            
        }

        System.out.println(blacks.size());
        // System.out.println(blacks.toString());
        // for (int[] arr : blacks) {
        //     System.out.println("(" + arr[0] + "," + arr[1] + ")");
        // }

        for (int i = 0; i < 100; i++) {
            part2(blacks);
        }
        System.out.println(blacks.size());

    }

    static void part2(ArrayList<int[]> blacks) {
        ArrayList<int[]> output = new ArrayList<>();

        blacks = output;
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

    public class Hex {
        int NS;
        int EW;
        boolean black;

        Hex() {
            NS = 0;
            EW = 0;
            black = false;
        }

        void ne() {
            NS++;
            EW++;
        }

        void nw() {
            NS++;
            EW--;
        }

        void e() {
            EW++;
        }

        void w() {
            EW--;
        }

        void sw() {
            NS--;
            EW--;
        }

        void se() {
            NS--;
            EW++;
        }
    }
}
