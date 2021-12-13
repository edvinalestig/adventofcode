import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Scanner;

public class Dec5 {
    public static void main(String[] args) throws Exception {
        ArrayList<String> lines = readLines("5dec.txt");
        ArrayList<Integer> seatIDs = new ArrayList<Integer>();
        for (String seat : lines) {
            int rowStart = 0;
            int rowEnd = 127;
            int colStart = 0;
            int colEnd = 7;
            for (int i = 0; i < seat.length(); i++) {
                char c = seat.charAt(i);
                switch (c) {
                    case 'F': 
                        rowEnd -= (rowEnd +1 - rowStart)/2;
                        break;
                    case 'B':
                        rowStart += (rowEnd +1 - rowStart)/2;
                        break;
                    case 'L':
                        colEnd -= (colEnd +1 - colStart)/2;
                        break;
                    case 'R':
                        colStart += (colEnd +1 - colStart)/2;
                        break;
                }

            }
            seatIDs.add(rowEnd*8 + colEnd);
        }

        System.out.println("Max: " + Integer.toString(Collections.max(seatIDs)));
        ArrayList<Integer> missing = new ArrayList<Integer>();

        for (int i = 1; i < 127*8+6; i++) {
            if (seatIDs.contains(i-1) && !seatIDs.contains(i) && seatIDs.contains(i+1)) {
                missing.add(i);
            }
        }
        System.out.println(missing.toString());
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
