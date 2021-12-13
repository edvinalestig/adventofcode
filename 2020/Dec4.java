import java.io.*;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.regex.*;

public class Dec4 {
    public static void main(String[] args) throws Exception {
        File file = new File("4dec.txt");
        Scanner scanner = new Scanner(file);
        ArrayList<String> passports = new ArrayList<String>();
        ArrayList<String> allLines = new ArrayList<String>();

        while (scanner.hasNextLine()) {
            allLines.add(scanner.nextLine());
        }
        scanner.close();

        String temp = "";
        for (String line : allLines) {
            if (line == "") {
                passports.add(temp);
                temp = "";
            } else {
                temp += line;
                temp += " ";
            }
        }
        if (temp != "") {
            passports.add(temp);
        }
        
        // System.out.println(part2Count(part2GetStrings(passports)));
        System.out.println(part2Countv2(passports));
    }

    static int part1Count(ArrayList<String> passports) {
        int count = 0;
        for (String s : passports) {
            System.out.println(s);
            if (s.contains("byr:") && s.contains("iyr:") && s.contains("eyr:") && s.contains("hgt:")
             && s.contains("hcl:") && s.contains("ecl:") && s.contains("pid:")) {
                count++;
            } 
        }
        return count;
    }

    static int part2Count(ArrayList<String> passports) {
        int count = 0;
        for (String s : passports) {
            String[] parts = s.split(" ");
            boolean byr = false;
            boolean iyr = false;
            boolean eyr = false;
            boolean hgt = false;
            boolean hcl = false;
            boolean ecl = false;
            boolean pid = false;
            for (String p : parts) {
                String[] a = p.split(":");
                if (Pattern.matches("byr", a[0])) {
                    int num = Integer.parseInt(a[1]);
                    if (num >= 1920 && num <= 2002) {
                        byr = true;
                    } else {
                        System.out.println("Break for byr: " + Integer.toString(num));
                        break;
                    }
                } else if (Pattern.matches("iyr", a[0])) {
                    int num = Integer.parseInt(a[1]);
                    if (num >= 2010 && num <= 2020) {
                        iyr = true;
                    } else {
                        System.out.println("Break for iyr: " + Integer.toString(num));
                        break;
                    }
                } else if (Pattern.matches("eyr", a[0])) {
                    int num = Integer.parseInt(a[1]);
                    if (num >= 2020 && num <= 2030) {
                        eyr = true;
                    } else {
                        System.out.println("Break for eyr: " + Integer.toString(num));
                        break;
                    }
                } else if (Pattern.matches("hgt", a[0])) {
                    int num = Integer.parseInt(a[1].replaceAll("[^0-9]", ""));
                    if (a[1].contains("cm")) {
                        if (num >= 150 && num <= 193) {
                            hgt = true;
                        } else {
                            break;
                        }
                    } else if (a[1].contains("in")) {
                        if (num >= 59 && num <= 76) {
                            hgt = true;
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                } else if (Pattern.matches("hcl", a[0])) {
                    if (Pattern.matches("#(\\d|[a-f]){6}", a[1])) {
                        hcl = true;
                    } else {
                        break;
                    }
                } else if (Pattern.matches("ecl", a[0])) {
                    if (Pattern.matches("(amb)|(blu)|(brn)|(gry)|(grn)|(hzl)|(oth)", a[1])) {
                        ecl = true;
                    } else {
                        break;
                    }
                } else if (Pattern.matches("pid", a[0])) {
                    if (Pattern.matches("\\d{9}", a[1])) {
                        pid = true;
                    } else {
                        break;
                    }
                }
            }
            if (byr && iyr && eyr && hgt && hcl && ecl && pid) {
                count++;
            } else {
                System.out.println(s);
            }
        }

        return count;
    }

    static int part2Countv2(ArrayList<String> passports) {
        // int count = 0;

        // for (String s : passports) {
        //     boolean byr = Pattern.matches(".*byr:(19[2-9][0-9]|200[0-2]).*", s);
        //     boolean iyr = Pattern.matches(".*iyr:(201[0-9]|2020).*", s);
        //     boolean eyr = Pattern.matches(".*eyr:(202[0-9]|2030).*", s);
        //     boolean hgt = Pattern.matches(".*hgt:(1[5-8][0-9]cm|19[0-3]cm|59in|6[0-9]in|7[0-6]in).*", s);
        //     boolean hcl = Pattern.matches(".*hcl:#([0-9]|[a-f]){6}.*", s);
        //     boolean ecl = Pattern.matches(".*ecl:(amb|blu|brn|gry|grn|hzl|oth).*", s);
        //     boolean pid = Pattern.matches(".*pid:[0-9]{9}([^0-9]|\\b).*", s);

        //     if (byr && iyr && eyr && hgt && hcl && ecl && pid) {
        //         count++;
        //     }
        // }

        // return count;
        return part2GetStrings(passports).size();
    }

    static ArrayList<String> part2GetStrings(ArrayList<String> passports) {
        ArrayList<String> output = new ArrayList<String>();
        for (String s : passports) {
            boolean byr = Pattern.matches(".*byr:(19[2-9][0-9]|200[0-2]).*", s);
            boolean iyr = Pattern.matches(".*iyr:(201[0-9]|2020).*", s);
            boolean eyr = Pattern.matches(".*eyr:(202[0-9]|2030).*", s);
            boolean hgt = Pattern.matches(".*hgt:(1[5-8][0-9]cm|19[0-3]cm|59in|6[0-9]in|7[0-6]in).*", s);
            boolean hcl = Pattern.matches(".*hcl:#([0-9]|[a-f]){6}.*", s);
            boolean ecl = Pattern.matches(".*ecl:(amb|blu|brn|gry|grn|hzl|oth).*", s);
            boolean pid = Pattern.matches(".*pid:[0-9]{9}([^0-9]|\\b).*", s);

            if (byr && iyr && eyr && hgt && hcl && ecl && pid) {
                output.add(s);
            }
        }
        return output;
    }
}
