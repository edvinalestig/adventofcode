
using System.Collections;
using System.IO;

String line;
try
{
    StreamReader reader = new StreamReader("../../../input.txt");
    line = reader.ReadLine();

    int currMax = 0;
    int cal = 0;
    List<int> list = new();

    while (line != null)
    {
        if (line == "")
        {
            currMax = Math.Max(currMax, cal);
            list.Add(cal);
            cal = 0;
        }
        else
        {
            cal += int.Parse(line);
        }
        line = reader.ReadLine();
    }
    reader.Close();

    Console.WriteLine(currMax);
    Console.WriteLine(list.Max());
    list.Sort();
    list.Reverse();
    Console.WriteLine(list[0] + list[1] + list[2]);
}
catch (Exception e)
{
    Console.WriteLine(e.Message);
}
