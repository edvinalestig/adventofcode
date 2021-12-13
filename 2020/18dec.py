def takeWhileNotOp(s):
    sOut = ""
    for c in s:
        if c == "+" or c == "*": return sOut, c
        sOut += c
    return sOut, None

def parseString(s):
    i = 0
    string = ""
    while i < len(s):
        c = s[i]
        if c == "(":
            sub = getSubstring(s[i+1:])
            string += str(parseString(sub))
            i += len(sub) + 1
        else:
            string += c
        i += 1

    return evalExpr2(string) # <-- Change between part 1 and 2 here
    
def evalExpr1(expr):
    total, operator = takeWhileNotOp(expr)
    i = len(total)+1
    total = int(total)
    while i < len(expr):
        n, newOp = takeWhileNotOp(expr[i:])
        i += len(n)+1
        if operator == "+":
            total += int(n)
        elif operator == "*":
            total *= int(n)
        operator = newOp
        if not newOp: break

    return total

def evalExpr2(expr):
    toMultiply = []
    total, operator = takeWhileNotOp(expr)
    i = len(total) + 1
    total = int(total)
    while i < len(expr):
        n, newOp = takeWhileNotOp(expr[i:])
        i += len(n) + 1
        if operator == "*":
            toMultiply.append(total)
            total = int(n)
        elif operator == "+":
            total += int(n)
        operator = newOp
    
    for m in toMultiply:
        total *= m
    
    return total

def getSubstring(s):
    o = 0
    outStr = ""
    for c in s:
        if c == "(":
            o += 1
        if c == ")":
            if o == 0:
                return outStr
            else:
                o -= 1

        outStr += c
    return outStr

def part12(exprs):
    sum = 0
    for expr in exprs:
        expr.strip()
        sum += parseString(expr.replace(" ", ""))

    print(sum)



with open("18dec.txt", "r") as f:
    exprs = f.readlines()
part12(exprs)
