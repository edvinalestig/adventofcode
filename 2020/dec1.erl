-module(dec1).
-export([dec1/0]).

dec1() -> 
    {_,Input} = file:read_file("1dec.txt"),
    Integers = [erlang:list_to_integer(X) || X <- string:tokens(erlang:binary_to_list(Input), [$\r,$\n])],
    io:fwrite("Two factors: "),
    [Doubles|_] = [{X,Y,X*Y} || X <- Integers, Y <- Integers, X+Y =:= 2020],
    io:write(Doubles),
    io:fwrite("~nThree factors: "),
    [Triples|_] = [{X,Y,Z,X*Y*Z} || X <- Integers, Y <- Integers, Z <- Integers, X+Y+Z =:= 2020],
    io:write(Triples).
