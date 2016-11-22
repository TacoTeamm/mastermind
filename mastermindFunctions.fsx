module mastermindFunctions =
    type codeColor = Red | Green | Yellow | Purple | White | Black
    type code = codeColor list
    type answer = int * int
    type board = (code * answer) list
    type player = Human | Computer
    
    let stringToColor n =
        match n with
        | ("Red"|"red"|"r") -> Red
        | ("Green"|"green"|"g") -> Green
        | ("Yellow"|"yellow"|"y") -> Yellow
        | ("Purple"|"purple"|"p") -> Purple
        | ("White"|"white"|"w") -> White
        | ("Black"|"black"|"b") -> Black
        | _ -> failwith "invalid color request try with:\nRed | Green | Yellow | Purple | White | Black"
