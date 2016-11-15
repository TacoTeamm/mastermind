open System

type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

let stringToColor n =
  match n with
  | "Red" -> Red
  | "Green" -> Green
  | "Yellow" -> Yellow
  | "Purple" -> Purple
  | "White" -> White
  | "Black" -> Black
  | _ -> failwith "invalid color request try with:\nRed | Green | Yellow | Purple | White | Black"

let stringToPlayer n =
 match n with
 | "Human" -> Human
 | "Computer" -> Computer
 | _ -> failwith "invalid player request try with: \nHuman | Computer"

let rec listRemove i l =
    match i, l with
    | 0, x::xs -> xs
    | i, x::xs -> x::listRemove (i - 1) xs
    | i, [] -> failwith "index out of range"

let mutable makeList = []
let mutable guessList = []

let mutable (codeColorList : code) = [Red; Green; Yellow; Purple; White; Black]
    
module guessModule  =

    let humanGuessFct (inputList : code) =
        Console.WriteLine "Request first color - Should be one of the following: \nRed | Green | Yellow | Purple | White | Black"
        let frst = stringToColor (Console.ReadLine())
        Console.WriteLine "Request second color"
        let scnd = stringToColor (Console.ReadLine())
        Console.WriteLine "Request third color"
        let thrd = stringToColor (Console.ReadLine())
        Console.WriteLine "Request fourth color"
        let frth = stringToColor (Console.ReadLine())
        inputList @  frst :: scnd :: thrd :: frth :: []
            
    let computerGuessFct (inputList : code) =
        let mutable inputList = inputList
        Console.WriteLine "Computer has decided upon a combination"
        let mutable nmbr = 0
        let rnd = System.Random()
        let mutable ix = 5
        for i=1 to 4 do
            let nmbr = rnd.Next(0, ix)
            inputList <-  codeColorList.[nmbr] :: inputList
            codeColorList <- listRemove nmbr codeColorList
            ix <- ix - 1
                
printfn "Who wants to make the codecombination?\nHuman | Computer"
let (makePlayer : player) = stringToPlayer (Console.ReadLine())

let makeCode player =
  match player with 
  | Human -> (guessModule.humanGuessFct makeList )
  | Computer -> (guessModule.computerGuessFct makeList)
  (*| _ -> failwith "Invalid player"*)
makeCode makePlayer

printfn "%A" makeList


let guessCode player =
  let mutable (codeColorList2 : code) = [Red; Green; Yellow; Purple; White; Black]
  match player with
  | Human -> (guessModule.humanGuessFct guessList)
  | Computer -> (guessModule.computerGuessFct guessList)
  (*| _ -> failwith "Invalid player"*)
printfn "Who wants to guess the codecombination?\nHuman | Computer"
let (guessPlayer : player) = stringToPlayer (Console.ReadLine())
guessCode guessPlayer
printfn "%A" guessList




