<<<<<<< Updated upstream:makeCode2.fsx
/// <summary> Mastermind </summary>
/// <summary> This .fsx file contains the code for the game called Mastermind. </summary>
open System

/// <remarks> Creates the different types, which we are going to use in this program. </remarks>
type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int /// <remarks> White * Black </remarks>
type board = (code * answer) list
type player = Human | Computer

/// <summary> Collects a string and return codeColor. </summary>
/// <param name = "sColor"> Takes a string, using: Console.ReadLine()  </param>
/// <returns> codeColor, we want to return a codeColor, so we can make our (code: codeColor list) </returns>
let stringToColor sColor =
  match sColor with
  | ("Red"|"red"|"r") -> Red
  | ("Green"|"green"|"g") -> Green
  | ("Yellow"|"yellow"|"y") -> Yellow
  | ("Purple"|"purple"|"p") -> Purple
  | ("White"|"white"|"w") -> White
  | ("Black"|"black"|"b") -> Black

/// <summary> Collects a string and return Player. </summary>
/// <param name = "sPlayer"> Takes a string, using: Console.ReadLine() </param>
/// <returns> Player, we want to return a Player, so we know who should make the color combination. </returns>
let stringToPlayer sPlayer =
 match sPlayer with
 | ("Human"|"human"|"h") -> Human
 | ("Computer"|"computer"|"c") -> Computer


(*let checkString consoleString=
  match ((stringToColor|stringToPlayer) consoleString) with
  |Some c -> c
  |None   -> printfn "%s is not a legal command" consoleString
             checkString (Console.ReadLine())*)

/// <summary> Removes one element from the list, so we don't get any dublicates. </summary>
/// <param name = "color"> This is the color,  </param>
/// <param name = "list"> Takes a string, using: Console.ReadLine() </param>
/// <returns> Player, we want to return a Player, so we know who should make the color combination. </returns>
let rec listRemove color list =
    match color, list with
    | 0, x::xs -> xs
    | i, x::xs -> x::listRemove (i - 1) xs
    | i, [] -> failwith "index out of range"

let mutable trueList = []
let mutable guessList = []
let makeCode player (codeList: code) =
  let mutable (codeColorList : code) =
            [Red; Green; Yellow; Purple; White; Black]
  let mutable codeList = codeList
  codeList <- []
  match player with
  | Human -> Console.WriteLine "Colors have to be typed in and should be one of the following: \nRed | Green | Yellow | Purple | White | Black \nPick a color for the 1st slot:"
             let frst = stringToColor (Console.ReadLine())
             Console.WriteLine "Pick a color for the 2nd slot:"
             let scnd = stringToColor (Console.ReadLine())
             Console.WriteLine "Pick a color for the 3rd slot:"
             let thrd = stringToColor (Console.ReadLine())
             Console.WriteLine "Pick a color for the 4th slot:"
             let frth = stringToColor (Console.ReadLine())
             codeList <- frst :: scnd :: thrd :: frth :: codeList
             (*guessList <- []
             for i=1 to 4 do  printfn "Pick a color for slot number %A:" i
                              guessList <- (stringToColor (Console.ReadLine()) :: guessList
             guessList <- (List.rev guessList)*)
  | Computer -> Console.WriteLine "Computer has decided upon a random guess combination"
                let mutable nmbr = 0
                let rnd = System.Random()
                let mutable ix = 5
                for i=1 to 4 do
                    nmbr <- rnd.Next(0, ix)
                    codeList <- codeColorList.[nmbr] :: codeList
                    codeColorList <- listRemove nmbr codeColorList
                    ix <- ix - 1
  (*| _ -> failwith "Invalid player"*)
printfn "Who wants to be the CODE-MAKER?\nHuman | Computer"
let (codeMaker : player) = stringToPlayer (Console.ReadLine())
makeCode codeMaker
printfn "%A" trueList
Console.Clear()

printfn "Who wants to be the CODE-GUESSER?\nHuman | Computer"
let (guessPlayer : player) = stringToPlayer (Console.ReadLine())

let mutable (board1 : board) = []
let rec validateCode (tryCode : code) (trueCode : code) (white: int) (black: int) =
  match tryCode with
  | [] -> (white, black)
  | x::xs when x = trueCode.[(4 - (tryCode.Length))] ->  (validateCode xs trueCode white (black+1))
  | x::xs when List.contains x trueCode ->  (validateCode xs trueCode (white+1) black)
  | x::xs -> (validateCode xs trueCode white black)

let rec playGame guess =
  Console.Clear()
  let Val = (validateCode trueList guessList 0 0)
  printfn "Your guess resolved to - (White, Black) : %A\n" Val
  board1 <- ((guessList, Val) :: board1)
  Console.WriteLine "The board so far"
  printfn "+-----------------------------------------+"
  for i = (board1.Length - 1) downto 0 do
      printfn "|%A\t|%A\t|%A\t|%A\t| | %A|"
              ((fst board1.[i]).[0]) ((fst board1.[i]).[1])
              ((fst board1.[i]).[2]) ((fst board1.[i]).[3])
              (snd board1.[i])
  printfn "+-----------------------------------------+"
  match (Val, board1.Length) with
  | ((0,4),_) -> Console.WriteLine "Congratulations, Champion! You succeeded in beating your incompetent opponent."
  | (_,10) -> Console.WriteLine "Sorry you lost! You didn't guess the code. Mordecai Meirowitzkl does not approve!"
              printfn "The true code was: %A" trueList
  | _ ->  playGame (makeCode guessPlayer guessList)
playGame (makeCode guessPlayer guessList)
=======
let mutable x = ["hej"; "hej2"]

let mutable y = ["bye"; "bye"]


let func lizzy =
    let mutable lizzy = lizzy
    lizzy <- []
    lizzy @ ["hej3"]

printfn "%A %A" (func x) (func y)



>>>>>>> Stashed changes:#testmodule.fsx#
