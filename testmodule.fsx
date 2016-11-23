open System

/// </summary> Define types </summary>
type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

/// <summary> Transforms user text/string input to type codeColor </summary>
let stringToColor n =
  match n with
  | ("Red"|"red"|"r") -> Red
  | ("Green"|"green"|"g") -> Green
  | ("Yellow"|"yellow"|"y") -> Yellow
  | ("Purple"|"purple"|"p") -> Purple
  | ("White"|"white"|"w") -> White
  | ("Black"|"black"|"b") -> Black
  | _ -> failwith "invalid color request try with:\nRed | Green | Yellow | Purple | White | Black"

/// <summary> Transforms user text/string input to type player </summary>
let stringToPlayer n =
 match n with
 | "Human" -> Human
 | "Computer" -> Computer
 | _ -> failwith "invalid player request try with: \nHuman | Computer"

/// <summary> listRemove : helper-function </summary>
let rec listRemove i l =
    match i, l with
    | 0, x::xs -> xs
    | i, x::xs -> x::listRemove (i - 1) xs
    | i, [] -> failwith "index out of range"

/////////////////////////////////////////////////////////////////////////////////////////////

let playerPrompt = "Colors have to be typed in and should be one of the following: \nRed | Green | Yellow | Purple | White | Black \nYou can type like: Red or red even r\n"
let computerPrompt = "Computer has decided upon a combination"
                                                                                              
let guessLength = 4
let mutable (theCode : code) = []
let mutable (theGuess : code) = []

let interaction x =
    printfn "Pick a number for slot : %i" x
    stringToColor (Console.ReadLine())

let randomizer (x : int) =
    let mutable (colorPossibilities : code) = [Red; Green; Yellow; Purple; White; Black]
    let mutable (sample : code) = []
    let rand = System.Random()
    let mutable (imax : int) = 6
    let mutable (counter: int) = 0
    for i = 1 to x do
        counter <- rand.Next(0, imax)
        sample <- List.append [colorPossibilities.[counter]] sample
        colorPossibilities <- listRemove counter colorPossibilities
        imax <- imax - 1
    sample

let createPlayerCode (iterations : int) =
    let fix = [1..iterations]
    theGuess  <- fix |> List.map interaction
    printfn "%A" theGuess

let createComputerCode (iterations : int) =
    theGuess <- (randomizer iterations)
    printfn "%A" theGuess

let makeCode player =
    theGuess <- []
    match player with
    | Human -> Console.WriteLine playerPrompt
               createPlayerCode guessLength
    | Computer -> Console.WriteLine computerPrompt
                  createComputerCode guessLength
                  
printfn "Who wants to be the CODE-MAKER?\nHuman | Computer"
let (playerOne : player) = stringToPlayer (Console.ReadLine())
makeCode playerOne
theCode <- theGuess
Console.Clear()

printfn "Who wants to be the CODE-GUESSER?\nHuman | Computer"
let (playerTwo : player) = stringToPlayer (Console.ReadLine())

let mutable (board1 : board) = []
let rec validateCode (tryCode : code) (trueCode : code) (H: int) (S: int) =
  match tryCode with
  | [] -> (H,S)
  |x::xs when x = trueCode.[(4 - (tryCode.Length))] ->  (validateCode xs trueCode H (S+1))
  |x::xs when List.contains x trueCode ->  (validateCode xs trueCode (H+1) S)
  |x::xs -> (validateCode xs trueCode H S)

let rec playGame guess =
  Console.Clear()
  let Val = (validateCode theCode theGuess 0 0)
  printfn "Your guess resolved to - (White, Black) : %A\n" Val
  board1 <- ((theGuess, Val) :: board1)
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
              printfn "The true code was: %A" theCode
  | _ ->  playGame (makeCode playerTwo)
  
playGame (makeCode playerTwo)