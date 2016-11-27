open System

/// </summary> Define types </summary>
type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

/// <summary> Transforms user text/string input to type codeColor </summary>
let stringToColor sColor =
  match sColor with
  | ("Red"|"red"|"r") -> Some(Red)
  | ("Green"|"green"|"g") -> Some(Green)
  | ("Yellow"|"yellow"|"y") -> Some(Yellow)
  | ("Purple"|"purple"|"p") -> Some(Purple)
  | ("White"|"white"|"w") -> Some(White)
  | ("Black"|"black"|"b") -> Some(Black)
  | ("exit"|"Exit") -> (exit 1)
  | _ -> None
/// <summary> Transforms user text/string input to type player </summary>
let stringToPlayer sPlayer =
 match sPlayer with
 | ("Human"|"human"|"h") -> Some(Human)
 | ("Computer"|"computer"|"c") -> Some(Computer)
 | ("exit"|"Exit") -> (exit 1)
 | _ -> None

let rec checkStringColor consoleString =
    match stringToColor consoleString with
    |Some c -> c
    |None -> printfn "%s is not a legal command\nTry with\nRed|red|r" consoleString
             checkStringColor (Console.ReadLine())

let rec checkStringPlayer consoleString =
    match stringToPlayer consoleString with
    |Some p -> p
    |None -> printfn "%s is not a legal command\nTry with\nHuman|human|h" consoleString
             checkStringPlayer (Console.ReadLine())

/// <summary> listRemove : helper-function </summary>
let rec listRemove i l =
    match i, l with
    | 0, x::xs -> xs
    | i, x::xs -> x::listRemove (i - 1) xs
    | i, [] -> failwith "index out of range"

/////////////////////////////////////////////////////////////////////////////////////////////

let playerPrompt = "Colors have to be typed in and should be one of the following: \nRed | Green | Yellow | Purple | White | Black \nYou can type like: Red or red even r\n"
let computerPrompt = "Computer has decided upon a combination"

let codeLength = 4

let interaction x =
    printfn "Pick a color for slot : %i" x
    checkStringColor (Console.ReadLine())

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
    let mutable (anyCode : code) = []
    let lst = [1..iterations]
    anyCode  <- lst |> List.map interaction
    printfn "%A" anyCode
    anyCode

let createComputerCode (iterations : int) =
    let mutable (anyCode : code) = []
    anyCode <- (randomizer iterations)
    printfn "%A" anyCode
    anyCode

let makeCode player =
    match player with
    | Human -> Console.WriteLine playerPrompt
               createPlayerCode codeLength
    | Computer -> Console.WriteLine computerPrompt
                  createComputerCode codeLength

let rec validateCode (tryCode : code) (trueCode : code) (white: int) (black: int) =
  match tryCode with
  | [] -> (white, black)
  |x::xs when x = trueCode.[(4 - (tryCode.Length))] ->  (validateCode xs trueCode white (black + 1))
  |x::xs when List.contains x trueCode ->  (validateCode xs trueCode (white + 1) black)
  |x::xs -> (validateCode xs trueCode white black)

let mutable (codeMaker : player) = Unchecked.defaultof<player>
let mutable (codeGuesser : player) = Unchecked.defaultof<player>
let mutable (trueList : code) = Unchecked.defaultof<code>
let mutable (board1 : board) = []

let boardOutlook (consoleBoard: board) =
  printfn "+-----------------------------------------+"
  for i = (consoleBoard.Length - 1) downto 0 do
      printfn "|%A\t|%A\t|%A\t|%A\t| | %A|"
              ((fst consoleBoard.[i]).[0]) ((fst consoleBoard.[i]).[1])
              ((fst consoleBoard.[i]).[2]) ((fst consoleBoard.[i]).[3])
              (snd consoleBoard.[i])
  printfn "+-----------------------------------------+"

let firstRound n =
      if n = 0 then
          Console.WriteLine "Who wants to be the CODE-MAKER?\nHuman | Computer"
          codeMaker <- checkStringPlayer (Console.ReadLine())
          trueList <- makeCode codeMaker
          Console.Clear()
          Console.WriteLine "Who wants to be the CODE-GUESSER?\nHuman | Computer"
          codeGuesser <- checkStringPlayer (Console.ReadLine())

let rec playGame n =
  firstRound n
  let guessList = makeCode codeGuesser
  Console.Clear()
  let Val = (validateCode trueList guessList 0 0)
  printfn "Your guess resolved to - (White, Black) : %A\n" Val
  board1 <- ((guessList, Val) :: board1)
  Console.WriteLine "The board so far"
  boardOutlook board1
  match (Val, board1.Length) with
  | ((0,4),_) -> Console.WriteLine "Congratulations, Champion! You succeeded in beating your incompetent opponent."
  | (_,10) -> Console.WriteLine "Sorry you lost! You didn't guess the code. Mordecai Meirowitzkl does not approve!"
              printfn "The true code was: %A" trueList
  | _ ->  playGame (n+1)
playGame (0)
