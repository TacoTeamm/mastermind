/// <summary> Mastermind </summary>
/// <summary> This .fsx file contains the code for the game called Mastermind. </summary>
open System

/// <remarks> Creates the different types, which we are going to use in this program. </remarks>
type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int /// <remarks> White * Black </remarks>
type board = (code * answer) list
type player = Human | Computer

/// <summary> Collects a string and return Some(codeColor). </summary>
/// <param name = "sColor"> Takes a string, using: Console.ReadLine()  </param>
/// <returns> Some(codeColor), we want to return a codeColor, so we can make our (code: codeColor list) </returns>
/// <remarks> There is also the command to terminate the program. </remarks>
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

/// <summary> Collects a string and return Some(Player). </summary>
/// <param name = "sPlayer"> Takes a string, using: Console.ReadLine() </param>
/// <returns> Some(Player) - we want to return a Some(Player). </returns>
/// <remarks> There is also the command to terminate the program. </remarks>
let stringToPlayer sPlayer =
 match sPlayer with
 | ("Human"|"human"|"h") -> Some(Human)
 | ("Computer"|"computer"|"c") -> Some(Computer)
 | ("exit"|"Exit") -> (exit 1)
 | _ -> None

/// <summary> Collects Some(codeColor), and checks whether the command is allowed. </summary>
/// <param name = "consoleString"> Takes a string, using: Console.ReadLine(), and matches with stringToColor. </param>
/// <returns> codeColor </returns>
/// <remarks> This funktion was needed, so we didn't have to throw an exception, if any mistyping. </remarks>
let rec checkStringColor consoleString =
    match stringToColor consoleString with
    |Some c -> c
    |None -> printfn "%s is not a legal command\nTry with\nRed|red|r" consoleString
             checkStringColor (Console.ReadLine())

/// <summary> Collects Some(player), and checks whether the command is allowed. </summary>
/// <param name = "consoleString"> Takes a string, using: Console.ReadLine(), and matches with stringToPlayer. </param>
/// <returns> player - we want to return a player, so we know who should make the color combination. </returns>
/// <remarks> This funktion was needed, so we didn't have to throw an exception, if any mistyping. </remarks>
let rec checkStringPlayer consoleString =
    match stringToPlayer consoleString with
    |Some p -> p
    |None -> printfn "%s is not a legal command\nTry with\nHuman|human|h" consoleString
             checkStringPlayer (Console.ReadLine())

/// <summary> Removes one element from the list, so we don't get any dublicates. </summary>
/// <param name = "color"> This is the color,  </param>
/// <param name = "list"> Takes a string, using: Console.ReadLine() </param>
/// <returns> The initial list, but without the specified color. </returns>
let rec listRemove color list =
    match color, list with
    | 0, x::xs -> xs
    | i, x::xs -> x::listRemove (i - 1) xs
    | i, [] -> failwith "index out of range"

/// <summary> makeCode, This funktion is used make the different codes, both the codeGuessers and codeMakers. </summary>
/// <param name = "player"> This is the color,  </param>
/// <param name = "list"> Takes a string, using: Console.ReadLine() </param>
/// <returns> Player, we want to return a Player, so we know who should make the color combination. </returns>
let makeCode player =
  let mutable (codeColorList : code) = [Red; Green; Yellow; Purple; White; Black]
  let mutable makeList = []
  match player with
  | Human -> Console.WriteLine "Colors have to be typed in and should be one of the following: \nRed | Green | Yellow | Purple | White | Black \nYou can type like: Red or red even r\nPick a color for the 1st slot:"
             let frst = checkStringColor (Console.ReadLine())
             Console.WriteLine "Pick a color for the 2nd slot:"
             let scnd = checkStringColor (Console.ReadLine())
             Console.WriteLine "Pick a color for the 3rd slot:"
             let thrd = checkStringColor (Console.ReadLine())
             Console.WriteLine "Pick a color for the 4th slot:"
             let frth = checkStringColor (Console.ReadLine())
             makeList <- frst :: scnd :: thrd :: frth :: []
             makeList
  | Computer -> Console.WriteLine "Computer has decided upon a combination"
                let mutable nmbr = 0
                let rnd = System.Random()
                let mutable ix = 5
                for i=1 to 4 do
                    nmbr <- rnd.Next(0, ix)
                    makeList <- codeColorList.[nmbr] :: makeList
                    codeColorList <- listRemove nmbr codeColorList
                    ix <- ix - 1
                makeList

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
          printfn "%A" trueList
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
