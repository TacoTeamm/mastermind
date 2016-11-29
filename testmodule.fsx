/// <summary> Mastermind </summary>
/// <summary> This .fsx file contains the entire game and can be run using interactive mode - or using mono</summary>

/// <remarks> Open System namespace/module </remarks>
open System

/////////////////////////////////////////////////////////////////////////////////////////////
///                                 Type definitions                                      ///
/////////////////////////////////////////////////////////////////////////////////////////////


/// </summary> Define types as provided in assignment </summary>
type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
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

/////////////////////////////////////////////////////////////////////////////////////////////
///                                 Basic auxiliary functions                            ///
/////////////////////////////////////////////////////////////////////////////////////////////

/// <summary> Collects a string and return Some(Player). </summary>
/// <param name = "sPlayer"> Takes a string, using: Console.ReadLine() </param>
/// <returns> Some(Player) - we want to return a Some(Player). </returns>
/// <remarks> The purpose of using options: Exception handling </remarks>
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
/// <remarks> This function was needed, so we didn't have to throw an exception, if any mistyping. </remarks>
let rec checkStringColor consoleString =
    match stringToColor consoleString with
    |Some c -> c
    |None -> printfn "%s is not a legal command\nTry with\nRed|red|r" consoleString
             checkStringColor (Console.ReadLine())

/// <summary> Collects Some(player), and checks whether the command is allowed. </summary>
/// <param name = "consoleString"> Takes a string, using: Console.ReadLine(), and matches with stringToPlayer. </param>
/// <returns> player - we want to return a player, so we know who should make the color combination. </returns>
/// <remarks> This function was needed, so we didn't have to throw an exception, if any mistyping. </remarks>
let rec checkStringPlayer consoleString =
    match stringToPlayer consoleString with
    |Some p -> p
    |None -> printfn "%s is not a legal command\nTry with\nHuman|human|h" consoleString
             checkStringPlayer (Console.ReadLine())

/// <summary> Removes one element from the list, so we don't get any dublicates. </summary>
/// <param name = "i> This is the color,  </param>
/// <param name = "list"> Takes a string, using: Console.ReadLine() </param>
/// <returns> A list which has been reduced by whatever's contained in "i" </returns>
let rec listRemove i l =
    match i, l with
    | 0, x::xs -> xs
    | i, x::xs -> x::listRemove (i - 1) xs
    | i, [] -> failwith "index out of range"

/////////////////////////////////////////////////////////////////////////////////////////////
///                                 Game functions/variables                              ///
/////////////////////////////////////////////////////////////////////////////////////////////

/// <summary> Strings for use when interacting with player(s) </summary>
let playerPrompt = "Colors have to be typed in and should be one of the following: \nRed | Green | Yellow | Purple | White | Black \nYou can type like: Red or red even r\n"
let computerPrompt = "Computer has decided upon a combination"

/// <summary> Set board-width - the number of colors in the code </summary>
let guessLength = 4
/// <summary> A mutable variable for storing temporary guesses </summary>
let mutable (theGuess : code) = []

/// <summary> A function, which prompts the player and takes input </summary>
/// <remarks> USED BY FUNCTION : "createPlayerCode"" </remarks>
/// <param name = "x"> Integer value: Used in printfn; let's player know which position in 'code' he/she has to make or guess </param>
/// <returns> codeColor </returns>
let interaction (x : int) =
    printfn "Pick a color for slot : %i" x
    checkStringColor (Console.ReadLine())

/// <summary> A function, which constructs a random code by subsetting the list of valid colors in codeColor </summary>
/// <remarks> USED BY FUNCTION : "createComputerCode" </summary>
/// <param name = "x"> Integer value; Number of iterations of for-loop. Determines the length of the randomized code </param>
/// <remarks> Mutable: colorPossibilities - list of possible colors for the function to pick from </remarks>
/// <remarks> Mutable: sample - temporary output list </remarks>
/// <remarks> Rand: Uses Random from the System-namespace. A random number generator, takes an interval </remarks>
/// <remarks> Mutable: imax - upper limit for Rand </remarks>
/// <remarks> Mutable: counter - stores the value generated by rand </remarks>
/// <returns> A randomly generated list of four unique colors </returns>
let randomizer (x : int) =
    let mutable (colorPossibilities : code) = [Red; Green; Yellow; Purple; White; Black]
    let mutable (sample : code) = []
    let rand = Random()
    let mutable (imax : int) = 6
    let mutable (counter: int) = 0
    for i = 1 to x do
        counter <- rand.Next(0, imax)
        sample <- List.append [colorPossibilities.[counter]] sample
        colorPossibilities <- listRemove counter colorPossibilities
        imax <- imax - 1
    sample

/// <summary> A function, which utilizes interaction to map each iteration into a mutable list </summary>
/// <remarks> Also prints the finished list of colors chosen by the player. </remarks>
/// <remarks> USED BY FUNCTION : "makecode" </remarks>
/// <param name = "iterations"> Number of guesses to be provided by player. </param>
/// <returns> Returns each guess appended to a mutable list 'theGuess' </returns>
let createPlayerCode (iterations : int) =
    let lst = [1..iterations]
    theGuess  <- lst |> List.map interaction
    printfn "%A" theGuess

/// <summary> A function, which utilizes the randomizer function to append to a mutable list </summary>
/// <remarks> Also prints the finished list chosen by the computer / random generator </remarks>
/// <param name = "iterations"> Number of colors to be chosen by computer </param>
/// <returns> Returns the randomly generated colors to mutable list 'theGuess' </returns>
let createComputerCode (iterations : int) =
    theGuess <- (randomizer iterations)
    printfn "%A" theGuess

/// <summary> A function, which given the player-type executes the code-making/code-guessing process </summary>
/// <remarks> If human, provide pre-defined initilization prompt in 'playerPromt' and then take user-input </remarks>
/// <remarks> If computer, provide pre-defined initilization prompt in 'computerPromt' and then generate code</remarks>
/// <param name = "player"> Takes player-type as input. </param>
/// <returns> theGuess: A mutable list of length 'guessLength' containing codeColors </returns>
let makeCode player =
    theGuess <- []
    match player with
    | Human -> Console.WriteLine playerPrompt
               createPlayerCode guessLength
    | Computer -> Console.WriteLine computerPrompt
                  createComputerCode guessLength

/// <summary> Recursive function that compares two codes with each other, and returns the answer </summary>
/// <param name = "tryCode"> This is the code which should be compared, it is the guess. </param>
/// <param name = "trueCode"> This is the code to be compared with, it is the first code made. </param>
/// <param name = "white"> this is a recursive function, thus we need to define whites in the call. </param>
/// <param name = "black"> this is a recursive function, thus we need to define blacks in the call. </param>
/// <returns> a tuple of int: containing Whites first and then Black. </returns>
let rec validateCode (tryCode : code) (trueCode : code) (white: int) (black: int) =
  match tryCode with
  | [] -> (white, black)
  |x::xs when x = trueCode.[(4 - (tryCode.Length))] ->  (validateCode xs trueCode white (black + 1))
  |x::xs when List.contains x trueCode ->  (validateCode xs trueCode (white + 1) black)
  |x::xs -> (validateCode xs trueCode white black)

/////////////////////////////////////////////////////////////////////////////////////////////
///                                 Play Game                                             ///
/////////////////////////////////////////////////////////////////////////////////////////////

/// <summary> Prompt user to decide who should be the Code Maker </summary>
printfn "Who wants to be the CODE-MAKER?\nHuman | Computer"
let (playerOne : player) = checkStringPlayer (Console.ReadLine())

/// <summary> Execute makeCode with codemaker as input </summary>
makeCode playerOne
/// <summary> Save the code as 'theCode' </summary>
let (theCode : code) = theGuess
/// <summary> Clear console so that theCode is not viewable for the code guesser </summary>
Console.Clear()

/// <summary> Prompt user to decide who should be the Code Guesser </summary>
printfn "Who wants to be the CODE-GUESSER?\nHuman | Computer"
let (playerTwo : player) = checkStringPlayer (Console.ReadLine())

/// <summary> Difines the board which should be printed to the console.  </summary>
/// <summary> Recursive function that plays the game, when called with  </summary>
/// <param name = "guess"> This part maked the new theGuess, which should be compared to theCode </param>
/// <returns> a string: Which tells you whether you lose or win. </returns>
let rec playGame guess =
  Console.Clear()
  let Val = (validateCode theCode theGuess 0 0) /// <remarks> Defines the answer, to be added to the board. </remarks>
  printfn "Your guess resolved to - (White, Black) : %A\n" Val
  board1 <- ((theGuess, Val) :: board1)
  Console.WriteLine "The board so far"
  printfn "+-----------------------------------------+"
  for i = (board1.Length - 1) downto 0 do
      printfn "|%A\t|%A\t|%A\t|%A\t| | %A|" /// <remarks> This is the outlook of the board. </remarks>
              ((fst board1.[i]).[0]) ((fst board1.[i]).[1])
              ((fst board1.[i]).[2]) ((fst board1.[i]).[3])
              (snd board1.[i])
  printfn "+-----------------------------------------+"
  match (Val, board1.Length) with /// <remarks> Compares Val, and the length of board1, to know whether you lost or won the game. </remarks>
  | ((0,4),_) -> Console.WriteLine "Congratulations, Champion! You succeeded in beating your incompetent opponent."
  | (_,10) -> Console.WriteLine "Sorry you lost! You didn't guess the code. Mordecai Meirowitzkl does not approve!"
              printfn "The true code was: %A" theCode
  | _ ->  playGame (makeCode playerTwo)

/// <summary> Run the game with the input provided for both players </summary>
playGame (makeCode playerTwo)
