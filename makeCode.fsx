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
let makeCode player =
  let mutable (codeColorList : code) = [Red; Green; Yellow; Purple; White; Black]
  match player with
  | Human -> Console.WriteLine "Request first color - Should be one of the following: \nRed | Green | Yellow | Purple | White | Black"
             let frst = stringToColor (Console.ReadLine())
             Console.WriteLine "Request second color"
             let scnd = stringToColor (Console.ReadLine())
             Console.WriteLine "Request third color"
             let thrd = stringToColor (Console.ReadLine())
             Console.WriteLine "Request fourth color"
             let frth = stringToColor (Console.ReadLine())
             makeList <- frst :: scnd :: thrd :: frth :: []
  | Computer -> Console.WriteLine "Computer has decided upon a combination"
                let mutable nmbr = 0
                let rnd = System.Random()
                let mutable ix = 5
                for i=1 to 4 do
                    nmbr <- rnd.Next(0, ix)
                    makeList <- codeColorList.[nmbr] :: makeList
                    codeColorList <- listRemove nmbr codeColorList
                    ix <- ix - 1
  (*| _ -> failwith "Invalid player"*)
printfn "Who wants to make the codecombination?\nHuman | Computer"
let (makePlayer : player) = stringToPlayer (Console.ReadLine())
makeCode makePlayer
printfn "%A" makeList

let mutable guessList = []
let guessCode player =
  let mutable (codeColorList2 : code) = [Red; Green; Yellow; Purple; White; Black]
  match player with
  | Human -> Console.WriteLine "Request first color - Should be one of the following: \nRed | Green | Yellow | Purple | White | Black"
             let frst = stringToColor (Console.ReadLine())
             Console.WriteLine "Request second color"
             let scnd = stringToColor (Console.ReadLine())
             Console.WriteLine "Request third color"
             let thrd = stringToColor (Console.ReadLine())
             Console.WriteLine "Request fourth color"
             let frth = stringToColor (Console.ReadLine())
             guessList <- frst :: scnd :: thrd :: frth :: []
  | Computer -> Console.WriteLine "Computer has decided upon a random guess combination"
                let mutable nmbr = 0
                let rnd = System.Random()
                let mutable ix = 5
                guessList <- []
                for i=1 to 4 do
                    nmbr <- rnd.Next(0, ix)
                    guessList <- codeColorList2.[nmbr] :: guessList
                    codeColorList2 <- listRemove nmbr codeColorList2
                    ix <- ix - 1
  (*| _ -> failwith "Invalid player"*)
printfn "Who wants to guess the codecombination?\nHuman | Computer"
let (guessPlayer : player) = stringToPlayer (Console.ReadLine())
guessCode guessPlayer
printfn "%A" guessList

let mutable (codeAnswer : answer) = (0, 0)
let mutable (board1 : board) = []
let validateCode (tryCode : code) (trueCode : code) =
  let mutable (hvid: int) = 0
  for i = 0 to (tryCode.Length - 1) do
    if List.contains tryCode.[i] trueCode then
       hvid <- hvid + 1
  printfn "%A" hvid
  let mutable (sort: int) = 0
  if hvid > 0 then
     for i = 0 to (tryCode.Length - 1) do
        if tryCode.[i] = trueCode.[i] then
           hvid <- hvid - 1
           sort <- sort + 1
  printfn "Hvid : %A \nSort : %A" hvid sort
  printfn "%A" guessList
  codeAnswer <- (hvid, sort)
  if makeList = guessList then
     Console.WriteLine "Congratz you won! You guessed the code"

validateCode makeList guessList
printfn "%A" codeAnswer
board1 <- ((guessList, codeAnswer) :: board1)

Console.WriteLine "The board so far"
for i = 0 to 10 do
  if board1.Length < 10 then
    Console.WriteLine "Sorry you lost! You didn't guess the code"
  else guessCode guessPlayer
       validateCode makeList guessList
       board1 <- ((guessList, codeAnswer) :: board1)
       for i = (board1.Length) downto 0 do
          printfn "%A\n" board1.[i]
