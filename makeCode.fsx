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
  | Human -> Console.WriteLine "Colors have to be typed in and should be one of the following: \nRed | Green | Yellow | Purple | White | Black \nPick a color for the 1st slot:"
             let frst = stringToColor (Console.ReadLine())
             Console.WriteLine "Pick a color for the 2nd slot:"
             let scnd = stringToColor (Console.ReadLine())
             Console.WriteLine "Pick a color for the 3rd slot:"
             let thrd = stringToColor (Console.ReadLine())
             Console.WriteLine "Pick a color for the 4th slot:"
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
printfn "Who wants to be the CODE-MAKER?\nHuman | Computer"
let (makePlayer : player) = stringToPlayer (Console.ReadLine())
makeCode makePlayer
printfn "%A" makeList

Console.Clear()


let mutable guessList = []
let guessCode player =
  let mutable (codeColorList2 : code) = [Red; Green; Yellow; Purple; White; Black]
  match player with
  | Human -> Console.WriteLine "Colors have to be typed in and should be one of the following: \nRed | Green | Yellow | Purple | White | Black \nPick a color for the 1st slot:"
             let frst = stringToColor (Console.ReadLine())
             Console.WriteLine "Pick a color for the 2nd slot:"
             let scnd = stringToColor (Console.ReadLine())
             Console.WriteLine "Pick a color for the 3rd slot:"
             let thrd = stringToColor (Console.ReadLine())
             Console.WriteLine "Pick a color for the 4th slot:"
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
printfn "Who wants to be the CODE-GUESSER?\nHuman | Computer"
let (guessPlayer : player) = stringToPlayer (Console.ReadLine())
guessCode guessPlayer

let mutable (codeAnswer : answer) = (0, 0)
let mutable (board1 : board) = []
let validateCode (tryCode : code) (trueCode : code) =
  let mutable (hvid: int) = 0
  for i = 0 to (tryCode.Length - 1) do
    if List.contains tryCode.[i] trueCode then
       hvid <- hvid + 1
  let mutable (sort: int) = 0
  if hvid > 0 then
     for i = 0 to (tryCode.Length - 1) do
        if tryCode.[i] = trueCode.[i] then
           hvid <- hvid - 1
           sort <- sort + 1
  printfn "Your guess : %A" guessList
  codeAnswer <- (hvid, sort)

validateCode makeList guessList
printfn "Your guess resolved to - (White, Black) : %A" codeAnswer
board1 <- ((guessList, codeAnswer) :: board1)

if makeList = guessList then
      Console.WriteLine "Congratulations, Champion! You succeeded in beating your incompetent opponent."
else
for i = 0 to 9 do
  if board1.Length = 10 then
    Console.WriteLine "Sorry you lost! You didn't guess the code"
    printfn "The true code was: %A" makeCode
  else guessCode guessPlayer
       validateCode makeList guessList
       board1 <- ((guessList, codeAnswer) :: board1)
       Console.WriteLine "The board so far"
       for i = (board1.Length-1) downto 0 do
          printfn "%A \t %A" (fst board1.[i]) (snd board1.[i])
          
