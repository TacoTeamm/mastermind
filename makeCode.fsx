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
                    let nmbr = rnd.Next(0, ix)
                    printfn "%A" codeColorList
                    makeList <- codeColorList.[nmbr] :: makeList
                    codeColorList <- listRemove nmbr codeColorList
                    if nmbr >= (codeColorList.Length - 1) then
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
                for i=1 to 4 do
                    let nmbr = rnd.Next(0, ix)
                    printfn "%A" codeColorList2
                    guessList <- codeColorList2.[nmbr] :: guessList
                    codeColorList2 <- listRemove nmbr codeColorList2
                    if nmbr >= (codeColorList2.Length - 1) then
                       ix <- ix - 1
  (*| _ -> failwith "Invalid player"*)
printfn "Who wants to guess the codecombination?\nHuman | Computer"
let (guessPlayer : player) = stringToPlayer (Console.ReadLine())
guessCode guessPlayer
printfn "%A" guessList
