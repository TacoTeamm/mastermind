open System

type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

let (makeCode : code) = [Red; Green; Yellow; Purple]
let (guessCode : code) = [Green; Green; Green; Purple]

module validation = 
  let validateCode (tryCode : code) (trueCode : code) =
    let mutable (hvid: int) = 0
    for i = 0 to (tryCode.Length - 1) do
        if List.contains tryCode.[i] trueCode then
            hvid <- hvid + 1
    printfn "temphvid %A" hvid
    let mutable (sort: int) = 0
    if hvid > 0 then
        for i = 0 to (tryCode.Length - 1) do
            if tryCode.[i] = trueCode.[i] then
                hvid <- hvid - 1
                sort <- sort + 1
    printfn "Hvid : %A \nSort : %A" hvid sort
  validateCode makeCode guessCode



(*
let mutable (codeAnswer : answer) = (0, 0)
let mutable (bord : board) = []
let validateCode (inputMakeCode : code) (inputGuessCode : code) =
  let mutable (hvid : int) = Unchecked.defaultof<int>
  let mutable (sort : int) = Unchecked.defaultof<int>
  let rec recValidateCode (inputMakeCode, inputGuessCode) =
    match inputMakeCode inputGuessCode with
    | (l :: ls, lss) -> if List.contains l lss then
                           hvid <- 1 + hvid
                           recValidateCode (ls, lss)
                        else recValidateCode (ls, lss)
    | ([], lss) -> codeAnswer <- (hvid, snd codeAnswer)
  (recValidateCode (inputMakeCode, inputGuessCode))
validateCode makeCode guessCode
bord <- ((guessCode, codeAnswer) :: bord)
printfn "%A" bord*)
(*let mutable (codeAnswer : answer) = (0, 0)
let validateCode ((inputMakeCode : code), (inputGuessCode : code)) =
  let mutable (hvid : int) = Unchecked.defaultof<int>
  let rec test ((inputMakeCode : code), (inputGuessCode : code)) =
    match (inputMakeCode, inputGuessCode) with
    | (l :: ls, lss) -> if List.contains l lss then
                           hvid <- (1 + hvid)
                           test (ls, lss)
                        else test (ls, lss)
    | ([], _) -> hvid
  codeAnswer <- (hvid, snd codeAnswer)
printfn "%A" codeAnswer*)
