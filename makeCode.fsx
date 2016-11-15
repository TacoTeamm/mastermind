/// <summary> For now, the System-namespace is required </summary>
open System

/// <summar> Define types as suggested in problem description </summary>
type codeColor =
  Red | Green | Yellow | Purple | White | Black

type code = codeColor list

type answer = int * int

type board = (code * answer) list

type player = Human | Computer


/// <summmary> Make a random code generator for when Computer picks code </summary>
let (codeColorElements : code) = [Red; Green; Yellow; Purple; White; Black]
let mutable (codeList : code) = []
/// <summary> pickRandom picks a random integer, extracts from codeList the value of that index (integer) and appends to kodeListe </summary>
/// <param name="input"> The list of possible colors of type "code" </param>
let pickRandom (input : code) =
    let rand = System.Random()
    let possibleColors = input
    for i = 0 to 3 do
        let index = rand.Next(0, (possibleColors.Length - 1))
        codeList <- (possibleColors.[index] :: codeList)
       (*printfn "%A" possibleColors.[index]
        printfn "%A" kodeListe*)

pickRandom codeColorElements
/// <remarks> Make sure, that the appended kodeListe is available outside the scope of pickRandom</remarks>
printfn "%A" codeList

