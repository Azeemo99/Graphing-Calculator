
module InterpreterCore



//Advanced programing Tom  
//blah blah
// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

open System
type v = 
    | IVal of int
    | FVal of float

type terminal = 
    Add | Sub | Mul | Div | Lpar | Rpar | Num of int | Rem | Pow | Flo of float | Equ | Id of string

let str2lst s = [for c in s -> c]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let isPoint c = (c = '.')
let isEqu c = (c = '=')
let isLetter c = System.Char.IsAsciiLetter c
let lexError = System.Exception("Lexer error")
let intVal (c:char) = (int)((int)c - (int)'0')
let parseError = System.Exception("Parser error")

let rec scString(idStr, id) = 
    match idStr with
    |c:: tail when isLetter c -> scString(tail, sprintf "%s%c" id c)
    |_ -> (idStr, id)


let rec scNumber(iStr, iVal, hasPoi:bool) = 
    match iStr with 
    |c:: tail when isdigit c -> scNumber(tail, sprintf "%s%c" iVal c, hasPoi)
    |c:: tail when isPoint c -> 
        if hasPoi then 
            raise lexError
        else 
            scNumber(tail, sprintf "%s%c" iVal c, true)
    | _ -> (iStr, iVal, hasPoi)


let rec scInt(iStr, iVal) = 
    match iStr with
    c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
    | _ -> (iStr, iVal)


let lexer input = 
    let rec scan input =
        match input with
        | [] -> []
        | '+'::tail -> Add :: scan tail
        | '-'::tail -> Sub :: scan tail
        | '*'::tail -> Mul :: scan tail
        | '/'::tail -> Div :: scan tail
        | '('::tail -> Lpar :: scan tail
        | ')'::tail -> Rpar :: scan tail
        | '%':: tail -> Rem :: scan tail
        | '^':: tail -> Pow :: scan tail
        | '=' :: tail -> Equ :: scan tail
        | c :: tail when isblank c -> scan tail
        | c :: tail when isLetter c -> let (idStr, id) = scString(tail, sprintf "%c" c)
                                       Id id :: scan tail
        | c :: tail when isdigit c -> let (iStr, iVal, hasPoi) = scNumber(tail, sprintf "%c" c, false) 
                                      if hasPoi then 
                                          Flo (float iVal) :: scan iStr  
                                      else 
                                          let intV = Int32.Parse(iVal)
                                          Num (intV) :: scan iStr
        | c :: tail when isPoint c -> let (iStr, iVal, hasPoi) = scNumber(tail, "", true) 
                                      Flo (float (sprintf "0.%s" iVal)) :: scan iStr  
        | _ -> raise lexError
    scan (str2lst input)

let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()

let add (v1:v, v2:v) = 
    match (v1, v2) with
    | (IVal i1, IVal i2) -> IVal (i1 + i2)
    | (FVal f1, FVal f2) -> FVal (f1 + f2)
    | (IVal i1, FVal f2) -> FVal ((float)i1 + f2)
    | (FVal f1, IVal i2) -> FVal (f1 + (float)i2)

let sub (v1:v, v2:v) = 
    match (v1, v2) with
    | (IVal i1, IVal i2) -> IVal (i1 - i2)
    | (FVal f1, FVal f2) -> FVal (f1 - f2)
    | (IVal i1, FVal f2) -> FVal ((float)i1 - f2)
    | (FVal f1, IVal i2) -> FVal (f1 - (float)i2)

let mul (v1:v, v2:v) =
    match (v1, v2) with
    | (IVal i1, IVal i2) -> IVal (i1 * i2)
    | (FVal f1, FVal f2) -> FVal (f1 * f2)
    | (IVal i1, FVal f2) -> FVal ((float)i1 * f2)
    | (FVal f1, IVal i2) -> FVal (f1 * (float)i2)

let div (v1:v, v2:v) = 
    match (v1, v2) with
    | (IVal i1, IVal i2) -> IVal (i1 / i2)
    | (FVal f1, FVal f2) -> FVal (f1 / f2)
    | (IVal i1, FVal f2) -> FVal ((float)i1 / f2)
    | (FVal f1, IVal i2) -> FVal (f1 / (float)i2)

let pow (v1: v, v2:v) = 
    match (v1, v2) with
    | (IVal i1, IVal i2) -> FVal (float (i1) ** (i2))
    | (FVal f1, FVal f2) -> FVal (float (f1 ** f2))
    | (FVal f1 , IVal i2) -> FVal (float (f1 ** i2))
    | (IVal i1, FVal f2) -> FVal (float ((float)i1 ** f2))

let rem (v1: v, v2:v) = 
    match (v1, v2) with
    | (IVal i1, IVal i2) -> IVal (i1 % i2)
    | (FVal f1, FVal f2) -> FVal (f1 % f2)
    | (IVal i1, FVal f2) -> FVal ((float)i1 % f2)
    | (FVal f1, IVal i2) -> FVal (f1 % (float)i2)



// Grammar in BNF:
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <NR> <Topt>
// <Topt>     ::= "*" <NR> <Topt> | "/" <NR> <Topt> | "%" <NR> <Topt>| "^" <NR> <Topt> | <empty>
// <NR>       ::= "Num" <value> | "(" <E> ")" | "-" <NR> | add float stuff 

let parser tList = 
    let rec E tList = (T >> Eopt) tList         // >> is forward function composition operator: let inline (>>) f g x = g(f x)
    and Eopt tList = 
        match tList with
        | Add :: tail -> (T >> Eopt) tail
        | Sub :: tail -> (T >> Eopt) tail
        | _ -> tList
    and T tList = (NR >> Topt) tList
    and Topt tList =
        match tList with
        | Mul :: tail -> (NR >> Topt) tail
        | Div :: tail -> (NR >> Topt) tail
        | Rem :: tail -> (NR >> Topt) tail
        | Pow :: tail -> (NR >> Topt) tail
        | _ -> tList
    and NR tList =
        match tList with
        | Sub :: tail -> (NR >> Topt) tail
        | Flo value :: tail -> tail
        | Num value :: tail -> tail
        | Lpar :: tail -> match E tail with 
                          | Rpar :: tail -> tail
                          | _ -> raise parseError
        | _ -> raise parseError
    E tList

let parseNeval tList = 
    let rec E tList = (T >> Eopt) tList
    and Eopt (tList, value) = 
        match tList with
        | Add :: tail -> let (tLst, tval) = T tail
                         Eopt (tLst, add(value, tval))
        | Sub :: tail -> let (tLst, tval) = T tail
                         Eopt (tLst, sub(value, tval))
        | _ -> (tList, value)
    and T tList = (NR >> Topt) tList
    and Topt (tList, value: v) =
        match tList with
        | Mul :: tail -> let (tLst, tval) = NR tail
                         Topt (tLst, mul(value , tval))
        | Div :: tail -> let (tLst, tval) = NR tail
                         match (tval) with 
                         | IVal 0 ->
                                //Console.WriteLine("Division by zero error")
                                raise parseError        
                         | FVal 0.0 ->
                                //Console.WriteLine("Division by zero error")
                                raise parseError
                         | _ -> Topt (tLst, div(value, tval))

        | Rem :: tail -> let (tLst, tval) = NR tail
                         match (tval) with 
                         | IVal 0 ->
                                //Console.WriteLine("Division by zero error")
                                raise parseError        
                         | FVal 0.0 ->
                                //Console.WriteLine("Division by zero error")
                                raise parseError
                         | _ -> Topt (tLst, rem(value, tval))

        | Pow :: tail -> let (tLst, tval) = NR tail
                         Topt (tLst, pow(value, tval))
        | _ -> (tList, value)
    and NR tList =
        match tList with 
        | Sub :: tail -> let (tLst, value) = NR tail
                         match value with
                         | IVal i1 -> (tLst , IVal -i1)
                         | FVal f1 -> (tLst, FVal -f1)
                         
        | Flo value :: tail -> (tail, FVal value)
        | Num value :: tail -> (tail, IVal value)

        | Lpar :: tail -> let (tLst, tval) = E tail
                          match tLst with 
                          | Rpar :: tail -> (tail, tval)
                          | _ -> raise parseError
        
        | _ -> raise parseError
    E tList

let rec printTList (lst:list<terminal>) : list<string> = 
    match lst with
    head::tail -> Console.Write("{0} ",head.ToString())
                  printTList tail
                  
    | [] -> Console.Write("EOL\n")
            []


let eval (input: string) =
    try
        let oList = lexer input
        let Out = parseNeval oList
        match snd Out with
        | IVal i1 -> (true, sprintf "Result = %d" i1)
        | FVal f1 -> (true, sprintf "Result = %f" f1)
    with
    | ex -> (false, ex.Message)

[<Class>]
type Wrapper() =
    static member Evaluate(input) =
        eval input



[<EntryPoint>]
let main argv  =
    Console.WriteLine("Simple Interpreter!")
    let input:string = getInputString()
    let oList = lexer input
    let sList = printTList oList;
    let pList = printTList (parser oList)
    let Out = parseNeval oList
    match snd Out with
    | IVal i1 -> Console.WriteLine("Result = {0}", i1)
    | FVal f1 -> Console.WriteLine("Result = {0}", f1)
    0

