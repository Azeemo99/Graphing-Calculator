
module InterpreterCore
//Advanced programing Tom  
// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

open System
open System.Collections.Generic
type builtInFunc = 
    | Sin | Cos | Tan | Exp | Log | Sqrt 

type Expr =
    | Const of float
    | Var of string
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr
    | Pow of Expr * Expr
    | Neg of Expr
    | Func of builtInFunc * Expr

type v = 
    | IVal of int
    | FVal of float

type terminal = 
    Add | Sub | Mul | Div | Lpar | Rpar | Num of int | Rem | Pow | Flo of float | Equ | Id of string| Func of builtInFunc

let symbolTable : Map <String, v> = Map.empty


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
    | c :: tail when isdigit c -> 
        scNumber(tail, sprintf "%s%c" iVal c, hasPoi)

    | c :: tail when isPoint c -> 
        if hasPoi then raise lexError
        else scNumber(tail, sprintf "%s%c" iVal c, true)

    | c :: tail when c = 'e' || c = 'E' ->
        let iValE = sprintf "%s%c" iVal c
        match tail with
        | signChar :: rest when signChar = '+' || signChar = '-' ->
            let iValSign = sprintf "%s%c" iValE signChar
            let (remaining, fullVal) = scanExponent(rest, iValSign)
            (remaining, fullVal, true)
        | digitChar :: rest when isdigit digitChar ->
            let iValDigit = sprintf "%s%c" iValE digitChar
            let (remaining, fullVal) = scanExponent(rest, iValDigit)
            (remaining, fullVal, true)
        | _ -> raise lexError

    | _ -> (iStr, iVal, hasPoi)

and scanExponent(iStr, iVal) =
    match iStr with
    | c :: tail when isdigit c ->
        scanExponent(tail, sprintf "%s%c" iVal c)
    | _ -> (iStr, iVal)


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
                                       if (id.ToLower() = "sin") then 
                                          Func Sin :: scan idStr
                                       else if (id.ToLower() = "cos") then
                                          Func Cos :: scan idStr
                                       else if (id.ToLower() = "tan") then 
                                          Func Tan :: scan idStr
                                       else if (id.ToLower() = "exp") then 
                                          Func Exp :: scan idStr
                                       else if (id.ToLower() = "log") then 
                                          Func Log :: scan idStr
                                       else if (id.ToLower() = "sqrt") then 
                                          Func Sqrt :: scan idStr

                                       else 
                                          Id id :: scan idStr
                                       

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

//function to check if the next value can be implicitly multiplied
let implicitMult token = 
    match token with 
    | Num _-> true
    | Flo _-> true
    | Func _-> true
    | Id _ -> true
    | Lpar -> true
    
    | _ -> false

//Type checking methods for operations 
//Currently working for floats and for Ints
//Might need to add some for rational, complex and e (not sure about e)
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

let sin(v1: v) = 
    match (v1) with 
    | (IVal i1) -> FVal (Math.Sin((float) i1))
    | (FVal f1) -> FVal (Math.Sin(f1))

let cos(v1: v) = 
    match (v1) with 
    | (IVal i1) -> FVal (Math.Cos((float) i1))
    | (FVal f1) -> FVal (Math.Cos(f1))

let tan(v1: v) = 
    match (v1) with 
    | (IVal i1) -> FVal (Math.Tan((float) i1))
    | (FVal f1) -> FVal (Math.Tan(f1))

let exp(v1: v) = 
    match (v1) with 
    | (IVal i1) -> FVal (Math.Exp((float) i1))
    | (FVal f1) -> FVal (Math.Exp(f1))

let log(v1 : v ) = 
    match (v1) with
    | (IVal i1) -> FVal (Math.Log10((float) i1))
    | (FVal f1) -> FVal (Math.Log10(f1))
let sqrt(v1: v) = 
    match (v1) with 
    | (IVal i1) -> FVal (Math.Sqrt((float) i1))
    | (FVal f1) -> FVal (Math.Sqrt(f1))

// Grammar in BNF:
// <STATEMENT> = ID "=" <E> |  <E>
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <Powopt>   ::= "^" <F> <Powopt>
// <F>        ::= <NR> <Powopt>
// <T>        ::= <F> <Topt>
// <Topt>     ::= "*" <NR> <Topt> | "/" <NR> <Topt> | "%" <NR> <Topt>| <empty>
// <NR>       ::= "Num" <value> | "(" <E> ")" | "-" <NR> | "Flo" <Value> | Func "(" <E> ")"


//Non-Evaluating Parser
let parser (tList) = 

    let rec E tList = (T >> Eopt) tList        
    and Eopt tList = 
        match tList with
        | Add :: tail -> (T >> Eopt) tail
        | Sub :: tail -> (T >> Eopt) tail
        
        | _ -> tList
    and T tList = (F >> Topt) tList
    
    and Topt tList =
        match tList with
        | Mul :: tail -> (F >> Topt) tail
        | Div :: tail -> (F >> Topt) tail
        | Rem :: tail -> (F >> Topt) tail
        | _ -> tList
    

    and F tList = (NR >> Powopt) tList
    and Powopt tList =
        match tList with
        | Pow:: tail -> (F >> Powopt) tail
        |_ -> tList
    and NR tList =
        match tList with
        | Sub :: tail -> NR tail
        | Flo value :: tail -> tail
        | Id name :: tail -> tail
        | Num value :: tail -> tail
        | Lpar :: tail -> match E tail with 
                          | Rpar :: more -> more
                          | _ -> raise parseError
        | Func ftype :: Lpar :: tail -> match E tail with 
                                        | Rpar :: more -> more
                                        | _ -> raise parseError
        
        | _ -> raise parseError
    E tList


//Evaluating parser 
let parseNeval (symbolTable : Map<String, v>, tList:  terminal list) : (terminal list * v * Map<string, v>)= 
    let rec E (tList: terminal list) : (terminal list * v * Map<string, v>) =
        let (tListAfterT, firstVal, symTable) = T tList
        Eopt (tListAfterT, firstVal, symTable)
    and Eopt (tList, value, symbolTable)  = 
        match tList with
        | Add :: tail -> let (tLst, tval, symbolTable) = T tail
                         Eopt (tLst, add(value, tval), symbolTable)
        | Sub :: tail -> let (tLst, tval, symbolTable) = T tail
                         Eopt (tLst, sub(value, tval), symbolTable)

        | _ -> (tList, value, symbolTable)
    and T (tList: terminal list) : (terminal list * v * Map<string, v>) =
        let (tListAfterF, firstVal, symTable) = F tList
        Topt (tListAfterF, firstVal, symTable)
    and Topt (tList, value: v, symbolTable) =
        match tList with
        | Mul :: tail -> let (tLst, tval, symbolTable) = F tail
                         Topt (tLst, mul(value , tval), symbolTable)
        | Div :: tail -> let (tLst, tval, symbolTable) = F tail
                         match (tval) with 
                         | IVal 0 ->
                                //Console.WriteLine("Division by zero error")
                                raise parseError        
                         | FVal 0.0 ->
                                //Console.WriteLine("Division by zero error")
                                raise parseError
                         | _ -> Topt (tLst, div(value, tval), symbolTable)

        | Rem :: tail -> let (tLst, tval, symbolTable) = F tail
                         match (tval) with 
                         | IVal 0 ->
                                //Console.WriteLine("Division by zero error")
                                raise parseError        
                         | FVal 0.0 ->
                                //Console.WriteLine("Division by zero error")
                                raise parseError
                         | _ -> Topt (tLst, rem(value, tval), symbolTable)

        | token :: _ when implicitMult(token) ->
            
            let (tLst, tval, symTable) = F tList
            Topt(tLst, mul(value, tval), symTable)
        | _ -> (tList, value, symbolTable)
    
    and F (tList: terminal list) : (terminal list * v * Map<string, v>) =
        let (tListAfterNR, firstVal, symTable) = NR tList
        Powopt (tListAfterNR, firstVal, symTable)
    
    and Powopt (tList, value, symbolTable) = 
        match tList with
        | Pow :: tail -> let (tLst, tval, symbolTable) = F tail
                         Powopt (tLst, pow(value, tval), symbolTable)   
        |_ -> (tList, value, symbolTable)




    and NR tList =
        match tList with 
        | Sub :: tail -> let (tLst, value, symbolTable) = NR tail
                         match value with
                         | IVal i1 -> (tLst , IVal -i1, symbolTable)
                         | FVal f1 -> (tLst, FVal -f1, symbolTable)
                         
        | Flo value :: tail -> (tail, FVal value, symbolTable)

        | Id name :: tail -> 
            match symbolTable.TryFind(name) with
            | Some value -> (tail, value, symbolTable)
            | None -> raise parseError
         
            
        
        | Num value :: tail -> (tail, IVal value, symbolTable)

        | Lpar :: tail -> 
            let (tLst, tval, symTable) = E tail
            match tLst with 
            | Rpar :: tail -> (tail, tval, symTable)
            | _ -> raise parseError
        
        | Func fType :: Lpar ::tail -> let (tLst, tval, symTable) = E tail
                                       match tLst with 
                                       | Rpar :: tail -> 
                                            match fType with 
                                            | Sin -> (tail, sin(tval), symTable)
                                            | Cos -> (tail, cos(tval), symTable)
                                            | Tan -> (tail, tan(tval), symTable)
                                            | Exp -> (tail, exp(tval), symTable)
                                            | Log -> (tail, log(tval), symTable)
                                            | Sqrt -> (tail, sqrt(tval), symTable)
                                            | _ -> raise parseError     
                                       | _ -> raise parseError
                                    

        
        | _ -> raise parseError
    E tList

//statement parser

let parseStatement (symbolTable: Map<string, v>, tList: terminal list) : (terminal list * v * Map<string, v>) =
    match tList with
    | Id name :: Equ :: tail ->
        
        let (rem, value, symTableAfterEval) = parseNeval (symbolTable, tail)
        
        let updatedSymTable = symTableAfterEval.Add(name, value)
        (rem, value, updatedSymTable)
    | _ ->
        
        parseNeval (symbolTable, tList)

let rec printTList (lst:list<terminal>) : list<string> = 
    match lst with
    head::tail -> Console.Write("{0} ",head.ToString())
                  printTList tail
                  
    | [] -> Console.Write("EOL\n")
            []

//Eval Function
let eval (symTable, input: string) =
    try
        let oList = lexer input
        let (rem, value, updatedSymTable) = parseStatement (symTable, oList)
        match value with
        | IVal i1 -> (true, sprintf "Result = %d" i1, updatedSymTable)
        | FVal f1 -> (true, sprintf "Result = %f" f1, updatedSymTable)
    with
    | ex -> (false, ex.Message, symTable)

let plotEval (symbolTable : Map<string, v>, input: string, xVals: float list) =
    let tokens = lexer input
    // Handle two cases:
    // Case 1: starts with y = ...
    // Case 2: just an expression (constant or function)
    let exprTokens =
        match tokens with
        | Id "y" :: Equ :: tail -> tail
        | _ -> tokens  // use tokens directly if no "y ="
    
    [for x in xVals do
        let temp = symbolTable.Add("x", FVal x)
        let (_, yVal, _) = parseNeval(temp, exprTokens)
        let yFloat =
            match yVal with
            | IVal i -> float i
            | FVal f -> f
        yield (x, yFloat)]

//diff an x value 

let diffXVal(n1, id, n2) = 
    if n2 = 2 then 
        $"{n1 * n2}x"
    elif n2 = 1 then 
        $"{n1 * n2}"
    else 
        $"{n1 * n2}x^{n2-1}"


let rec recDiff (tokens, value) =
    match tokens with

    // --- ax^(-n) with coefficient ---
    | Num n1 :: Id name :: Pow :: Sub :: Num n2 :: tail ->
        recDiff(tail, value + diffXVal(n1, name, -n2))

    // --- ax^n with coefficient ---
    | Num n1 :: Id name :: Pow :: Num n2 :: tail ->
        recDiff(tail, value + diffXVal(n1, name, n2))

    // --- x^(-n) ---
    | Id name :: Pow :: Sub :: Num n2 :: tail ->
        recDiff(tail, value + diffXVal(1, name, -n2))

    // --- x^n ---
    | Id name :: Pow :: Num n2 :: tail ->
        recDiff(tail, value + diffXVal(1, name, n2))

    // --- ax ---
    | Num n1 :: Id name :: tail ->
        recDiff(tail, value + diffXVal(n1, name, 1))

    // --- x ---
    | Id name :: tail ->
        recDiff(tail, value + diffXVal(1, name, 1))

    // --- constants ---
    | Num _ :: tail ->
        recDiff(tail, value + "0")

    | Flo _ :: tail ->
        recDiff(tail, value + "0")

    // --- operators ---
    | Add :: tail ->
        recDiff(tail, value + "+")

    | Sub :: tail ->
        recDiff(tail, value + "-")

    // --- end of token stream ---
    | [] ->
        ([], value)

    // --- unexpected token ---
    | _ ->
        ([], value)


    
let rec diff expression =
    "a"




/////Chris work

///evaluate the interpreter expression at a given x value
let evalAtX (expr:string, symTable) (xValue:float) : float =
    //replace "x" with the number in the string
    let replaced = expr.Replace("x", sprintf "(%f)" xValue)
    match eval(symTable, replaced) with
    | true, resultStr, symTable ->
        //extract float from "Result = ..." string
        let parts = resultStr.Split("=")
        float (parts.[1].Trim())
    | false, msg, symTable -> failwithf "Error evaluating expression: %s" msg

///generate points for plotting: [(x1, y1); (x2, y2); ...]
let evalPoly (expr:string) (xMin:float) (xMax:float) (dx:float) =
    [xMin .. dx .. xMax] |> List.map (fun x -> (x, evalAtX( expr, symbolTable)))

 
/////



//Connection to the WPF
[<Class>]
type Wrapper() =
    // symbol table stored in a reference cell, not mutable
    static let symRef = ref symbolTable   // symbolTable comes from the interpreter

    static member Evaluate(input: string) : string =
        let (success, result, updatedTable) = eval(!symRef, input)
        symRef := updatedTable   // update reference to new map, no `mutable` keyword used
        result

    //parse a polynomial string like "1 0 -2" -> float list [1.0; 0.0; -2.0]
    static member ParsePolynomial(input: string) : float[] =
        input.Split(' ')
        |> Array.map (fun s -> float (s.Trim()))

    //evaluate polynomial at points x from start to end with step
    static member EvalPoly(coeffs: float[], xStart: float, xEnd: float, step: float) : (float*float)[] =
        let mutable xs = []
        let mutable x = xStart
        while x <= xEnd do
            let y = coeffs |> Array.mapi (fun i c -> c * (x ** float i)) |> Array.sum
            xs <- (x, y)::xs
            x <- x + step
        xs |> List.rev |> List.toArray




