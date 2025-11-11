
module InterpreterCore
//Advanced programing Tom  
// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

open System
type builtInFunc = 
    | Sin | Cos | Tan | Exp | Log | Sqrt 

type v = 
    | IVal of int
    | FVal of float

type terminal = 
    Add | Sub | Mul | Div | Lpar | Rpar | Num of int | Rem | Pow | Flo of float | Equ | Id of string| Func of builtInFunc

let str2lst s = [for c in s -> c]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let isPoint c = (c = '.')
let isEqu c = (c = '=')
let isLetter c = System.Char.IsAsciiLetter c
let lexError = System.Exception("Lexer error")
let intVal (c:char) = (int)((int)c - (int)'0')
let parseError = System.Exception("Parser error")
let env = new System.Collections.Generic.Dictionary<string, v>()


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
//<Stmt> ::= Id "=" <E> | <E>
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <Powopt>   ::= "^" <F> <Powopt>
// <F>        ::= <NR> <Powopt>
// <T>        ::= <F> <Topt>
// <Topt>     ::= "*" <NR> <Topt> | "/" <NR> <Topt> | "%" <NR> <Topt>| <empty>
// <NR>       ::= "Num" <value> | "(" <E> ")" | "-" <NR> | "Flo" <Value> | Func "(" <E> ")"


//Non-Evaluating Parser
let parser tList = 
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
let parseNeval tList = 
    let rec E tList = (T >> Eopt) tList
    and Eopt (tList, value) = 
        match tList with
        | Add :: tail -> let (tLst, tval) = T tail
                         Eopt (tLst, add(value, tval))
        | Sub :: tail -> let (tLst, tval) = T tail
                         Eopt (tLst, sub(value, tval))

        | _ -> (tList, value)
    and T tList = (F >> Topt) tList
    and Topt (tList, value: v) =
        match tList with
        | Mul :: tail -> let (tLst, tval) = F tail
                         Topt (tLst, mul(value , tval))
        | Div :: tail -> let (tLst, tval) = F tail
                         match (tval) with 
                         | IVal 0 ->
                                //Console.WriteLine("Division by zero error")
                                raise parseError        
                         | FVal 0.0 ->
                                //Console.WriteLine("Division by zero error")
                                raise parseError
                         | _ -> Topt (tLst, div(value, tval))

        | Rem :: tail -> let (tLst, tval) = F tail
                         match (tval) with 
                         | IVal 0 ->
                                //Console.WriteLine("Division by zero error")
                                raise parseError        
                         | FVal 0.0 ->
                                //Console.WriteLine("Division by zero error")
                                raise parseError
                         | _ -> Topt (tLst, rem(value, tval))


        | _ -> (tList, value)
    
    and F tList = (NR >> Powopt) tList
    
    and Powopt (tList, value) = 
        match tList with
        | Pow :: tail -> let (tLst, tval) = F tail
                         Powopt (tLst, pow(value, tval))    
        |_ -> (tList, value)




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
        
        | Func fType :: Lpar ::tail -> let (tLst, tval) = E tail
                                       match tLst with 
                                       | Rpar :: tail -> 
                                            match fType with 
                                            | Sin -> (tail, sin(tval))
                                            | Cos -> (tail, cos(tval))
                                            | Tan -> (tail, tan(tval))
                                            | Exp -> (tail, exp(tval))
                                            | Log -> (tail, log(tval))
                                            | Sqrt -> (tail, sqrt(tval))
                                            | _ -> raise parseError     
                                       | _ -> raise parseError
                                    

        
        | _ -> raise parseError
    E tList

let parseStatement tList =
    match tList with
    | Id name :: Equ :: tail ->
        let (tRest, value) = parseNeval tail
        env.[name] <- value
        (tRest, value)
    | _ ->
        parseNeval tList


let rec printTList (lst:list<terminal>) : list<string> = 
    match lst with
    head::tail -> Console.Write("{0} ",head.ToString())
                  printTList tail
                  
    | [] -> Console.Write("EOL\n")
            []

//Eval Function
let eval (input: string) =
    try
        let oList = lexer input
        let Out = parseStatement oList
        match snd Out with
        | IVal i1 -> (true, sprintf "Result = %d" i1)
        | FVal f1 -> (true, sprintf "Result = %f" f1)
    with
    | ex -> (false, ex.Message)



/////Chris work

///evaluate the interpreter expression at a given x value
let evalAtX (expr:string) (xValue:float) : float =
    //replace "x" with the number in the string
    let replaced = expr.Replace("x", sprintf "(%f)" xValue)
    match eval replaced with
    | true, resultStr ->
        //extract float from "Result = ..." string
        let parts = resultStr.Split("=")
        float (parts.[1].Trim())
    | false, msg -> failwithf "Error evaluating expression: %s" msg

///generate points for plotting: [(x1, y1); (x2, y2); ...]
let evalPoly (expr:string) (xMin:float) (xMax:float) (dx:float) =
    [xMin .. dx .. xMax] |> List.map (fun x -> (x, evalAtX expr x))

 
/////



//Connection to the WPF
[<Class>]
type Wrapper() =
    static member Evaluate(input) =
        eval input

(*
//Connection to the Console Application
[<EntryPoint>]
let main argv  =
    Console.WriteLine("Simple Interpreter!!")
    let input:string = getInputString()
    let oList = lexer input
    let sList = printTList oList;
    let pList = printTList (parser oList)
    let Out = parseNeval oList
    match snd Out with
    | IVal i1 -> Console.WriteLine("Result = {0}", i1)
    | FVal f1 -> Console.WriteLine("Result = {0}", f1)
    0*)

