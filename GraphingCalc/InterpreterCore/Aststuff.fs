

module Aststuff
open System
open System.Collections.Generic
open InterpreterCore


type Expr =
    | ConstE of float
    | VarE of string
    | AddE of Expr * Expr
    | SubE of Expr * Expr
    | MulE of Expr * Expr
    | DivE of Expr * Expr
    | RemE of Expr * Expr
    | PowE of Expr * Expr
    | NegE of Expr
    | FuncE of builtInFunc * Expr
    | AssignE of string * Expr





let rec parseStatement (tokens: terminal list) : Expr * terminal list =
        match tokens with
        | Id name :: Equ :: tail ->
            let (expr, rest) = parseE tail
            // represent assignment as Add(Var name, expr)? Or handle separately
            expr, rest
        | _ ->
            parseE tokens

    and parseE tokens =
        let tExpr, rest = parseT tokens
        parseEOpt(tExpr, rest)

    and parseEOpt (left: Expr, tokens: terminal list) =
        match tokens with
        | Add :: tail ->
            let tExpr, rest = parseT tail
            parseEOpt(AddE(left, tExpr), rest)
        | Sub :: tail ->
            let tExpr, rest = parseT tail
            parseEOpt(SubE(left, tExpr), rest)
        | _ -> left, tokens

    and parseT tokens =
        let fExpr, rest = parseF tokens
        parseTOpt(fExpr, rest)

    and parseTOpt (left: Expr, tokens: terminal list) =
        match tokens with
        | Mul :: tail ->
            let fExpr, rest = parseF tail
            parseTOpt(MulE(left, fExpr), rest)
        | Div :: tail ->
            let fExpr, rest = parseF tail
            parseTOpt(DivE(left, fExpr), rest)
        | Rem :: tail ->
            let fExpr, rest = parseF tail
            parseTOpt(RemE(left, fExpr), rest)
        | _ -> left, tokens

    and parseF tokens =
        let nrExpr, rest = parseNR tokens
        parsePowOpt(nrExpr, rest)

    and parsePowOpt (left: Expr, tokens: terminal list) =
        match tokens with
        | Pow :: tail ->
            let fExpr, rest = parseF tail
            parsePowOpt(PowE(left, fExpr), rest)
        | _ -> left, tokens

    and parseNR tokens =
        match tokens with
        | Sub :: tail ->
            let expr, rest = parseNR tail
            NegE expr, rest
        | Num n :: tail -> ConstE (float n), tail
        | Flo f :: tail -> ConstE f, tail
        | Id name :: tail -> VarE name, tail
        | Lpar :: tail ->
            let expr, rest = parseE tail
            match rest with
            | Rpar :: restTail -> expr, restTail
            | _ -> raise parseError
        | Func fType :: Lpar :: tail ->
            let expr, rest = parseE tail
            match rest with
            | Rpar :: restTail -> FuncE(fType, expr), restTail
            | _ -> raise parseError
        | _ -> raise parseError

let rec evalAST (expr: Expr) (symTable: Map<string,v>) : v =
    match expr with
    | ConstE f -> FVal f
    | VarE name -> 
        match symTable.TryFind(name) with
        | Some value -> value
        | None -> failwith $"Variable {name} not found"
    | AddE(a,b) -> add(evalAST a symTable, evalAST b symTable)
    | SubE(a,b) -> sub(evalAST a symTable, evalAST b symTable)
    | MulE(a,b) -> mul(evalAST a symTable, evalAST b symTable)
    | DivE(a,b) -> div(evalAST a symTable, evalAST b symTable)
    | RemE(a,b) -> rem(evalAST a symTable, evalAST b symTable)
    | PowE(a,b) -> pow(evalAST a symTable, evalAST b symTable)
    | NegE a -> 
        match evalAST a symTable with
        | IVal i -> IVal -i
        | FVal f -> FVal -f
    | FuncE(fType,arg) ->
        match fType with
        | Sin -> sin(evalAST arg symTable)
        | Cos -> cos(evalAST arg symTable)
        | Tan -> tan(evalAST arg symTable)
        | Exp -> exp(evalAST arg symTable)
        | Log -> log(evalAST arg symTable)
        | Sqrt -> sqrt(evalAST arg symTable)
    | AssignE(name, valueExpr) ->
        let v = evalAST valueExpr symTable
        v  // optionally, update symbol table outside this function


let rec simplify expr =
    match expr with
    | AddE(a,b) -> 
        let a', b' = simplify a, simplify b
        match a', b' with
        | ConstE 0.0, b'' -> b''
        | a'', ConstE 0.0 -> a''
        | ConstE x, ConstE y -> ConstE (x + y)
        | _ -> AddE(a', b')
    | SubE(a,b) ->
        let a', b' = simplify a, simplify b
        match a', b' with
        | ConstE 0.0, b'' -> NegE b''
        | a'', ConstE 0.0 -> a''
        | ConstE x, ConstE y -> ConstE (x - y)
        | _ -> SubE(a', b')
    | MulE(a,b) ->
        let a', b' = simplify a, simplify b
        match a', b' with
        | ConstE 0.0, _ | _, ConstE 0.0 -> ConstE 0.0
        | ConstE 1.0, b'' -> b''
        | a'', ConstE 1.0 -> a''
        | ConstE x, ConstE y -> ConstE (x * y)
        | _ -> MulE(a', b')
    | DivE(a,b) ->
        let a', b' = simplify a, simplify b
        match a', b' with
        | ConstE 0.0, _ -> ConstE 0.0
        | a'', ConstE 1.0 -> a''
        | _ -> DivE(a', b')
    | PowE(a,b) ->
        let a', b' = simplify a, simplify b
        match a', b' with
        | _, ConstE 1.0 -> a'          // x^1 -> x
        | PowE(x, ConstE m), ConstE n -> PowE(x, ConstE(m*n)) // (x^a)^b -> x^(a*b)
        | ConstE x, ConstE y -> ConstE (x ** y)
        | _ -> PowE(a', b')
    | NegE a ->
        match simplify a with
        | ConstE x -> ConstE(-x)
        | NegE b -> b
        | a' -> NegE a'
    | _ -> expr









let rec diff expr var =
    match expr with
    | ConstE _ -> ConstE 0.0
    | VarE v when v = var -> ConstE 1.0
    | VarE _ -> ConstE 0.0
    | AddE(a,b) -> AddE(diff a var, diff b var)
    | SubE(a,b) -> SubE(diff a var, diff b var)
    | MulE(a,b) -> AddE(MulE(diff a var, b), MulE(a, diff b var))
    | DivE(a,b) -> DivE(SubE(MulE(diff a var, b), MulE(a, diff b var)), PowE(b, ConstE 2.0))
    
    // Power rule: u^n (n constant)
    | PowE(u, ConstE n) -> MulE(ConstE n, MulE(PowE(u, ConstE(n-1.0)), diff u var))
    
    // Power rule: a^v (a constant)
    | PowE(ConstE a, v) -> MulE(MulE(PowE(ConstE a, v), FuncE(Log, ConstE a)), diff v var)
    
    // General power rule: u^v
    | PowE(u, v) -> 
        let du = diff u var
        let dv = diff v var
        MulE(PowE(u, v), AddE(MulE(dv, FuncE(Log,u)), MulE(v, DivE(du,u))))
    
    | NegE a -> NegE(diff a var)
    | FuncE(f,arg) ->
        let dArg = diff arg var
        match f with
        | Sin -> MulE(dArg, FuncE(Cos, arg))
        | Cos -> MulE(dArg, NegE(FuncE(Sin, arg)))
        | Tan -> MulE(dArg, DivE(ConstE 1.0, PowE(FuncE(Cos,arg), ConstE 2.0)))
        | Exp -> MulE(dArg, FuncE(Exp, arg))
        | Log -> DivE(dArg, arg)
        | Sqrt -> DivE(dArg, MulE(ConstE 2.0, FuncE(Sqrt, arg)))
        | _ -> failwith "Function derivative not implemented"
    | AssignE(_, e) -> diff e var


let differentiate (input: string) : string =
    try
        let tokens = lexer input
        let ast, _ = parseStatement tokens
        let astSimplified = simplify ast
        let derivative = diff astSimplified "x"
        let derivativeSimplified = simplify derivative
        
        let rec exprToString expr =
            let wrapIfNeeded e =
                match e with
                | AddE(_,_) | SubE(_,_) -> "(" + exprToString e + ")"
                | _ -> exprToString e

            match expr with
            | ConstE f -> 
                if f < 0.0 then "(" + string f + ")" else string f
            | VarE v -> v
            | AddE(a,b) -> exprToString a + " + " + exprToString b
            | SubE(a,b) -> exprToString a + " - " + exprToString b
            | MulE(ConstE 1.0, e) | MulE(e, ConstE 1.0) -> exprToString e
            | MulE(ConstE c, e) | MulE(e, ConstE c) -> 
                if c = 1.0 then exprToString e
                elif c = -1.0 then "-" + exprToString e
                else string c + wrapIfNeeded e
            | MulE(a,b) -> wrapIfNeeded a + wrapIfNeeded b
            | DivE(a,b) -> wrapIfNeeded a + "/" + wrapIfNeeded b
            | PowE(a,b) -> wrapIfNeeded a + "^" + wrapIfNeeded b
            | NegE a -> "-" + wrapIfNeeded a
            | FuncE(f,arg) -> f.ToString() + "(" + exprToString arg + ")"
            | RemE(a,b) -> wrapIfNeeded a + "%" + wrapIfNeeded b



        
        exprToString derivativeSimplified
    with
    | ex -> "Error differentiating: " + ex.Message
