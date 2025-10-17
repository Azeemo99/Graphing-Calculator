namespace FSharpInterpreter

module Interpreter =

    /// Evaluates a simple arithmetic expression given as a string.
    /// Currently supports +, -, *, /.
    let evaluate (expr: string) : float option =
        try
            // WARNING: This is a very simple parser using F# Interactive eval
            // For safety, consider writing your own parser for production
            let result =
                Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation <@@
                    let v = System.Data.DataTable().Compute(expr, "") 
                    float v
                @@
            Some result
        with
        | _ -> None

    /// Example usage
    let test () =
        let examples = ["2 + 3"; "10 / 2"; "5 * 7"; "invalid"]
        for e in examples do
            match evaluate e with
            | Some v -> printfn "%s = %f" e v
            | None -> printfn "%s is invalid" e
