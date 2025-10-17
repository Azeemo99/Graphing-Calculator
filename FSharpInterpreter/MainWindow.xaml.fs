namespace FSharpInterpreter

open System
open System.Windows
open System.Windows.Controls

type MainWindow() as this =
    inherit Window()

    do
        // Link the XAML to this code-behind
        let uri = new Uri("/FSharpInterpreter;component/MainWindow.xaml", UriKind.Relative)
        Application.LoadComponent(this, uri)

        // Get controls by name from XAML
        let inputBox = this.FindName("InputBox") :?> TextBox
        let resultBox = this.FindName("ResultBox") :?> TextBlock
        let evalButton = this.FindName("EvalButton") :?> Button

        // Wire up button click
        evalButton.Click.Add(fun _ ->
            let expr = inputBox.Text
            match Interpreter.evaluate expr with
            | Some v -> resultBox.Text <- sprintf "%f" v
            | None -> resultBox.Text <- "Invalid expression"
        )
