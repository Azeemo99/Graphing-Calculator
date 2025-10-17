namespace FSharpInterpreter

open System
open System.Windows

[<EntryPoint>]
let main argv =
    // Create the application
    let app = App()
    
    // Show the main window
    let mainWindow = MainWindow()
    app.Run(mainWindow)

