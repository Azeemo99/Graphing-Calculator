namespace FSharpInterpreter

open System.Windows

type App() =
    inherit Application()

    override this.OnStartup(e) =
        base.OnStartup(e)
        // Initialize things here
        let mainWindow = MainWindow()
        mainWindow.Show()
