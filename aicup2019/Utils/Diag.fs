namespace Robocop.Utils

module Diag =
    let elapsed msg f = 
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let returnValue = f()
#if DEBUG
        printfn "%s elapsed Time: %i ms " msg timer.ElapsedMilliseconds
#endif
        returnValue

