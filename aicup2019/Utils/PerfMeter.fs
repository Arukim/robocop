namespace Robocop.Utils

open System
open System.Diagnostics

type PerfMeter = 
    val mutable eventsCount:int
    val mutable min: int64
    val mutable max: int64
    val mutable last: int64
    val mutable toSkip: int
    val sw: Stopwatch
    val metricsName: string

    new(metricsName, initialSkip) = {
        eventsCount = 0; min = Int64.MaxValue; max = 0L; last = 0L;
        sw = new Stopwatch()
        metricsName = metricsName
        toSkip = initialSkip
    }

    member this.Start () = 
        if this.toSkip = 0 then this.sw.Start()

    member this.Stop () =
        if this.toSkip > 0 then
            this.toSkip <- this.toSkip - 1
        else
            this.sw.Stop() 
            let diff = this.sw.ElapsedMilliseconds - this.last
            this.min <- if diff < this.min then diff else this.min
            this.max <- if diff > this.max then diff else this.max
            this.eventsCount <- this.eventsCount + 1
            this.last <- this.sw.ElapsedMilliseconds
        ignore()

    member this.Summarize () = 
        let avg = float this.sw.ElapsedMilliseconds / float this.eventsCount
        let dur = this.sw.ElapsedMilliseconds
        sprintf "%s avg: %.2fms min: %Ams max %Ams dur: %A events: %A" this.metricsName avg this.min this.max dur this.eventsCount
        

