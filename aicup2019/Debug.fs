namespace AiCup2019

type Debug(writer) =
    member _.draw(customData) =
#if DEBUG
        let message : Model.PlayerMessageGameCustomDataMessage = {Data = customData}
        message.writeTo writer
        writer.Flush()
#else
        ignore()
#endif

