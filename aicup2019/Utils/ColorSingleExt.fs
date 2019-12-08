namespace Robocop.Utils

open AiCup2019.Model
open System.Drawing

module ColorSingle =
    let from (k:KnownColor) =
        let c = Color.FromKnownColor(k)
        let inline encode (v:byte) =
            (single v) / 255.0f
        {
            R = encode c.R
            G = encode c.G
            B = encode c.B
            A = encode c.A
        }