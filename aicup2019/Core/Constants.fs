namespace Robocop.Core

module Constants =
    let Unit_Jump_Time = 0.55f
    let Max_Jump_Pad = 11
    let Ticks_Per_Second = 60
    let Max_Speed = 10.0f
    let Tick_Movement = Max_Speed / single Ticks_Per_Second
    let Max_Jump = Max_Speed * Unit_Jump_Time
    let One_Tick_Move = Max_Speed / (single Ticks_Per_Second)
    let One_Tick_Bullet_Move = 50.0f / (single Ticks_Per_Second)
    let One_Tick_Grenade_Move = 20.0f / (single Ticks_Per_Second)
    let Bullet_Evasion_Distance = 5.0f
    let Evasion_Time = Ticks_Per_Second / 5