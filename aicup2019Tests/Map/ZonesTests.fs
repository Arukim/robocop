namespace aicup2019Tests.Zones

open NUnit.Framework
open Robocop.Map

module ZonesTests =
    [<Test>]    
    let buildGrounds_GivenSimpleSingleGround_Works () =
        let map = [(0,0); (1,0); (2,0); (3,0);
                   (0,1);               (3,1);
                   (0,2);               (3,2);
                   (0,3); (1,3); (2,3); (3,3);]
        let data = map 
                    |> List.map(fun c -> {X = fst c; Y = snd c} : Cell)
                    |> Seq.ofList
        
        let grounds = data |> Zones.buildGrounds(fun g -> g)

        Assert.That(grounds |> Seq.length, Is.EqualTo(1))
        Assert.That(grounds |> Seq.head, Is.EquivalentTo(
                                            seq<Cell> {
                                                {X=1;Y=0}; {X=2;Y=0};
                                            }))
      
    [<Test>]    
    let buildGrounds_GivenSimpleDoubleGround_Works () =
        let map = [(0,0); (1,0); (2,0);        (4,0); (5,0); (6,0);
                   (0,1);                                    (6,1);
                   (0,2);                                    (6,2);
                   (0,3); (1,3); (2,3); (3,3); (4,3); (5,3); (6,3);]
        let data = map 
                    |> List.map(fun c -> {X = fst c; Y = snd c} : Cell)
                    |> Seq.ofList
        
        let grounds = data |> Zones.buildGrounds(fun g -> g)

        Assert.That(grounds |> Seq.length, Is.EqualTo(2))
        Assert.That(grounds |> Seq.head, Is.EquivalentTo(
                                            seq<Cell> {
                                                {X=1;Y=0}; {X=2;Y=0};
                                            }))
                                            
        Assert.That(grounds |> Seq.skip 1
                              |> Seq.head, Is.EquivalentTo(
                                            seq<Cell> {
                                                {X=4;Y=0}; {X=5;Y=0};
                                            }))

    [<Test>]    
    let buildGrounds_GivenSinglePoint_Works () =
        let map = [(0,0);        (2,0);                      (6,0);
                   (0,1);                                    (6,1);
                   (0,2);                                    (6,2);
                   (0,3); (1,3); (2,3); (3,3); (4,3); (5,3); (6,3);]
        let data = map 
                    |> List.map(fun c -> {X = fst c; Y = snd c} : Cell)
                    |> Seq.ofList
    
        let grounds = data |> Zones.buildGrounds(fun g -> g)
        
        Assert.That(grounds |> Seq.length, Is.EqualTo(1))
        Assert.That(grounds |> Seq.head, Is.EquivalentTo(
                                            seq<Cell> {
                                                {X=2;Y=0};
                                            }))
    [<Test>]    
    let buildGrounds_GivenTwoPoints_Works () =
        let map = [(0,0);        (2,0);        (4,0);        (6,0);
                   (0,1);                                    (6,1);
                   (0,2);                                    (6,2);
                   (0,3); (1,3); (2,3); (3,3); (4,3); (5,3); (6,3);]
        let data = map 
                    |> List.map(fun c -> {X = fst c; Y = snd c} : Cell)
                    |> Seq.ofList
    
        let grounds = data |> Zones.buildGrounds(fun g -> g)
        
        Assert.That(grounds |> Seq.length, Is.EqualTo(2))
        Assert.That(grounds |> Seq.head, Is.EquivalentTo(
                                            seq<Cell> {
                                                {X=2;Y=0};
                                            }))
        
        Assert.That(grounds |> Seq.skip 1
                              |> Seq.head, Is.EquivalentTo(
                                            seq<Cell> {
                                                {X=4;Y=0};
                                            }))

