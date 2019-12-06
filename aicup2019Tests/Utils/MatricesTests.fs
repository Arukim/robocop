namespace aicup2019Tests.Utils

open NUnit.Framework
open Robocop.Utils

module MatricesTests =
    [<Test>]
    let allBorders_Works () =
        let testData = Array.create 3 0 |> Array.create 3

        let borders = Matrices.allBorders(testData)

        Assert.That(borders, Is.EquivalentTo(
                                seq{
                                    (0,0);
                                    (0,1);(0,2);
                                    (1,0);(2,0);
                                    (2,1);(2,2);
                                    (1,2)
                                })) 

    [<Test>]
    let allTilesG_Works () =
        let testData = Array.create 3 0 |> Array.create 3

        let borders = testData |> Matrices.allTilesG(fun x y _ -> (x,y))

        Assert.That(borders, Is.EquivalentTo(
                                seq{
                                    (0,0); (0,1); (0,2);
                                    (1,0); (1,1); (1,2);
                                    (2,0); (2,1); (2,2);
                                })) 