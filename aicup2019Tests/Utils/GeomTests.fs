namespace aicup2019Tests.Utils

open NUnit.Framework
open Robocop.Utils

module GeomTests =
   [<Test>]
   let lineEquation_GivenCorrectPoints_Works () =
       Assert.That(Geom.lineEquation (0.0,0.0) (2.0,1.0) , Is.EqualTo (0.5, 0.0))
