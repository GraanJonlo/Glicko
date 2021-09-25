module Tests

open System

open Xunit
open FsUnit.Xunit

open Glicko

[<Fact>]
let ``Calculate c`` () =
    let newPlayerRD = Glicko.RD 350
    let typicalPlayerRD = Glicko.RD 50
    let timeToBaseUncertainty = Glicko.RatingPeriod 100

    Glicko.c newPlayerRD typicalPlayerRD timeToBaseUncertainty
    |> should (equalWithin 0.01) 34.64

[<Fact>]
let ``Determine new RD from time passing`` () =
    let newPlayerRD = Glicko.RD 350
    let typicalRD = Glicko.RD 50
    let timeToBaseUncertainty = Glicko.RatingPeriod 100
    let elapsedTime = Glicko.RatingPeriod 10
    let rd0 = Glicko.RD 200

    Glicko.updateRD newPlayerRD typicalRD timeToBaseUncertainty elapsedTime rd0
    |> should equal (Glicko.RD 228)

[<Fact>]
let ``Calculate q`` () =
    Glicko.q
    |> should (equalWithin 0.000001) 0.00575646273

[<Theory>]
[<InlineData(30., 0.9955)>]
[<InlineData(100., 0.9531)>]
[<InlineData(300., 0.7242)>]
let ``Calculate g`` (rd, expected) =
    Glicko.g (Glicko.RD rd)
    |> should (equalWithin 0.0001) expected

[<Theory>]
[<InlineData(1500, 1400, 30, 0.639)>]
[<InlineData(1500, 1550, 100, 0.432)>]
[<InlineData(1500, 1700, 300, 0.303)>]
[<InlineData(1500, 1500, 45, 0.5)>]
let ``Calculate E`` (r, rj, rdi, expected) =
    Glicko.e (Glicko.Rating r) (Glicko.Rating rj) (Glicko.RD rdi)
    |> should (equalWithin 0.001) expected

[<Fact>]
let ``Calculate dSquared`` () =
    let r = Glicko.Rating 1500

    let games =
        [ Glicko.Rating 1400, Glicko.RD 30, Glicko.Outcome.Won
          Glicko.Rating 1550, Glicko.RD 100, Glicko.Outcome.Lost
          Glicko.Rating 1700, Glicko.RD 300, Glicko.Outcome.Lost ]

    Glicko.dSquared r games
    |> sqrt
    |> should (equalWithin 0.1) 231.67

[<Fact>]
let ``Calculate post period r`` () =
    let r = Glicko.Rating 1500
    let rd = Glicko.RD 200

    let games =
        [ Glicko.Rating 1400, Glicko.RD 30, Glicko.Outcome.Won
          Glicko.Rating 1550, Glicko.RD 100, Glicko.Outcome.Lost
          Glicko.Rating 1700, Glicko.RD 300, Glicko.Outcome.Lost ]

    Glicko.newR r rd games
    |> should equal (Glicko.Rating 1464)

[<Fact>]
let ``Calculate post period rd`` () =
    let r = Glicko.Rating 1500
    let rd = Glicko.RD 200

    let games =
        [ Glicko.Rating 1400, Glicko.RD 30, Glicko.Outcome.Won
          Glicko.Rating 1550, Glicko.RD 100, Glicko.Outcome.Lost
          Glicko.Rating 1700, Glicko.RD 300, Glicko.Outcome.Lost ]

    Glicko.newRd r rd games
    |> should equal (Glicko.RD 151)

[<Fact>]
let ``Predict outcome`` () =
    let r1 = Glicko.Rating 1400
    let rd1 = Glicko.RD 80
    let r2 = Glicko.Rating 1500
    let rd2 = Glicko.RD 150

    Glicko.expectedOutcome r1 rd1 r2 rd2
    |> should (equalWithin 0.001) 0.376

[<Fact>]
let ``Calculate rating interval`` () =
    let r = Glicko.Rating 1500
    let rd = Glicko.RD 30

    Glicko.interval r rd
    |> should equal (Glicko.Rating 1441, Glicko.Rating 1559)
