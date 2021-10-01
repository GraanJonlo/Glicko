module Tests

open System

open NodaTime
open Xunit
open FsUnit.Xunit

open Glicko

[<Fact>]
let ``Calculate c`` () =
    let newPlayerRD = Glicko.RD 350
    let typicalPlayerRD = Glicko.RD 50
    let timeToBaseUncertainty = 100

    Glicko.c newPlayerRD typicalPlayerRD timeToBaseUncertainty
    |> should (equalWithin 0.01) 34.64

[<Fact>]
let ``Determine new RD from time passing`` () =
    let newPlayerRD = Glicko.RD 350
    let typicalRD = Glicko.RD 50
    let daysToInitialRD = 100
    let elapsedDays = 10
    let rd0 = Glicko.RD 200

    Glicko.updateRD newPlayerRD typicalRD daysToInitialRD elapsedDays rd0
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
        [ "", Glicko.Rating 1400, Glicko.RD 30, Glicko.Outcome.Won
          "", Glicko.Rating 1550, Glicko.RD 100, Glicko.Outcome.Lost
          "", Glicko.Rating 1700, Glicko.RD 300, Glicko.Outcome.Lost ]

    Glicko.dSquared r games
    |> sqrt
    |> should (equalWithin 0.1) 231.67

[<Fact>]
let ``Calculate post period r`` () =
    let r = Glicko.Rating 1500
    let rd = Glicko.RD 200

    let games =
        [ "", Glicko.Rating 1400, Glicko.RD 30, Glicko.Outcome.Won
          "", Glicko.Rating 1550, Glicko.RD 100, Glicko.Outcome.Lost
          "", Glicko.Rating 1700, Glicko.RD 300, Glicko.Outcome.Lost ]

    Glicko.newR r rd games
    |> should equal (Glicko.Rating 1464)

[<Fact>]
let ``Calculate post period rd`` () =
    let r = Glicko.Rating 1500
    let rd = Glicko.RD 200

    let games =
        [ "", Glicko.Rating 1400, Glicko.RD 30, Glicko.Outcome.Won
          "", Glicko.Rating 1550, Glicko.RD 100, Glicko.Outcome.Lost
          "", Glicko.Rating 1700, Glicko.RD 300, Glicko.Outcome.Lost ]

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

[<Fact>]
let ``Calculate player state at date`` () =
    let records =
        [ LocalDate(2021, 9, 29), [ "Alice", Glicko.Rating 1375, Glicko.RD 30, Glicko.Outcome.Draw ]
          LocalDate(2021, 9, 28),
          [ "Alice", Glicko.Rating 1400, Glicko.RD 30, Glicko.Outcome.Won
            "Eve", Glicko.Rating 1550, Glicko.RD 100, Glicko.Outcome.Lost
            "Tony", Glicko.Rating 1700, Glicko.RD 300, Glicko.Outcome.Lost ] ]

    Glicko.calcPlayerState (Glicko.Rating 1500) (Glicko.RD 350) (Glicko.RD 50) 100 (LocalDate(2021, 9, 28)) records
    |> should equal (Glicko.Rating 1500, Glicko.RD 350)

    Glicko.calcPlayerState (Glicko.Rating 1500) (Glicko.RD 350) (Glicko.RD 50) 100 (LocalDate(2021, 9, 29)) records
    |> should equal (Glicko.Rating 1442, Glicko.RD 196)

    Glicko.calcPlayerState (Glicko.Rating 1500) (Glicko.RD 350) (Glicko.RD 50) 100 (LocalDate(2021, 9, 30)) records
    |> should equal (Glicko.Rating 1426, Glicko.RD 173)

[<Fact>]
let ``Calculate player state at date 2`` () =
    let records =
        [ LocalDate(2021, 9, 28), [ "Alice", Glicko.Rating 1500, Glicko.RD 350, Glicko.Outcome.Won ] ]

    Glicko.calcPlayerState (Glicko.Rating 1500) (Glicko.RD 350) (Glicko.RD 50) 100 (LocalDate(2021, 9, 29)) records
    |> should equal (Glicko.Rating 1662, Glicko.RD 292)

[<Fact>]
let ``Foo`` () =
    let tournament = Map.empty

    let add player1 player2 (date: LocalDate) outcome tournament =
        let p1Records =
            match Map.tryFind player1 tournament with
            | Some x -> x
            | None -> List.empty

        let p2Records =
            match Map.tryFind player2 tournament with
            | Some x -> x
            | None -> List.empty

        let (p1Rating, p1Rd) =
            Glicko.calcPlayerState (Glicko.Rating 1500) (Glicko.RD 350) (Glicko.RD 50) 100 date p1Records

        let (p2Rating, p2Rd) =
            Glicko.calcPlayerState (Glicko.Rating 1500) (Glicko.RD 350) (Glicko.RD 50) 100 date p2Records

        let p1RecordsNew =
            p1Records
            |> Map.ofList
            |> Map.change
                date
                (function
                | None -> [ (player2, p2Rating, p2Rd, outcome) ] |> Some
                | Some x -> (player2, p2Rating, p2Rd, outcome) :: x |> Some)
            |> Map.toList
            |> List.sortByDescending (fun (x, _) -> x)

        let p2RecordsNew =
            p2Records
            |> Map.ofList
            |> Map.change
                date
                (function
                | None ->
                    [ (player1, p1Rating, p1Rd, Glicko.Outcome.invert outcome) ]
                    |> Some
                | Some x ->
                    (player1, p1Rating, p1Rd, Glicko.Outcome.invert outcome)
                    :: x
                    |> Some)
            |> Map.toList
            |> List.sortByDescending (fun (x, _) -> x)

        Map.add player1 p1RecordsNew tournament
        |> Map.add player2 p2RecordsNew

    let expected =
        [ "Alice",
          [ LocalDate(2021, 9, 29), [ "Bob", Glicko.Rating 1338, Glicko.RD 292, Glicko.Outcome.Won ]
            LocalDate(2021, 9, 28), [ "Bob", Glicko.Rating 1500, Glicko.RD 350, Glicko.Outcome.Won ] ]
          "Bob",
          [ LocalDate(2021, 9, 29), [ "Alice", Glicko.Rating 1662, Glicko.RD 292, Glicko.Outcome.Lost ]
            LocalDate(2021, 9, 28), [ "Alice", Glicko.Rating 1500, Glicko.RD 350, Glicko.Outcome.Lost ] ] ]
        |> Map.ofList

    add "Alice" "Bob" (LocalDate(2021, 9, 28)) Glicko.Outcome.Won tournament
    |> add "Alice" "Bob" (LocalDate(2021, 9, 29)) Glicko.Outcome.Won
    |> should equal expected
