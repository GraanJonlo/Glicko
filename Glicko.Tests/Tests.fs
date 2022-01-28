module Tests

open NodaTime
open Xunit
open FsUnit.Xunit

open Glicko
open Glicko.Domain

[<Fact>]
let ``Calculate c``() =
    let newPlayerRd = RD 350
    let typicalRd = RD 50
    let timeToBaseUncertainty = 100

    RD.c newPlayerRd typicalRd timeToBaseUncertainty |> should (equalWithin 0.01) 34.64

[<Fact>]
let ``Determine new RD from time passing``() =
    let newPlayerRd = RD 350
    let typicalRd = RD 50
    let timeToBaseUncertainty = 100
    let elapsedDays = 10
    let currentRd = RD 200

    let c = RD.c newPlayerRd typicalRd timeToBaseUncertainty

    RD.updateRd c newPlayerRd elapsedDays currentRd |> should equal (RD 228)

[<Fact>]
let ``Calculate q``() = q |> should (equalWithin 0.000001) 0.00575646273

[<Theory>]
[<InlineData(30., 0.9955)>]
[<InlineData(100., 0.9531)>]
[<InlineData(300., 0.7242)>]
let ``Calculate g`` (rd, expected) = RD.g (RD rd) |> should (equalWithin 0.0001) expected

[<Theory>]
[<InlineData(1500, 1400, 30, 0.639)>]
[<InlineData(1500, 1550, 100, 0.432)>]
[<InlineData(1500, 1700, 300, 0.303)>]
[<InlineData(1500, 1500, 45, 0.5)>]
let ``Calculate E`` (r, rj, rdj, expected) =
    { rating = (Rating r)
      rd = (RD 0) }
    |> Player.e
        { rating = (Rating rj)
          rd = (RD rdj) }
    |> should (equalWithin 0.001) expected

[<Fact>]
let ``Calculate dSquared``() =
    let games =
        [ "",
          { rating = Rating 1400
            rd = RD 30 }, Outcome.Won
          "",
          { rating = Rating 1550
            rd = RD 100 }, Outcome.Lost
          "",
          { rating = Rating 1700
            rd = RD 300 }, Outcome.Lost ]

    { rating = Rating 1500
      rd = RD 0 }
    |> Player.dSquared games
    |> sqrt
    |> should (equalWithin 0.1) 231.67

[<Fact>]
let ``Calculate post period r``() =
    let r = Rating 1500
    let rd = RD 200

    let games =
        [ { OpponentName = ""
            OpponentRating = Rating 1400
            OpponentRd = RD 30
            Outcome = Outcome.Won }
          { OpponentName = ""
            OpponentRating = Rating 1550
            OpponentRd = RD 100
            Outcome = Outcome.Lost }
          { OpponentName = ""
            OpponentRating = Rating 1700
            OpponentRd = RD 300
            Outcome = Outcome.Lost } ]

    Glicko.newR r rd games |> should equal (Rating 1464)

[<Fact>]
let ``Calculate post period rd``() =
    let r = Rating 1500
    let rd = RD 200

    let games =
        [ { OpponentName = ""
            OpponentRating = Rating 1400
            OpponentRd = RD 30
            Outcome = Outcome.Won }
          { OpponentName = ""
            OpponentRating = Rating 1550
            OpponentRd = RD 100
            Outcome = Outcome.Lost }
          { OpponentName = ""
            OpponentRating = Rating 1700
            OpponentRd = RD 300
            Outcome = Outcome.Lost } ]

    Glicko.newRd r rd games |> should equal (RD 151)

[<Fact>]
let ``Predict outcome``() =
    let r1 = Rating 1400
    let rd1 = RD 80
    let r2 = Rating 1500
    let rd2 = RD 150

    Glicko.expectedOutcome r1 rd1 r2 rd2 |> should (equalWithin 0.001) 0.376

[<Fact>]
let ``Calculate rating interval``() =
    let r = Rating 1500
    let rd = RD 30

    Glicko.interval r rd |> should equal (Rating 1441, Rating 1559)

[<Fact>]
let ``Calculate player state at date``() =
    let newPlayerRating = Rating 1500
    let newPlayerRd = RD 350
    let typicalRd = RD 50
    let timeToBaseUncertainty = 100

    let c = RD.c newPlayerRd typicalRd timeToBaseUncertainty

    let updateRd = RD.updateRd c newPlayerRd

    let records =
        [ LocalDate(2021, 9, 29),
          [ { OpponentName = "Alice"
              OpponentRating = Rating 1375
              OpponentRd = RD 30
              Outcome = Outcome.Draw } ]
          LocalDate(2021, 9, 28),
          [ { OpponentName = "Alice"
              OpponentRating = Rating 1400
              OpponentRd = RD 30
              Outcome = Outcome.Won }
            { OpponentName = "Eve"
              OpponentRating = Rating 1550
              OpponentRd = RD 100
              Outcome = Outcome.Lost }
            { OpponentName = "Tony"
              OpponentRating = Rating 1700
              OpponentRd = RD 300
              Outcome = Outcome.Lost } ] ]

    let calculate = Glicko.calcPlayerState updateRd newPlayerRating newPlayerRd

    calculate (LocalDate(2021, 9, 28)) records |> should equal (Rating 1500, RD 350)

    calculate (LocalDate(2021, 9, 29)) records |> should equal (Rating 1442, RD 196)

    calculate (LocalDate(2021, 9, 30)) records |> should equal (Rating 1426, RD 173)

[<Fact>]
let Foo() =
    let tournament = Map.empty

    let add player1 player2 (date: LocalDate) outcome tournament =
        let newPlayerRd = RD 350
        let typicalRd = RD 50
        let timeToBaseUncertainty = 100

        let c = RD.c newPlayerRd typicalRd timeToBaseUncertainty

        let updateRd = RD.updateRd c newPlayerRd

        let p1Records =
            match Map.tryFind player1 tournament with
            | Some x -> x
            | None -> List.empty

        let p2Records =
            match Map.tryFind player2 tournament with
            | Some x -> x
            | None -> List.empty

        let p1Rating, p1Rd = Glicko.calcPlayerState updateRd (Rating 1500) (RD 350) date p1Records

        let p2Rating, p2Rd = Glicko.calcPlayerState updateRd (Rating 1500) (RD 350) date p2Records

        let p1RecordsNew =
            p1Records
            |> Map.ofList
            |> Map.change date (function
                   | None ->
                       [ { OpponentName = player2
                           OpponentRating = p2Rating
                           OpponentRd = p2Rd
                           Outcome = outcome } ]
                       |> Some
                   | Some x ->
                       { OpponentName = player2
                         OpponentRating = p2Rating
                         OpponentRd = p2Rd
                         Outcome = outcome } :: x |> Some)
            |> Map.toList
            |> List.sortByDescending (fun (d, _) -> d)

        let p2RecordsNew =
            p2Records
            |> Map.ofList
            |> Map.change date (function
                   | None ->
                       [ { OpponentName = player1
                           OpponentRating = p1Rating
                           OpponentRd = p1Rd
                           Outcome = Outcome.invert outcome } ]
                       |> Some
                   | Some x ->
                       { OpponentName = player1
                         OpponentRating = p1Rating
                         OpponentRd = p1Rd
                         Outcome = Outcome.invert outcome } :: x |> Some)
            |> Map.toList
            |> List.sortByDescending (fun (d, _) -> d)

        Map.add player1 p1RecordsNew tournament |> Map.add player2 p2RecordsNew

    let expected =
        [ "Alice",
          [ LocalDate(2021, 9, 29),
            [ { OpponentName = "Bob"
                OpponentRating = Rating 1338
                OpponentRd = RD 292
                Outcome = Outcome.Won } ]
            LocalDate(2021, 9, 28),
            [ { OpponentName = "Bob"
                OpponentRating = Rating 1500
                OpponentRd = RD 350
                Outcome = Outcome.Won } ] ]
          "Bob",
          [ LocalDate(2021, 9, 29),
            [ { OpponentName = "Alice"
                OpponentRating = Rating 1662
                OpponentRd = RD 292
                Outcome = Outcome.Lost } ]
            LocalDate(2021, 9, 28),
            [ { OpponentName = "Alice"
                OpponentRating = Rating 1500
                OpponentRd = RD 350
                Outcome = Outcome.Lost } ] ] ]
        |> Map.ofList

    add "Alice" "Bob" (LocalDate(2021, 9, 28)) Outcome.Won tournament
    |> add "Alice" "Bob" (LocalDate(2021, 9, 29)) Outcome.Won
    |> should equal expected
