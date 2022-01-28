namespace Glicko

module Glicko =
    module Infrastructure =
        open System

        let (><) f x y = f y x // flip operator

        let roundOff (x: float) = Math.Round(x, MidpointRounding.AwayFromZero)

        let roundOffToInt = roundOff >> int
        let pow (x: float) (y: float) = Math.Pow(x, y)
        let square = pow >< 2.

    module Domain =
        open System
        open Infrastructure

        type Outcome =
            | Won
            | Draw
            | Lost

        module Outcome =
            let invert =
                function
                | Won -> Lost
                | Draw -> Draw
                | Lost -> Won

        let q = Math.Log(10.) / 400.

        type RD = RD of int

        module RD =
            let c (RD newPlayerRd) (RD typicalRd) daysToReturnToInitialRd =
                (square (float newPlayerRd) - square (float typicalRd)) / (float daysToReturnToInitialRd) |> sqrt

            let updateRd c (RD newPlayerRd) days (RD currentRd) =
                [ sqrt (square (float currentRd) + square c * (float days))
                  (float newPlayerRd) ]
                |> List.min
                |> roundOffToInt
                |> RD

            let g (RD rd) =
                let piSquared = square Math.PI
                let rdSquared = square (float rd)
                let qSquared = square q

                1. / sqrt (1. + (3. * qSquared * rdSquared / piSquared))

        type Rating = Rating of int

        type Player =
            { rating: Rating
              rd: RD }

        module Player =
            let e { rating = (Rating rj); rd = rdj } { rating = (Rating r); rd = _ } =
                let ratingDifference = (float r) - (float rj)

                1. / (1. + pow 10. ((0. - (RD.g rdj)) * ratingDifference / 400.))

            let dSquared games player =
                let qSquared = square q

                let sigma player =
                    let sigma' player (_, pj, _) =
                        let { rating = _; rd = rdj } = pj

                        let gRdj = RD.g rdj |> square
                        let e' = player |> e pj
                        gRdj * e' * (1. - e')

                    List.map (fun opponent -> sigma' player opponent) >> List.sum

                pow (qSquared * (sigma player games)) -1.0

        type GameRecord =
            { OpponentName: string
              OpponentRating: Rating
              OpponentRd: RD
              Outcome: Outcome }

    open NodaTime

    open Infrastructure
    open Domain

    let newR r rd games =
        let (Rating r') = r
        let (RD rd') = rd

        let p =
            { rating = r
              rd = rd }

        let rdSquared = float rd' |> square

        let sigma p =
            let sigma' p gameRecord =
                let sj' =
                    match gameRecord.Outcome with
                    | Won -> 1.
                    | Draw -> 0.5
                    | Lost -> 0.

                let pj =
                    { rating = gameRecord.OpponentRating
                      rd = gameRecord.OpponentRd }

                let e' = p |> Player.e pj

                RD.g gameRecord.OpponentRd * (sj' - e')

            List.map (fun opponent -> sigma' p opponent) >> List.sum

        let games' =
            games
            |> List.map (fun gameRecord ->
                (gameRecord.OpponentName,
                 { rating = gameRecord.OpponentRating
                   rd = gameRecord.OpponentRd }, gameRecord.Outcome))

        (float r') + (q / ((1. / rdSquared) + (1. / (Player.dSquared games' p)))) * (sigma p games)
        |> roundOffToInt
        |> Rating

    let newRd r rd games =
        let (RD rd') = rd

        let p =
            { rating = r
              rd = rd }

        let rdSquared = float rd' |> square

        let games' =
            games
            |> List.map (fun gameRecord ->
                (gameRecord.OpponentName,
                 { rating = gameRecord.OpponentRating
                   rd = gameRecord.OpponentRd }, gameRecord.Outcome))

        let dSquared' = Player.dSquared games' p

        pow ((1. / rdSquared) + (1. / dSquared')) -1.0
        |> sqrt
        |> roundOffToInt
        |> RD

    let expectedOutcome (Rating r1) (RD rd1) (Rating r2) (RD rd2) =
        let g' =
            let rd1Squared = float rd1 |> square
            let rd2Squared = float rd2 |> square

            rd1Squared + rd2Squared
            |> sqrt
            |> roundOffToInt
            |> RD
            |> RD.g

        1. / (1. + (pow 10. -(g' * (r1 - r2 |> float) / 400.)))

    let interval r rd =
        let (Rating r') = r
        let (RD rd') = rd
        let halfInterval = 1.96 * (float rd')

        let lower =
            (float r') - halfInterval
            |> roundOffToInt
            |> Rating

        let upper =
            (float r') + halfInterval
            |> roundOffToInt
            |> Rating

        (lower, upper)

    let calcPlayerState updateRd newPlayerRating newPlayerRd (date: LocalDate) records =
        let rec calcPlayerState' r0 rd0 lastPlayed records =
            match records with
            | (datePlayed, games) :: tail ->
                calcPlayerState' (newR r0 rd0 games) (newRd r0 rd0 games) datePlayed tail
            | [] -> (r0, rd0, lastPlayed)

        List.skipWhile (fun (d, _) -> d >= date) records
        |> List.rev
        |> calcPlayerState' newPlayerRating newPlayerRd date
        |> fun (rating, rd, lastPlayed) ->
            let timeSinceLastPlayed = date - lastPlayed

            let degradedRd = updateRd timeSinceLastPlayed.Days rd

            rating, degradedRd
