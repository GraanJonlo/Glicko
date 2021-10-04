namespace Glicko

module Glicko =
    module Infrastructure =
        open System

        let (><) f x y = f y x // flip operator

        let roundOff (x: float) =
            Math.Round(x, MidpointRounding.AwayFromZero)

        let roundOffToInt = roundOff >> int
        let pow (x: float) (y: float) = Math.Pow(x, y)
        let square = pow >< 2.

    module Domain =
        open System
        open Infrastructure

        let q = Math.Log(10.) / 400.

        type RD = RD of int

        module RD =
            let c (RD newPlayerRd) (RD typicalRd) daysToReturnToInitialRd =
                (square (float newPlayerRd)
                 - square (float typicalRd))
                / (float daysToReturnToInitialRd)
                |> sqrt

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

                1.
                / sqrt (1. + (3. * qSquared * rdSquared / piSquared))

    open System

    open NodaTime

    open Infrastructure
    open Domain

    type Rating = Rating of int

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

    let e (Rating r) (Rating rj) rdj =
        let ratingDifference = (float r) - (float rj)

        1.
        / (1.
           + pow 10. ((0. - (RD.g rdj)) * ratingDifference / 400.))

    let dSquared r games =
        let qSquared = square q

        let sigma r =
            let sigma' r (_, rj, rdj, _) =
                let gRdj = RD.g rdj |> square
                let e' = e r rj rdj
                gRdj * e' * (1. - e')

            List.map (fun opponent -> sigma' r opponent)
            >> List.sum

        pow (qSquared * (sigma r games)) -1.

    let newR r (RD rd) games =
        let (Rating r') = r
        let rdSquared = float rd |> square

        let sigma r =
            let sigma' r (_, rj, rdj, sj) =
                let sj' =
                    match sj with
                    | Won -> 1.
                    | Draw -> 0.5
                    | Lost -> 0.

                RD.g rdj * (sj' - (e r rj rdj))

            List.map (fun opponent -> sigma' r opponent)
            >> List.sum

        (float r')
        + (q / ((1. / rdSquared) + (1. / (dSquared r games))))
          * (sigma r games)
        |> roundOffToInt
        |> Rating

    let newRd r (RD rd) games =
        let rdSquared = float rd |> square
        let dSquared' = dSquared r games

        pow ((1. / rdSquared) + (1. / dSquared')) -1.
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

        1.
        / (1. + (pow 10. -(g' * (r1 - r2 |> float) / 400.)))

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
            | (datePlayed, games) :: tail -> calcPlayerState' (newR r0 rd0 games) (newRd r0 rd0 games) datePlayed tail
            | [] -> (r0, rd0, lastPlayed)

        List.skipWhile (fun (d, _) -> d >= date) records
        |> List.rev
        |> calcPlayerState' newPlayerRating newPlayerRd date
        |> fun (rating, rd, lastPlayed) ->
            let timeSinceLastPlayed = date - lastPlayed

            let degradedRd = updateRd (timeSinceLastPlayed.Days) rd

            rating, degradedRd
