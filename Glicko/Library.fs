namespace Glicko

module Glicko =
    open System

    let (><) f x y = f y x // flip operator

    let roundOff (x: float) =
        Math.Round(x, MidpointRounding.AwayFromZero)

    let roundOffToInt = roundOff >> int
    let pow (x: float) (y: float) = Math.Pow(x, y)
    let square = pow >< 2.

    type RD = RD of int
    type RatingPeriod = RatingPeriod of int
    type Rating = Rating of int

    type Outcome =
        | Won
        | Draw
        | Lost

    let c (RD initial) (RD typical) (RatingPeriod ratingPeriodsToReturnToInitial) =
        (square (float initial) - square (float typical))
        / (float ratingPeriodsToReturnToInitial)
        |> sqrt

    let updateRD (RD newPlayerRD) typicalRD timeToBaseUncertainty (RatingPeriod t) (RD rd0) =
        let c =
            c (RD newPlayerRD) typicalRD timeToBaseUncertainty

        [ sqrt (square (float rd0) + square c * (float t))
          (float newPlayerRD) ]
        |> List.min
        |> roundOffToInt
        |> RD

    let q = Math.Log(10.) / 400.

    let g (RD rd) =
        let piSquared = square Math.PI
        let rdSquared = square (float rd)
        let qSquared = square q

        1.
        / sqrt (1. + (3. * qSquared * rdSquared / piSquared))

    let e (Rating r) (Rating rj) rdj =
        let ratingDifference = (float r) - (float rj)

        1.
        / (1.
           + pow 10. ((0. - (g rdj)) * ratingDifference / 400.))

    let dSquared r games =
        let qSquared = square q

        let sigma r =
            let sigma' r (rj, rdj, _) =
                let gRdj = g rdj |> square
                let e' = e r rj rdj
                gRdj * e' * (1. - e')

            List.map (fun opponent -> sigma' r opponent)
            >> List.sum

        pow (qSquared * (sigma r games)) -1.

    let newR r (RD rd) games =
        let (Rating r') = r
        let rdSquared = float rd |> square

        let sigma r =
            let sigma' r (rj, rdj, sj) =
                let sj' =
                    match sj with
                    | Won -> 1.
                    | Draw -> 0.5
                    | Lost -> 0.

                g rdj * (sj' - (e r rj rdj))

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
            |> g

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
