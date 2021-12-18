module Day6 exposing (..)

import Utilities exposing (maybeAll)


parseInput : String -> Maybe (List Int)
parseInput input =
    input
        |> String.split ","
        |> List.map String.toInt
        |> maybeAll


solvePart1 : List Int -> Int
solvePart1 fishTimers =
    evolveDays 80 fishTimers
        |> List.length


solvePart2 : List Int -> Int
solvePart2 fishTimers =
    evolveDays 256 fishTimers
        |> List.length


evolveDays : Int -> List Int -> List Int
evolveDays days fishTimers =
    let
        rounds =
            List.range 1 days
    in
    List.foldl
        (\_ timers -> evolveStep timers)
        fishTimers
        rounds


evolveStep : List Int -> List Int
evolveStep fishTimers =
    List.foldl
        (\fish ( evolvedFishes, newFishes ) ->
            if fish == 0 then
                ( 6 :: evolvedFishes, 8 :: newFishes )

            else
                ( (fish - 1) :: evolvedFishes, newFishes )
        )
        ( [], [] )
        fishTimers
        |> (\( evolved, new ) -> evolved ++ new)
