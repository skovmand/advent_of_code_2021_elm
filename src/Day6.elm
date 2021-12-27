module Day6 exposing (parseInput, solvePart1, solvePart2)

{-| Day 6: Lanternfish
<https://adventofcode.com/2021/day/6>
-}

import Dict exposing (Dict)
import Utilities exposing (maybeAll)


parseInput : String -> Maybe (Dict Int Int)
parseInput input =
    input
        |> String.split ","
        |> List.map String.toInt
        |> maybeAll
        |> Maybe.map toFishDict


toFishDict : List Int -> Dict Int Int
toFishDict fishTimers =
    List.foldl
        (\timer dict ->
            Dict.update
                timer
                (\value ->
                    case value of
                        Nothing ->
                            Just 1

                        Just count ->
                            Just (count + 1)
                )
                dict
        )
        Dict.empty
        fishTimers


solvePart1 : Dict Int Int -> Int
solvePart1 fishTimers =
    evolveDays 80 fishTimers
        |> Dict.values
        |> List.sum


solvePart2 : Dict Int Int -> Int
solvePart2 fishTimers =
    evolveDays 256 fishTimers
        |> Dict.values
        |> List.sum


evolveDays : Int -> Dict Int Int -> Dict Int Int
evolveDays days fishTimers =
    let
        rounds =
            List.range 1 days
    in
    List.foldl
        (\_ timerDict -> evolveStep timerDict)
        fishTimers
        rounds


evolveStep : Dict Int Int -> Dict Int Int
evolveStep fishTimers =
    Dict.foldr
        (\timer count newDict ->
            case timer of
                0 ->
                    newDict
                        |> Dict.insert 8 count
                        |> Dict.update 6
                            (\value ->
                                case value of
                                    Nothing ->
                                        Just count

                                    Just c ->
                                        Just (c + count)
                            )

                n ->
                    Dict.insert (n - 1) count newDict
        )
        Dict.empty
        fishTimers
