module Day6 exposing (..)

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
    let
        newFishCount =
            fishTimers
                |> Dict.get 0
                |> Maybe.withDefault 0
    in
    fishTimers
        |> Dict.toList
        |> List.map (\( timer, count ) -> ( timer - 1, count ))
        |> List.filter (\( timer, _ ) -> timer >= 0)
        |> (\list -> ( 8, newFishCount ) :: list)
        |> Dict.fromList
        -- We need to update the 6s to add the sum of old fish coming from 0
        |> Dict.update 6 (addNewFishCount newFishCount)


addNewFishCount : Int -> Maybe Int -> Maybe Int
addNewFishCount extra existingCount =
    case existingCount of
        Nothing ->
            Just extra

        Just count ->
            Just (count + extra)
