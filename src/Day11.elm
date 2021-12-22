module Day11 exposing (parseInput, solvePart1)

{-| Day 11: Dumbo Octopus
-}

import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)
import Utilities exposing (maybeAll)



---------------
-- PARSE
---------------


parseInput : String -> Maybe (Dict ( Int, Int ) Int)
parseInput input =
    input
        |> String.trim
        |> String.lines
        |> List.map toIntegers
        |> maybeAll
        |> Maybe.map fillDict


toIntegers : String -> Maybe (List Int)
toIntegers line =
    line
        |> String.split ""
        |> List.map String.toInt
        |> maybeAll


fillDict : List (List Int) -> Dict ( Int, Int ) Int
fillDict intLines =
    List.indexedMap Tuple.pair intLines
        |> List.concatMap
            (\( y, line ) ->
                List.indexedMap (\x pointHeight -> ( x, y, pointHeight )) line
            )
        |> List.foldl
            (\( x, y, pointHeight ) dictAcc -> Dict.insert ( x, y ) pointHeight dictAcc)
            Dict.empty



-----------------------------
-- PART 1
-----------------------------


solvePart1 : Dict ( Int, Int ) Int -> Int
solvePart1 energyLevels =
    solveRounds 100 { energyLevels = energyLevels, flashCount = 0 }


solveRounds : Int -> { energyLevels : Dict ( Int, Int ) Int, flashCount : Int } -> Int
solveRounds rounds { energyLevels, flashCount } =
    if rounds < 1 then
        flashCount

    else
        energyLevels
            |> addOne
            |> flash Set.empty
            |> addFlashesToTotalCount flashCount
            |> resetFlashed
            |> solveRounds (rounds - 1)


addOne : Dict ( Int, Int ) Int -> Dict ( Int, Int ) Int
addOne energyLevels =
    Dict.map (\_ val -> val + 1) energyLevels


addFlashesToTotalCount : Int -> ( Dict ( Int, Int ) Int, Set ( Int, Int ) ) -> { energyLevels : Dict ( Int, Int ) Int, flashCount : Int }
addFlashesToTotalCount totalFlashCount ( energyLevels, flashedThisRound ) =
    { energyLevels = energyLevels, flashCount = totalFlashCount + Set.size flashedThisRound }


resetFlashed : { energyLevels : Dict ( Int, Int ) Int, flashCount : Int } -> { energyLevels : Dict ( Int, Int ) Int, flashCount : Int }
resetFlashed { energyLevels, flashCount } =
    let
        resetEnergyLevels =
            Dict.map
                (\_ value ->
                    if value > 9 then
                        0

                    else
                        value
                )
                energyLevels
    in
    { energyLevels = resetEnergyLevels, flashCount = flashCount }


{-| Apply all flashes for this round recursively using tail call optimization
-}
flash : Set ( Int, Int ) -> Dict ( Int, Int ) Int -> ( Dict ( Int, Int ) Int, Set ( Int, Int ) )
flash alreadyFlashed energyLevels =
    case findNextFlashing energyLevels alreadyFlashed of
        Just coord ->
            flash (Set.insert coord alreadyFlashed) (flashOctopus coord energyLevels)

        Nothing ->
            ( energyLevels, alreadyFlashed )


{-| Find the next flashing octopus which has not already flashed
-}
findNextFlashing : Dict ( Int, Int ) Int -> Set ( Int, Int ) -> Maybe ( Int, Int )
findNextFlashing energyLevels alreadyFlashed =
    energyLevels
        |> Dict.toList
        |> List.Extra.find
            (\( key, value ) -> value > 9 && not (Set.member key alreadyFlashed))
        |> Maybe.map (\( coord, _ ) -> coord)


{-| Flash an octopus, return the updated energy levels after this single flash
-}
flashOctopus : ( Int, Int ) -> Dict ( Int, Int ) Int -> Dict ( Int, Int ) Int
flashOctopus coord energyLevels =
    coord
        |> toAdjacentCoords energyLevels
        |> List.foldl
            (\adjacentCoord energyLevelsAcc ->
                Dict.update
                    adjacentCoord
                    (Maybe.map ((+) 1))
                    energyLevelsAcc
            )
            energyLevels


{-| Get adjacent coords from a coordinate, filter out non-existing
-}
toAdjacentCoords : Dict ( Int, Int ) Int -> ( Int, Int ) -> List ( Int, Int )
toAdjacentCoords energyLevels ( x, y ) =
    [ ( x + 1, y ), ( x + 1, y - 1 ), ( x, y - 1 ), ( x - 1, y - 1 ), ( x - 1, y ), ( x - 1, y + 1 ), ( x, y + 1 ), ( x + 1, y + 1 ) ]
        |> List.filter (\coord -> Dict.member coord energyLevels)
