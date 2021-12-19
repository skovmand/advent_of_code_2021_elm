module Day9 exposing (parseInput, solvePart1)

import Dict exposing (Dict)
import Utilities exposing (maybeAll)


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
                List.indexedMap (\x point -> ( x, y, point )) line
            )
        |> List.foldl (\( x, y, point ) dictAcc -> Dict.insert ( x, y ) point dictAcc) Dict.empty


solvePart1 : Dict ( Int, Int ) Int -> Int
solvePart1 dict =
    Dict.foldl
        (\coord value riskLevelSum ->
            let
                minAdjacentHeight =
                    adjacentHeights coord dict
                        |> List.minimum
                        -- For convenience, should never happen
                        |> Maybe.withDefault -1

                riskLevel =
                    if value < minAdjacentHeight then
                        1 + value

                    else
                        0
            in
            riskLevelSum + riskLevel
        )
        0
        dict


adjacentHeights : ( Int, Int ) -> Dict ( Int, Int ) Int -> List Int
adjacentHeights ( x, y ) dict =
    [ ( x + 1 , y ), ( x - 1, y ), ( x, y - 1 ), ( x , y + 1 ) ]
        |> List.filterMap (\coord -> Dict.get coord dict)
