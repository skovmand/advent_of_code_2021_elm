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
        |> List.foldl
            (\( x, y, point ) dictAcc -> Dict.insert ( x, y ) point dictAcc)
            Dict.empty


solvePart1 : Dict ( Int, Int ) Int -> Maybe Int
solvePart1 dict =
    let
        lowPoints =
            findLowPoints dict
    in
    lowPoints
        |> List.map (\point -> Dict.get point dict)
        |> maybeAll
        |> Maybe.map (List.map ((+) 1))
        |> Maybe.map List.sum


findLowPoints : Dict ( Int, Int ) Int -> List ( Int, Int )
findLowPoints dict =
    Dict.foldl
        (\coord value lowPoints ->
            let
                minAdjacentHeight =
                    adjacentHeights coord dict
                        |> List.minimum
                        -- For convenience, should never happen
                        |> Maybe.withDefault -1
            in
            if value < minAdjacentHeight then
                coord :: lowPoints

            else
                lowPoints
        )
        []
        dict


adjacentHeights : ( Int, Int ) -> Dict ( Int, Int ) Int -> List Int
adjacentHeights ( x, y ) dict =
    [ ( x + 1, y ), ( x - 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
        |> List.filterMap (\coord -> Dict.get coord dict)



{-
   Ideas for part 2: Find all low points. Add to list. Recurse over each one to grow it and count the size.
   Note: I am taking the assumption that all low points are unique (e.g. there is only one).

   From the task: A basin is all locations that eventually flow downward to a single low point.

   Idea: Rewrite task 1 into making a list of low points.
         Task 2: Make an algorithm to grow basins.
-}
