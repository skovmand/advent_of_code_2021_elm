module Day9 exposing (parseInput, solvePart1, solvePart2)

{-| Day 9: Smoke Basin.
<https://adventofcode.com/2021/day/9>

Featuring a nice recursive homegrown pool crawling algorithm.

-}

import Dict exposing (Dict)
import Set exposing (Set)
import Utilities exposing (maybeAll, unwrapMaybe)



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
-- NOW FIND SOME SMOKE HOLES!
-----------------------------


solvePart1 : Dict ( Int, Int ) Int -> Maybe Int
solvePart1 dict =
    findLowPoints dict
        |> List.map (\coord -> Dict.get coord dict)
        |> maybeAll
        |> Maybe.map (List.map ((+) 1))
        |> Maybe.map List.sum


solvePart2 : Dict ( Int, Int ) Int -> Maybe Int
solvePart2 dict =
    findLowPoints dict
        |> List.map (expandPool dict)
        |> List.map Set.size
        |> List.sort
        |> List.reverse
        |> (\list ->
                case list of
                    a :: b :: c :: _ ->
                        Just (a * b * c)

                    _ ->
                        Nothing
           )


{-| Find the low points of the 2D grid. Note that there is a single low point for each pool.
Returns a list of coordinates.
-}
findLowPoints : Dict ( Int, Int ) Int -> List ( Int, Int )
findLowPoints dict =
    Dict.foldl
        (\coord value lowPoints ->
            let
                minAdjacentHeight =
                    adjacentHeights coord dict
                        |> List.minimum
                        |> unwrapMaybe
            in
            if value < minAdjacentHeight then
                coord :: lowPoints

            else
                lowPoints
        )
        []
        dict


{-| Given a coordinate, get adjacent heights. Filters out non-existing fields
(e.g. when on the edge of the map)
-}
adjacentHeights : ( Int, Int ) -> Dict ( Int, Int ) Int -> List Int
adjacentHeights ( x, y ) allPoints =
    [ ( x + 1, y ), ( x - 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
        |> List.filterMap (\coord -> Dict.get coord allPoints)


{-| Recursive pool expansion algorithm
Given all points and the coordinate of a point in a pool, crawl the pool and return a set of all its coordinates
-}
expandPool : Dict ( Int, Int ) Int -> ( Int, Int ) -> Set ( Int, Int )
expandPool allPoints startCoord =
    expandStep allPoints (Set.fromList [ startCoord ]) startCoord


type Direction
    = Up
    | Down
    | Left
    | Right


expandStep : Dict ( Int, Int ) Int -> Set ( Int, Int ) -> ( Int, Int ) -> Set ( Int, Int )
expandStep allPoints poolCoords currentCoord =
    -- Every coordinate called with expandStep will attempt to go all 4 directions
    [ Up, Left, Down, Right ]
        |> List.foldl
            (\direction poolCoordsAcc ->
                let
                    -- Calculate the next coord from the direction and the current coord
                    -- Returns Nothing if height of point is 9
                    -- Returns Nothing for coords already visited
                    nextPoolCoord =
                        nextCoord allPoints direction currentCoord
                            |> nothingIf9 allPoints
                            |> nothingIfAlreadyInSet poolCoordsAcc
                in
                case nextPoolCoord of
                    -- A next coordinate exists, recurse into it
                    Just coord ->
                        expandStep allPoints (Set.insert coord poolCoordsAcc) coord

                    -- We have met a dead end, backtrack, returning the new set of points
                    Nothing ->
                        poolCoordsAcc
            )
            poolCoords


{-| If a coordinate has height 9, treat is as Nothing
-}
nothingIf9 : Dict ( Int, Int ) Int -> Maybe ( Int, Int ) -> Maybe ( Int, Int )
nothingIf9 allPoints coordinate =
    coordinate
        |> Maybe.andThen (\coord -> Dict.get coord allPoints)
        |> Maybe.andThen
            (\pointHeight ->
                case pointHeight of
                    9 ->
                        Nothing

                    _ ->
                        coordinate
            )


nothingIfAlreadyInSet : Set ( Int, Int ) -> Maybe ( Int, Int ) -> Maybe ( Int, Int )
nothingIfAlreadyInSet setOfCoords coordinate =
    coordinate
        |> Maybe.andThen
            (\coord ->
                if Set.member coord setOfCoords then
                    Nothing

                else
                    coordinate
            )


{-| Given a coord and a direction, return the next coordinate or Nothing.
-}
nextCoord : Dict ( Int, Int ) Int -> Direction -> ( Int, Int ) -> Maybe ( Int, Int )
nextCoord allPoints direction coord =
    let
        next =
            calcNextCoord direction coord
    in
    if Dict.member next allPoints then
        Just next

    else
        Nothing


calcNextCoord : Direction -> ( Int, Int ) -> ( Int, Int )
calcNextCoord direction coord =
    case direction of
        Up ->
            Tuple.mapSecond (\y -> y - 1) coord

        Down ->
            Tuple.mapSecond (\y -> y + 1) coord

        Left ->
            Tuple.mapFirst (\x -> x - 1) coord

        Right ->
            Tuple.mapFirst (\x -> x + 1) coord
