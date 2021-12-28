module Day15 exposing (parseInput, solvePart1, solvePart2)

{-| Day 15: Chiton
<https://adventofcode.com/2021/day/15>

Solution finding the shortest path using Dijkstras shortest path algorithm.
Used resources:

  - <https://www.youtube.com/watch?v=GazC3A4OQTE>

-}

import Dict exposing (Dict)
import PrioritySet exposing (PrioritySet)
import SantasList
import Utilities exposing (maybeAll, unwrapMaybe, unwrapMaybeWithMessage)



---------------
-- PARSE
---------------


type alias Cave =
    Dict ( Int, Int ) Int


parseInput : String -> Cave
parseInput input =
    input
        |> String.trim
        |> String.lines
        |> List.map toIntegers
        |> maybeAll
        |> unwrapMaybeWithMessage "PARSE ERROR"
        |> fillDict


toIntegers : String -> Maybe (List Int)
toIntegers line =
    line
        |> String.split ""
        |> List.map String.toInt
        |> maybeAll


fillDict : List (List Int) -> Cave
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
-- PART 1 + 2
-----------------------------


solvePart1 : Cave -> Int
solvePart1 cave =
    lowestRiskPath adjacentCoordsAndRisks (findEndCoord cave) cave initialPriorityQueue


solvePart2 : Cave -> Int
solvePart2 cave =
    let
        endCoord =
            findEndCoord cave
                |> endCoordTimes5

        ( boardSizeX, boardSizeY ) =
            Tuple.mapBoth ((+) 1) ((+) 1) (findEndCoord cave)
    in
    lowestRiskPath (virtualAdjacentCoordsAndRisks ( boardSizeX, boardSizeY )) endCoord cave initialPriorityQueue


{-| Multiply end coordinate by 5, maintaining zero-based index
-}
endCoordTimes5 : ( Int, Int ) -> ( Int, Int )
endCoordTimes5 endCoord =
    let
        timesFive coord =
            (coord + 1) * 5 - 1
    in
    Tuple.mapBoth timesFive timesFive endCoord


findEndCoord : Cave -> ( Int, Int )
findEndCoord cave =
    cave
        |> Dict.toList
        |> SantasList.last
        |> unwrapMaybe
        |> Tuple.first


{-| Signature for function to get adjacent coords. This varies btw part 1 and 2 due to the virtual boards
-}
type alias GetAdjacentCoordsFn =
    ( Int, Int ) -> Cave -> List ( ( Int, Int ), Int )


{-| A simplified implementation of Dijkstras Shortest Path Algorithm, <https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm>
Finds the path with the lowest risk, at least in this AOC task.

Uses a PrioritySet (my own alternate implementation), which avoids duplicate entries.

-}
lowestRiskPath : GetAdjacentCoordsFn -> ( Int, Int ) -> Cave -> PrioritySet ( Int, Int ) Int -> Int
lowestRiskPath getAdjacentFn endCoord cave prioritySet =
    let
        ( coord, totalRisk ) =
            PrioritySet.head prioritySet |> unwrapMaybe

        updatedPriorityQueue =
            getAdjacentFn coord cave
                -- add adjacent risks to the PrioritySet to have the accumulated distance in it
                -- the next iteration will pull out the shortest accumulative distance
                |> List.foldl
                    (\( adjCoord, risk ) prioritySetAcc -> PrioritySet.insert adjCoord (totalRisk + risk) prioritySetAcc)
                    (PrioritySet.tail prioritySet)
    in
    if coord == endCoord then
        totalRisk

    else
        lowestRiskPath getAdjacentFn endCoord cave updatedPriorityQueue


initialPriorityQueue : PrioritySet ( Int, Int ) Int
initialPriorityQueue =
    PrioritySet.empty
        |> PrioritySet.insert ( 0, 0 ) 0


{-| Given a coordinate, get adjacent coords. Filters out non-existing fields
(e.g. when on the edge of the map)
-}
adjacentCoordsAndRisks : ( Int, Int ) -> Cave -> List ( ( Int, Int ), Int )
adjacentCoordsAndRisks ( x, y ) cave =
    [ ( x + 1, y ), ( x - 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
        |> List.filterMap (\coord -> Dict.get coord cave |> Maybe.map (\value -> ( coord, value )))


{-| Given a coordinate, get adjacent coords in the space made by the base map and the virtual maps in the 5x5 grid
-}
virtualAdjacentCoordsAndRisks : ( Int, Int ) -> ( Int, Int ) -> Cave -> List ( ( Int, Int ), Int )
virtualAdjacentCoordsAndRisks boardSize ( x, y ) cave =
    [ ( x + 1, y ), ( x - 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
        |> List.filterMap (virtualValue boardSize cave)


virtualValue : ( Int, Int ) -> Cave -> ( Int, Int ) -> Maybe ( ( Int, Int ), Int )
virtualValue ( boardSizeX, boardSizeY ) cave ( x, y ) =
    let
        -- translate the coordinate to the base board coords
        ( transX, transY ) =
            Tuple.mapBoth (remainderBy boardSizeX) (remainderBy boardSizeY) ( x, y )

        -- figure out which virtual board we are accessing, needed to add extra risk
        ( boardX, boardY ) =
            Tuple.mapBoth (\xVal -> xVal // boardSizeX) (\yVal -> yVal // boardSizeY) ( x, y )
    in
    -- reject out of bounds coordinates
    if boardX > 4 || boardY > 4 || boardX < 0 || boardY < 0 then
        Nothing

    else
        cave
            |> Dict.get ( transX, transY )
            |> Maybe.map (addExtraVirtualRisk boardX boardY)
            |> Maybe.map wrapAround
            |> Maybe.map (\risk -> ( ( x, y ), risk ))


{-| Add extra virtual risk depending on the virtual board accessed
-}
addExtraVirtualRisk : Int -> Int -> Int -> Int
addExtraVirtualRisk boardX boardY baseValue =
    baseValue + boardX + boardY


{-| Wrap around so our values go from 1 to 9
-}
wrapAround : Int -> Int
wrapAround value =
    value
        - 1
        |> remainderBy 9
        |> (+) 1
