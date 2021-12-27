module Day15 exposing (parseInput, solvePart1)

{-| Day 15: Chiton
<https://adventofcode.com/2021/day/15>

Solution finding the shortest path using Dijkstras shortest path algorithm.
Used resources:

  - <https://www.youtube.com/watch?v=GazC3A4OQTE>

-}

import Dict exposing (Dict)
import PriorityQueue exposing (PriorityQueue)
import Set exposing (Set)
import Utilities exposing (maybeAll, unwrapMaybe)



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
        |> unwrapMaybe
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
-- PART 1
-----------------------------


solvePart1 : Cave -> Int
solvePart1 cave =
    let
        endCoord : ( Int, Int )
        endCoord =
            cave
                |> Dict.toList
                |> lastListElement
                |> unwrapMaybe
                |> Tuple.first
    in
    doTheDijkstra endCoord cave Set.empty initialPriorityQueue



-- ved hjælp af initial priority queue
-- check om vi har nået lastCoord
-- hvis ja, break - vi skal bare bruge summen af distancen, ikke stien selv
-- ellers:
-- tag head fra priority queue
-- scan naboerne til koordinatet
-- læg deres afstand til den samlede afstand, indsæt dem i priority queue


doTheDijkstra : ( Int, Int ) -> Cave -> Set ( Int, Int ) -> PriorityQueue ( ( Int, Int ), Int ) -> Int
doTheDijkstra endCoord cave visited priorityQueue =
    let
        ( coord, totalRisk ) =
            PriorityQueue.head priorityQueue |> unwrapMaybe

        updatedPriorityQueue =
            adjacentCoordsAndRisks coord cave visited
                |> List.foldl
                    -- Bug: Only update coords already present if the total risk is lower, don't duplicate!
                    (\( adjCoord, risk ) priorityQueueAcc -> PriorityQueue.insert ( adjCoord, totalRisk + risk ) priorityQueueAcc)
                    (PriorityQueue.tail priorityQueue)
    in
    if coord == endCoord then
        totalRisk

    else
        doTheDijkstra endCoord cave (Set.insert coord visited) updatedPriorityQueue


initialPriorityQueue : PriorityQueue ( ( Int, Int ), Int )
initialPriorityQueue =
    PriorityQueue.empty Tuple.second
        |> PriorityQueue.insert ( ( 0, 0 ), 0 )


{-| Given a coordinate, get adjacent coords. Filters out non-existing fields
(e.g. when on the edge of the map)
-}
adjacentCoordsAndRisks : ( Int, Int ) -> Cave -> Set ( Int, Int ) -> List ( ( Int, Int ), Int )
adjacentCoordsAndRisks ( x, y ) cave visited =
    [ ( x + 1, y ), ( x - 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
        |> List.filter (\coord -> not (Set.member coord visited))
        |> List.filterMap (\coord -> Dict.get coord cave |> Maybe.map (\value -> ( coord, value )))


{-| Get the last element of a list
-}
lastListElement : List a -> Maybe a
lastListElement list =
    case list of
        [] ->
            Nothing

        [ value ] ->
            Just value

        _ :: rest ->
            lastListElement rest
