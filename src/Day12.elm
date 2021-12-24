module Day12 exposing (parseInput, solvePart1, solvePart2)

{-| Day 12: Passage Pathing

Your goal is to find the number of distinct paths that start at start, end at end,
and don't visit small caves more than once. There are two types of caves: big caves
(written in uppercase, like A) and small caves (written in lowercase, like b).

-}

import Dict exposing (Dict)
import List.Extra
import Set
import Utilities exposing (maybeAll)



---------------
-- PARSE
---------------


parseInput : String -> Maybe (Dict String (List String))
parseInput input =
    input
        |> String.trim
        |> String.lines
        |> List.map toTuples
        |> maybeAll
        |> Maybe.map (List.concatMap withReverseEntries)
        |> Maybe.map toDict


toTuples : String -> Maybe ( String, String )
toTuples input =
    case String.split "-" input of
        [ a, b ] ->
            Just ( a, b )

        _ ->
            Nothing


{-| Reverse all entries (since a connection from b to a is also a connection from a to b
-}
withReverseEntries : ( String, String ) -> List ( String, String )
withReverseEntries ( a, b ) =
    [ ( a, b ), ( b, a ) ]


toDict : List ( String, String ) -> Dict String (List String)
toDict connections =
    List.foldl
        (\( a, b ) dictAcc ->
            Dict.update a
                (\nodeConnections ->
                    case nodeConnections of
                        Just list ->
                            Just (b :: list)

                        Nothing ->
                            Just [ b ]
                )
                dictAcc
        )
        Dict.empty
        connections



-----------------------------
-- PART 1
-----------------------------


{-| Part 1: Just traverse, making sure small caves are never visited more than once
-}
solvePart1 : Dict String (List String) -> Int
solvePart1 connections =
    traversePaths "start" connections [] Dict.empty
        |> List.length



-----------------------------
-- PART 2
-----------------------------


{-| Part 2: Make a small hack by finding all small caves, then traversing for each of them.
This is allowed by setting the visit count to -1 for the cave that is allowed to be visited twice.
-}
solvePart2 : Dict String (List String) -> Int
solvePart2 connections =
    connections
        |> smallCaves
        |> List.foldl
            (\nodeThatCanBeVisitedTwice pathSet ->
                let
                    smallCaveVisits =
                        Dict.fromList [ ( nodeThatCanBeVisitedTwice, -1 ) ]
                in
                traversePaths "start" connections [] smallCaveVisits
                    |> List.foldl (\path pathSetAcc -> Set.insert path pathSetAcc) pathSet
            )
            Set.empty
        |> Set.size


smallCaves : Dict String (List String) -> List String
smallCaves connections =
    connections
        |> Dict.keys
        |> List.filter (\nodeName -> nodeName /= "start" && nodeName /= "end" && isSmallCave nodeName)
        |> List.Extra.unique


isSmallCave : String -> Bool
isSmallCave nodeName =
    String.toLower nodeName == nodeName


traversePaths : String -> Dict String (List String) -> List String -> Dict String Int -> List (List String)
traversePaths node allConnections currentNodePath smallCavesVisits =
    let
        updatedSmallCavesVisits =
            if String.toLower node == node then
                Dict.update node
                    (\visits ->
                        case visits of
                            Just v ->
                                Just (v + 1)

                            Nothing ->
                                Just 1
                    )
                    smallCavesVisits

            else
                smallCavesVisits

        -- get the connections for this node, remove small caves already visited
        nodeConnections =
            Dict.get node allConnections
                |> Maybe.withDefault []
                |> removeSmallCaves smallCavesVisits
    in
    -- go through each connection
    List.foldl
        (\nodeName connectionAcc ->
            let
                updatedCurrentPath =
                    nodeName :: currentNodePath
            in
            -- the end node is reached, return the full path that got us here
            if nodeName == "end" then
                updatedCurrentPath :: connectionAcc

            else
                -- we are not at an end node, traverse further into the graph
                -- once returning, append the paths to the accumulator
                List.append
                    (traversePaths nodeName allConnections updatedCurrentPath updatedSmallCavesVisits)
                    connectionAcc
        )
        []
        nodeConnections


removeSmallCaves : Dict String Int -> List String -> List String
removeSmallCaves smallCavesVisited nodeList =
    nodeList
        |> List.filter (nodeNeverVisited smallCavesVisited)


nodeNeverVisited : Dict String Int -> String -> Bool
nodeNeverVisited smallCavesVisited nodeName =
    case Dict.get nodeName smallCavesVisited of
        Just 1 ->
            False

        _ ->
            True
