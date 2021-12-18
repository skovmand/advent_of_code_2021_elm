module Day8 exposing (parseInput, solvePart1)

import Utilities exposing (maybeAll)


parseInput : String -> Maybe (List ( List String, List String ))
parseInput input =
    input
        |> String.trim
        |> String.lines
        |> List.map (String.split " | ")
        |> List.map parseLine
        |> maybeAll


parseLine : List String -> Maybe ( List String, List String )
parseLine splitLine =
    case splitLine of
        [ patterns, digits ] ->
            let
                patternsList =
                    String.split " " patterns

                digitsList =
                    String.split " " digits
            in
            case ( List.length patternsList, List.length digitsList ) of
                ( 10, 4 ) ->
                    Just ( patternsList, digitsList )

                _ ->
                    Nothing

        _ ->
            Nothing


solvePart1 : List ( List String, List String ) -> Int
solvePart1 input =
    input
        |> List.concatMap (\( _, digits ) -> digits)
        |> List.filter (\digit -> List.member (String.length digit) [ 2, 3, 4, 7 ])
        |> List.length
