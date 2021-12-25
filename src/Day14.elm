module Day14 exposing (parseInput, solvePart1)

import Dict exposing (Dict)
import List.Extra
import Utilities exposing (maybeAll)


{-| Day 14: Extended Polymerization
-}



---------------
-- PARSE
---------------


type alias InsertionRules =
    Dict ( String, String ) String


type alias ParsedInput =
    { polymerTemplate : String, insertionRules : InsertionRules }


parseInput : String -> Maybe ParsedInput
parseInput input =
    let
        splitInput =
            input
                |> String.trim
                |> String.split "\n\n"
    in
    case splitInput of
        [ template, rules ] ->
            Maybe.map2
                ParsedInput
                (Just template)
                (parseRules rules)

        _ ->
            Nothing


parseRules : String -> Maybe (Dict ( String, String ) String)
parseRules rules =
    rules
        |> String.lines
        |> List.map parseRule
        |> maybeAll
        |> Maybe.map fillDict


parseRule : String -> Maybe ( ( String, String ), String )
parseRule line =
    case String.split " -> " line of
        [ elements, insertedElement ] ->
            case String.split "" elements of
                [ a, b ] ->
                    Just ( ( a, b ), insertedElement )

                _ ->
                    Nothing

        _ ->
            Nothing


fillDict : List ( ( String, String ), String ) -> Dict ( String, String ) String
fillDict ruleList =
    List.foldl
        (\( rule, insertion ) dictAcc -> Dict.insert rule insertion dictAcc)
        Dict.empty
        ruleList



-----------------------------
-- PART 1
-----------------------------


solvePart1 : ParsedInput -> Maybe Int
solvePart1 { insertionRules, polymerTemplate } =
    polymerTemplate
        |> applyPairInsertionRounds 10 insertionRules
        |> calculateScore


pairwise : String -> List ( String, String )
pairwise string =
    let
        split =
            String.split "" string
    in
    List.map2 Tuple.pair split (List.drop 1 split)


applyPairInsertionRounds : Int -> InsertionRules -> String -> String
applyPairInsertionRounds rounds rules template =
    let
        insertionChars : List String
        insertionChars =
            List.map (\pair -> Dict.get pair rules) (pairwise template)
                |> maybeAll
                |> Maybe.withDefault []

        splitTemplate =
            String.split "" template
    in
    if rounds < 1 then
        template

    else
        List.Extra.interweave splitTemplate insertionChars
            |> String.join ""
            |> applyPairInsertionRounds (rounds - 1) rules


calculateScore : String -> Maybe Int
calculateScore template =
    let
        frequencies =
            template
                |> String.split ""
                |> List.foldl
                    (\char dictAcc ->
                        Dict.update char
                            (\value ->
                                case value of
                                    Just n ->
                                        Just (n + 1)

                                    Nothing ->
                                        Just 1
                            )
                            dictAcc
                    )
                    Dict.empty
                |> Dict.values
    in
    Maybe.map2
        (\max min -> max - min)
        (List.maximum frequencies)
        (List.minimum frequencies)
