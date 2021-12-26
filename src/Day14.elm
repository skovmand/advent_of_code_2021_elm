module Day14 exposing (parseInput, solvePart1, solvePart2)

import Dict exposing (Dict)
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
    let
        initialLetterCount =
            countInitialLetters polymerTemplate
    in
    polymerTemplate
        |> pairwise
        |> initialDict
        |> (\dictOfPairs -> applyPairInsertionRounds 10 insertionRules ( dictOfPairs, initialLetterCount ))
        |> calculateScore


solvePart2 : ParsedInput -> Maybe Int
solvePart2 { insertionRules, polymerTemplate } =
    let
        initialLetterCount =
            countInitialLetters polymerTemplate
    in
    polymerTemplate
        |> pairwise
        |> initialDict
        |> (\dictOfPairs -> applyPairInsertionRounds 40 insertionRules ( dictOfPairs, initialLetterCount ))
        |> calculateScore


countInitialLetters : String -> Dict String Int
countInitialLetters string =
    string
        |> String.split ""
        |> List.foldl
            (\letter dictAcc -> Dict.update letter (upsertCount 1) dictAcc)
            Dict.empty


pairwise : String -> List ( String, String )
pairwise string =
    let
        split =
            String.split "" string
    in
    List.map2 Tuple.pair split (List.drop 1 split)


type alias DictOfPairs =
    Dict ( String, String ) Int


type alias InsertedLetterCountDict =
    Dict String Int


initialDict : List ( String, String ) -> DictOfPairs
initialDict letterPairs =
    List.foldl
        (\entry dictAcc -> Dict.update entry (upsertCount 1) dictAcc)
        Dict.empty
        letterPairs


applyPairInsertionRounds : Int -> InsertionRules -> ( DictOfPairs, InsertedLetterCountDict ) -> InsertedLetterCountDict
applyPairInsertionRounds rounds rules ( dictOfPairs, insertedLetterCounts ) =
    if rounds < 1 then
        insertedLetterCounts

    else
        Dict.foldl
            (\( a, b ) count ( dictOfPairsAcc, insertedLettersAcc ) ->
                let
                    c =
                        Dict.get ( a, b ) rules
                            |> Maybe.withDefault "*"

                    updatedDictOfPairs =
                        dictOfPairsAcc
                            |> Dict.update ( a, c ) (upsertCount count)
                            |> Dict.update ( c, b ) (upsertCount count)

                    updatedInsertedLetters =
                        insertedLettersAcc
                            |> Dict.update c (upsertCount count)
                in
                ( updatedDictOfPairs, updatedInsertedLetters )
            )
            ( Dict.empty, insertedLetterCounts )
            dictOfPairs
            |> applyPairInsertionRounds (rounds - 1) rules


upsertCount : Int -> Maybe Int -> Maybe Int
upsertCount amount maybeValue =
    case maybeValue of
        Just v ->
            Just (v + amount)

        Nothing ->
            Just amount


calculateScore : Dict String Int -> Maybe Int
calculateScore dict =
    let
        list =
            Dict.values dict
    in
    Maybe.map2
        (\max min -> max - min)
        (List.maximum list)
        (List.minimum list)
