module Day14 exposing (parseInput, solvePart1, solvePart2)

{-| Day 14: Extended Polymerization
<https://adventofcode.com/2021/day/14>
-}

import Dict exposing (Dict)
import Utilities exposing (maybeAll, unwrapMaybe)



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
solvePart1 parsedInput =
    solveRounds 10 parsedInput


solvePart2 : ParsedInput -> Maybe Int
solvePart2 parsedInput =
    solveRounds 40 parsedInput


solveRounds : Int -> ParsedInput -> Maybe Int
solveRounds rounds { insertionRules, polymerTemplate } =
    let
        initialLetterCount =
            toInitialLetterCount polymerTemplate

        initialDictOfPairs =
            polymerTemplate |> pairwise |> toInitialDict
    in
    applyPairInsertionRounds rounds insertionRules ( initialDictOfPairs, initialLetterCount )
        |> calculateScore


type alias DictOfPairs =
    Dict ( String, String ) Int


type alias InsertedLetterCountDict =
    Dict String Int


{-| Generate the initial letter counts from the polymer template
-}
toInitialLetterCount : String -> InsertedLetterCountDict
toInitialLetterCount string =
    string
        |> String.split ""
        |> List.foldl
            (\letter dictAcc -> Dict.update letter (upsertCount 1) dictAcc)
            Dict.empty


{-| Generate the initial dictionary from the letter pairs
-}
toInitialDict : List ( String, String ) -> DictOfPairs
toInitialDict letterPairs =
    List.foldl
        (\entry dictAcc -> Dict.update entry (upsertCount 1) dictAcc)
        Dict.empty
        letterPairs


{-| Generate a list of 2-tuples from a string using a sliding window moving 1 character forward in each step
For example "HEY" -> [("H", "E"), ("E", "Y")]
-}
pairwise : String -> List ( String, String )
pairwise string =
    let
        split =
            String.split "" string
    in
    List.map2 Tuple.pair split (List.drop 1 split)


{-| Recursive solving of the puzzle given a number of rounds, insertion rules and the initial states
-}
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
                            |> unwrapMaybe

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


{-| Handy helper to update the count of an element by an amount,
inserting the count if not present already
-}
upsertCount : Int -> Maybe Int -> Maybe Int
upsertCount amount maybeValue =
    case maybeValue of
        Just v ->
            Just (v + amount)

        Nothing ->
            Just amount


{-| Calculate the final score
-}
calculateScore : InsertedLetterCountDict -> Maybe Int
calculateScore dict =
    let
        list =
            Dict.values dict
    in
    Maybe.map2
        (\max min -> max - min)
        (List.maximum list)
        (List.minimum list)
