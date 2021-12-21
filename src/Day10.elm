module Day10 exposing (parseInput, solvePart1, solvePart2)

import Utilities exposing (maybeAll)


{-| Day 10: Syntax Scoring
I was hoping to improve the handling of Maybes in the code by parsing everything up front.
It turned out to be very verbose :-)
-}



---------------
-- PARSE
---------------


type ChunkChars
    = OpeningParenthesis
    | OpeningBracket
    | OpeningCurlyBrace
    | OpeningPizza
    | ClosingParenthesis
    | ClosingBracket
    | ClosingCurlyBrace
    | ClosingPizza


parseInput : String -> Maybe (List (List ChunkChars))
parseInput input =
    input
        |> String.trim
        |> String.lines
        |> List.map (String.split "")
        |> List.map toEnumeration
        |> maybeAll


toEnumeration : List String -> Maybe (List ChunkChars)
toEnumeration listOfChars =
    List.map
        (\char ->
            case char of
                "(" ->
                    Just OpeningParenthesis

                "[" ->
                    Just OpeningBracket

                "{" ->
                    Just OpeningCurlyBrace

                "<" ->
                    Just OpeningPizza

                ")" ->
                    Just ClosingParenthesis

                "]" ->
                    Just ClosingBracket

                "}" ->
                    Just ClosingCurlyBrace

                ">" ->
                    Just ClosingPizza

                _ ->
                    Nothing
        )
        listOfChars
        |> maybeAll



-----------------------------
-- PART 1
-----------------------------


solvePart1 : List (List ChunkChars) -> Int
solvePart1 list =
    list
        |> List.map parseChunkLine
        |> List.filter (\chunkLine -> not (isIncomplete chunkLine))
        |> List.map calculateLineScore
        |> List.sum


calculateLineScore : ParseStatus -> Int
calculateLineScore status =
    case status of
        InvalidCharacter char ->
            case char of
                ClosingParenthesis ->
                    3

                ClosingBracket ->
                    57

                ClosingCurlyBrace ->
                    1197

                ClosingPizza ->
                    25137

                _ ->
                    0

        _ ->
            0



-----------------------------
-- PART 2
-----------------------------


solvePart2 : List (List ChunkChars) -> Maybe Int
solvePart2 list =
    list
        |> List.map parseChunkLine
        |> List.filterMap getRemainingStack
        |> List.map (List.map invertChar)
        |> List.map calculateAutocompleteScore
        |> middleElementInList


getRemainingStack : ParseStatus -> Maybe (List ChunkChars)
getRemainingStack status =
    case status of
        Successful ->
            Nothing

        Incomplete stack ->
            Just stack

        InvalidCharacter _ ->
            Nothing


calculateAutocompleteScore : List ChunkChars -> Int
calculateAutocompleteScore list =
    List.foldl
        (\char score -> score * 5 + pointsForChar char)
        0
        list


pointsForChar : ChunkChars -> Int
pointsForChar char =
    case char of
        ClosingParenthesis ->
            1

        ClosingBracket ->
            2

        ClosingCurlyBrace ->
            3

        ClosingPizza ->
            4

        _ ->
            0


middleElementInList : List Int -> Maybe Int
middleElementInList list =
    let
        length =
            List.length list
    in
    if remainderBy 2 length == 0 then
        Nothing

    else
        list
            |> List.sort
            |> List.drop (length // 2)
            |> List.head



---------------------------------------------
-- THE AWESOME PART: THE RECURSIVE PARSER
---------------------------------------------


type ParseStatus
    = Successful
    | InvalidCharacter ChunkChars
    | Incomplete (List ChunkChars)


parseChunkLine : List ChunkChars -> ParseStatus
parseChunkLine chunkList =
    doParseChunkLine [] chunkList


doParseChunkLine : List ChunkChars -> List ChunkChars -> ParseStatus
doParseChunkLine stack chunkString =
    case chunkString of
        [] ->
            if stack == [] then
                Successful

            else
                Incomplete stack

        head :: tail ->
            if isOpeningChar head then
                doParseChunkLine (head :: stack) tail

            else if isClosingChar head stack then
                doParseChunkLine (List.drop 1 stack) tail

            else
                InvalidCharacter head


isOpeningChar : ChunkChars -> Bool
isOpeningChar char =
    List.member char [ OpeningParenthesis, OpeningBracket, OpeningCurlyBrace, OpeningPizza ]


{-| Given a character, check if it is a closing character matching the character at the head of the stack
-}
isClosingChar : ChunkChars -> List ChunkChars -> Bool
isClosingChar char stack =
    case stack of
        stackHead :: _ ->
            List.member char [ ClosingParenthesis, ClosingBracket, ClosingCurlyBrace, ClosingPizza ]
                && (char == invertChar stackHead)

        [] ->
            False


invertChar : ChunkChars -> ChunkChars
invertChar chunk =
    case chunk of
        OpeningParenthesis ->
            ClosingParenthesis

        OpeningBracket ->
            ClosingBracket

        OpeningCurlyBrace ->
            ClosingCurlyBrace

        OpeningPizza ->
            ClosingPizza

        ClosingParenthesis ->
            OpeningParenthesis

        ClosingBracket ->
            OpeningBracket

        ClosingCurlyBrace ->
            OpeningCurlyBrace

        ClosingPizza ->
            OpeningPizza


isIncomplete : ParseStatus -> Bool
isIncomplete status =
    case status of
        Incomplete _ ->
            True

        _ ->
            False
