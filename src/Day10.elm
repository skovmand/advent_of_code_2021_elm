module Day10 exposing (parseInput, solvePart1)

import Utilities exposing (maybeAll)


{-| Day 10: Syntax Scoring
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
        |> List.filter (\chunkLine -> chunkLine /= Incomplete)
        |> calculateTotalScore


calculateTotalScore : List ParseStatus -> Int
calculateTotalScore statuses =
    List.map calculateLineScore statuses
    |> List.sum


calculateLineScore : ParseStatus -> Int
calculateLineScore status =
    case status of
        Successful ->
            0

        Incomplete ->
            0

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


type ParseStatus
    = Successful
    | InvalidCharacter ChunkChars
    | Incomplete


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
                Incomplete

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
