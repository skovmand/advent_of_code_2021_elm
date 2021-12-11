module Day3 exposing (parseInput, solvePart1, solvePart2)

import Array exposing (..)
import List.Extra


parseInput : String -> List (List Char)
parseInput input =
    input
        |> String.trim
        |> String.lines
        |> List.map String.toList



------------
-- Part 1 --
------------


solvePart1 : List (List Char) -> Int
solvePart1 binaryList =
    binaryList
        |> List.Extra.transpose
        |> List.map summarizeBinary
        |> binarySummariesToGammaAndEpsilon
        |> (\rates -> rates.gammaRate * rates.epsilonRate)


{-| Convert a list of binary summaries to gamma and epsilon rate
-}
binarySummariesToGammaAndEpsilon : List BinarySummary -> { gammaRate : Int, epsilonRate : Int }
binarySummariesToGammaAndEpsilon summaries =
    { gammaRate = binarySummariesToBase10 toMostCommonBit summaries
    , epsilonRate = binarySummariesToBase10 toLeastCommonBit summaries
    }



------------
-- Part 2 --
------------


solvePart2 : List (List Char) -> Maybe Int
solvePart2 binaryList =
    let
        oxygenGeneratorRating =
            calculateRating toMostCommonBit binaryList

        co2ScrubberRating =
            calculateRating toLeastCommonBit binaryList
    in
    Maybe.map2 (\a b -> a * b) oxygenGeneratorRating co2ScrubberRating


{-| Calculate a rating given a SuperSummarizer
-}
calculateRating : SuperSummarizer -> List (List Char) -> Maybe Int
calculateRating summarizer binaryList =
    let
        -- Convert binaries to arrays to be able to use Array.get later on
        binariesArrays : List (Array Char)
        binariesArrays =
            List.map Array.fromList binaryList
    in
    binaryList
        |> listOfBinaryIndexes
        |> Maybe.map (List.foldl (filterBinariesByBitAtIndex summarizer) binariesArrays)
        |> Maybe.andThen singleElementListToBase10


{-| Generate a list of indexes to consider, e.g. [0, 1, 2, 3]
-}
listOfBinaryIndexes : List (List Char) -> Maybe (List Int)
listOfBinaryIndexes binaryList =
    let
        binaryLength binary =
            binary |> List.length |> (+) -1
    in
    binaryList
        |> List.head
        |> Maybe.map (binaryLength >> List.range 0)


{-| Function to be used with List.foldl iteratively to filter a list of binaries by most common bit.
Can be curried with a SuperSummarizer, and then it takes the index, and a list of binaries.
-}
filterBinariesByBitAtIndex : SuperSummarizer -> Int -> List (Array Char) -> List (Array Char)
filterBinariesByBitAtIndex summarizer index binaries =
    let
        -- Map the remaining binaries into a list of column bits
        bitToKeep : Char
        bitToKeep =
            binaries
                |> List.filterMap (Array.get index)
                |> summarizeBinary
                |> summarizer
    in
    case binaries of
        -- Only one binary left, return that for the remaining iterations
        [ binary ] ->
            [ binary ]

        remainingBinaries ->
            List.filter
                (\binary ->
                    Array.get index binary
                        |> Maybe.andThen (\bit -> Just (bit == bitToKeep))
                        |> Maybe.withDefault False
                )
                remainingBinaries



------------
-- Common --
------------


{-| This is a summary of a binary
-}
type alias BinarySummary =
    { zeros : Int, ones : Int }


{-| A SuperSummarizer takes a BinarySummary and reduces it to a single character,
in a sense summarizing the summaries!
-}
type alias SuperSummarizer =
    BinarySummary -> Char


summarizeBinary : List Char -> BinarySummary
summarizeBinary list =
    List.foldl
        (\bit acc ->
            case bit of
                '0' ->
                    { acc | zeros = acc.zeros + 1 }

                '1' ->
                    { acc | ones = acc.ones + 1 }

                _ ->
                    acc
        )
        (BinarySummary 0 0)
        list


binarySummariesToBase10 : SuperSummarizer -> List BinarySummary -> Int
binarySummariesToBase10 summarizer binarySummaries =
    binarySummaries
        |> List.map summarizer
        |> binaryToBase10


toMostCommonBit : SuperSummarizer
toMostCommonBit summary =
    if summary.ones >= summary.zeros then
        '1'

    else
        '0'


toLeastCommonBit : SuperSummarizer
toLeastCommonBit summary =
    if summary.ones >= summary.zeros then
        '0'

    else
        '1'


singleElementListToBase10 : List (Array Char) -> Maybe Int
singleElementListToBase10 list =
    list
        |> List.head
        |> Maybe.map (Array.toList >> binaryToBase10)


binaryToBase10 : List Char -> Int
binaryToBase10 bits =
    List.foldr
        (\bit acc ->
            if bit == '1' then
                { position = acc.position + 1, sum = acc.sum + 2 ^ acc.position }

            else
                { acc | position = acc.position + 1 }
        )
        { position = 0, sum = 0 }
        bits
        |> .sum
