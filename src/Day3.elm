module Day3 exposing (parseInput, solvePart1, solvePart2)

import Array exposing (..)
import List.Extra


parseInput : String -> List (List Char)
parseInput input =
    input
        |> String.trim
        |> String.lines
        |> List.map String.toList


solvePart1 : List (List Char) -> Int
solvePart1 binaryList =
    binaryList
        |> List.Extra.transpose
        |> List.map summarizeBinary
        |> binarySummariesToGammaAndEpsilon
        |> (\rates -> rates.gammaRate * rates.epsilonRate)


solvePart2 : List (List Char) -> Maybe Int
solvePart2 binaryList =
    let
        -- Convert binaries to arrays to be able to use Array.get
        binariesArrays : List (Array Char)
        binariesArrays =
            List.map Array.fromList binaryList

        -- Generate the indexes to iterate over (defaulting to [] for convenience)
        indexes : List Int
        indexes =
            listOfIndexes binaryList
                |> Maybe.withDefault []

        oxygenGeneratorRating =
            List.foldl
                (filterBinariesByCriteriaAndIndex toMostCommonBit)
                binariesArrays
                indexes
                |> singleElementListToBase10

        co2ScrubberRating =
            List.foldl
                (filterBinariesByCriteriaAndIndex toLeastCommonBit)
                binariesArrays
                indexes
                |> singleElementListToBase10
    in
    Maybe.map2 (\a b -> a * b) oxygenGeneratorRating co2ScrubberRating


{-| Generate a list of indexes to consider, e.g. [0, 1, 2, 3]
-}
listOfIndexes : List (List Char) -> Maybe (List Int)
listOfIndexes binaryList =
    let
        binaryLength binary =
            binary |> List.length |> (+) -1
    in
    binaryList
        |> List.head
        |> Maybe.map (binaryLength >> List.range 0)


{-| Function to be used with List.foldl iteratively to filter a list of binaries by most common bit.
Can be curried with the SelectionCriteria function which is (BinarySummary -> Char), and then it takes
the index, and a list of binaries.
-}
filterBinariesByCriteriaAndIndex : (BinarySummary -> Char) -> Int -> List (Array Char) -> List (Array Char)
filterBinariesByCriteriaAndIndex selectionCriteria index binaries =
    let
        -- Map the remaining binaries into a list of column bits
        bitToKeep : Char
        bitToKeep =
            binaries
                |> List.filterMap (Array.get index)
                |> summarizeBinary
                |> selectionCriteria
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


singleElementListToBase10 : List (Array Char) -> Maybe Int
singleElementListToBase10 list =
    list
        |> List.head
        |> Maybe.map (Array.toList >> binaryToBase10)


type alias BinarySummary =
    { zeros : Int, ones : Int }


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


{-| Convert a list of binary summaries to gamma and epsilon rate
-}
binarySummariesToGammaAndEpsilon : List BinarySummary -> { gammaRate : Int, epsilonRate : Int }
binarySummariesToGammaAndEpsilon summaries =
    { gammaRate = binarySummariesToBase10 toMostCommonBit summaries, epsilonRate = binarySummariesToBase10 toLeastCommonBit summaries }


{-| Given a selection criteria (e.g. toMostCommonBit), convert a list of binary summaries to an integer
-}
binarySummariesToBase10 : (BinarySummary -> Char) -> List BinarySummary -> Int
binarySummariesToBase10 selectionCriteria binarySummaries =
    binarySummaries
        |> List.map selectionCriteria
        |> binaryToBase10


toMostCommonBit : BinarySummary -> Char
toMostCommonBit summary =
    if summary.ones >= summary.zeros then
        '1'

    else
        '0'


toLeastCommonBit : BinarySummary -> Char
toLeastCommonBit summary =
    if summary.ones >= summary.zeros then
        '0'

    else
        '1'


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
