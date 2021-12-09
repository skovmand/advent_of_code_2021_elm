module Day3 exposing (parseInput, solvePart1)

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
        |> toBasicRates
        |> (\rates -> rates.gammaRate * rates.epsilonRate)


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


toBasicRates : List BinarySummary -> { gammaRate : Int, epsilonRate : Int }
toBasicRates summary =
    let
        gammaRate =
            summary
                |> List.map toMostCommonBit
                |> binaryToBase10

        epsilonRate =
            summary
                |> List.map toLeastCommonBit
                |> binaryToBase10
    in
    { gammaRate = gammaRate, epsilonRate = epsilonRate }


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
