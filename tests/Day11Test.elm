module Day11Test exposing (..)

import Day11
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "day 11"
        [ test "part 1 example" <|
            \() ->
                exampleInput
                    |> Day11.parseInput
                    |> Maybe.map Day11.solvePart1
                    |> Expect.equal (Just 1656)
        , test "part 1 answer" <|
            \() ->
                puzzleInput
                    |> Day11.parseInput
                    |> Maybe.map Day11.solvePart1
                    |> Expect.equal (Just 1679)
        , test "part 2 example" <|
            \() ->
                exampleInput
                    |> Day11.parseInput
                    |> Maybe.map Day11.solvePart2
                    |> Expect.equal (Just 195)
        , test "part 2 answer" <|
            \() ->
                puzzleInput
                    |> Day11.parseInput
                    |> Maybe.map Day11.solvePart2
                    |> Expect.equal (Just 519)
        ]


exampleInput : String
exampleInput =
    """
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
"""


puzzleInput : String
puzzleInput =
    """
1553421288
5255384882
1224315732
4258242274
1658564216
6872651182
5775552238
5622545172
8766672318
2178374835
"""
