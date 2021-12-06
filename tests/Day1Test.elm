module Day1Test exposing (..)

import Day1 exposing (..)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "day 1"
        [ test "part 1 example" <|
            \() ->
                Day1.exampleInput
                    |> Day1.parseInput
                    |> Day1.part1Answer
                    |> Expect.equal 7
        , test "part 1 answer" <|
            \() ->
                Day1.puzzleInput
                    |> Day1.parseInput
                    |> Day1.part1Answer
                    |> Expect.equal 1715
        , test "part 2 example" <|
            \() ->
                Day1.exampleInput
                    |> Day1.parseInput
                    |> Day1.part2Answer
                    |> Expect.equal 5
        , test "part 2 answer" <|
            \() ->
                Day1.puzzleInput
                    |> Day1.parseInput
                    |> Day1.part2Answer
                    |> Expect.equal 1739
        ]
