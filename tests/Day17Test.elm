module Day17Test exposing (..)

import Day17
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "day 17"
        [ test "part 1 example" <|
            \() ->
                exampleInput
                    |> Day17.parseInput
                    |> Maybe.andThen Day17.solvePart1
                    |> Expect.equal (Just 45)
        , test "part 1 answer" <|
            \() ->
                puzzleInput
                    |> Day17.parseInput
                    |> Maybe.andThen Day17.solvePart1
                    |> Expect.equal (Just 11175)
        , test "part 2 example" <|
            \() ->
                exampleInput
                    |> Day17.parseInput
                    |> Maybe.map Day17.solvePart2
                    |> Expect.equal (Just 112)
        , test "part 2 answer" <|
            \() ->
                puzzleInput
                    |> Day17.parseInput
                    |> Maybe.map Day17.solvePart2
                    |> Expect.equal (Just 3540)
        ]


exampleInput : String
exampleInput =
    "target area: x=20..30, y=-10..-5"


puzzleInput : String
puzzleInput =
    "target area: x=81..129, y=-150..-108"
