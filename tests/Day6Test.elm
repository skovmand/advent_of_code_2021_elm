module Day6Test exposing (..)

import Day6
import Expect exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "day 6"
        [ test "part 1 example" <|
            \() ->
                exampleInput
                    |> Day6.parseInput
                    |> Maybe.map Day6.solvePart1
                    |> Expect.equal (Just 5934)
        , test "part 1 answer" <|
            \() ->
                puzzleInput
                    |> Day6.parseInput
                    |> Maybe.map Day6.solvePart1
                    |> Expect.equal (Just 390011)
--        , test "part 2 example" <|
--            \() ->
--                exampleInput
--                    |> Day6.parseInput
--                    |> Maybe.map Day6.solvePart2
--                    |> Expect.equal (Just 26984457539)
--        , test "part 2 answer" <|
--            \() ->
--                puzzleInput
--                    |> Day6.parseInput
--                    |> Maybe.map Day6.solvePart2
--                    |> Expect.equal (Just 5)
        ]


exampleInput : String
exampleInput =
    "3,4,3,1,2"


puzzleInput : String
puzzleInput =
    "1,3,4,1,1,1,1,1,1,1,1,2,2,1,4,2,4,1,1,1,1,1,5,4,1,1,2,1,1,1,1,4,1,1,1,4,4,1,1,1,1,1,1,1,2,4,1,3,1,1,2,1,2,1,1,4,1,1,1,4,3,1,3,1,5,1,1,3,4,1,1,1,3,1,1,1,1,1,1,1,1,1,1,1,1,1,5,2,5,5,3,2,1,5,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,5,1,1,1,1,5,1,1,1,1,1,4,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,1,3,1,2,4,1,5,5,1,1,5,3,4,4,4,1,1,1,2,1,1,1,1,1,1,2,1,1,1,1,1,1,5,3,1,4,1,1,2,2,1,2,2,5,1,1,1,2,1,1,1,1,3,4,5,1,2,1,1,1,1,1,5,2,1,1,1,1,1,1,5,1,1,1,1,1,1,1,5,1,4,1,5,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,5,4,5,1,1,1,1,1,1,1,5,1,1,3,1,1,1,3,1,4,2,1,5,1,3,5,5,2,1,3,1,1,1,1,1,3,1,3,1,1,2,4,3,1,4,2,2,1,1,1,1,1,1,1,5,2,1,1,1,2"
