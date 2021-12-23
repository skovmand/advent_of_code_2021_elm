module Day12Test exposing (..)

import Day12
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "day 12"
        [ test "part 1 example 1" <|
            \() ->
                exampleInput1
                    |> Day12.parseInput
                    |> Maybe.map Day12.solvePart1
                    |> Expect.equal (Just 10)
        , test "part 1 example 2" <|
            \() ->
                exampleInput2
                    |> Day12.parseInput
                    |> Maybe.map Day12.solvePart1
                    |> Expect.equal (Just 19)
        , test "part 1 example 3" <|
            \() ->
                exampleInput3
                    |> Day12.parseInput
                    |> Maybe.map Day12.solvePart1
                    |> Expect.equal (Just 226)
        , test "part 1 answer" <|
            \() ->
                puzzleInput
                    |> Day12.parseInput
                    |> Maybe.map Day12.solvePart1
                    |> Expect.equal (Just 4573)

        --, test "part 2 example" <|
        --    \() ->
        --        exampleInput
        --            |> Day12.parseInput
        --            |> Maybe.map Day12.solvePart2
        --            |> Expect.equal (Just 195)
        --, test "part 2 answer" <|
        --    \() ->
        --        puzzleInput
        --            |> Day12.parseInput
        --            |> Maybe.map Day12.solvePart2
        --            |> Expect.equal (Just 519)
        ]


exampleInput1 : String
exampleInput1 =
    """
start-A
start-b
A-c
A-b
b-d
A-end
b-end
"""


exampleInput2 : String
exampleInput2 =
    """
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
"""


exampleInput3 : String
exampleInput3 =
    """
fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
"""


puzzleInput : String
puzzleInput =
    """
fw-ll
end-dy
tx-fw
tx-tr
dy-jb
ZD-dy
dy-BL
dy-tr
dy-KX
KX-start
KX-tx
fw-ZD
tr-end
fw-jb
fw-yi
ZD-nr
start-fw
tx-ll
ll-jb
yi-jb
yi-ll
yi-start
ZD-end
ZD-jb
tx-ZD
"""
