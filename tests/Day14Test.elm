module Day14Test exposing (..)

import Day14
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "day 14"
        [ test "part 1 example" <|
            \() ->
                exampleInput1
                    |> Day14.parseInput
                    |> Maybe.andThen Day14.solvePart1
                    |> Expect.equal (Just 1588)
        , test "part 1 answer" <|
            \() ->
                puzzleInput
                    |> Day14.parseInput
                    |> Maybe.andThen Day14.solvePart1
                    |> Expect.equal (Just 2345)

        --, test "part 2 example" <|
        --    \() ->
        --        exampleInput1
        --            |> Day14.parseInput
        --            |> Maybe.andThen Day14.solvePart2
        --            |> Expect.equal (Just 0)
        --, test "part 2 answer" <|
        --    \() ->
        --        puzzleInput
        --            |> Day14.parseInput
        --            |> Maybe.andThen Day14.solvePart2
        --            |> Expect.equal (Just 0)
        ]


exampleInput1 : String
exampleInput1 =
    """
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
"""


puzzleInput : String
puzzleInput =
    """
BNSOSBBKPCSCPKPOPNNK

HH -> N
CO -> F
BC -> O
HN -> V
SV -> S
FS -> F
CV -> F
KN -> F
OP -> H
VN -> P
PF -> P
HP -> H
FK -> K
BS -> F
FP -> H
FN -> V
VV -> O
PS -> S
SK -> N
FF -> K
PK -> V
OF -> N
VP -> K
KB -> H
OV -> B
CH -> F
SF -> F
NH -> O
NC -> N
SP -> N
NN -> F
OK -> S
BB -> S
NK -> S
FH -> P
FC -> S
OB -> P
VS -> P
BF -> S
HC -> V
CK -> O
NP -> K
KV -> S
OS -> V
CF -> V
FB -> C
HO -> S
BV -> V
KS -> C
HB -> S
SO -> N
PH -> C
PN -> F
OC -> F
KO -> F
VF -> V
CS -> O
VK -> O
FV -> N
OO -> K
NS -> S
KK -> C
FO -> S
PV -> S
CN -> O
VC -> P
SS -> C
PO -> P
BN -> N
PB -> N
PC -> H
SH -> K
BH -> F
HK -> O
VB -> P
NV -> O
NB -> C
CP -> H
NO -> K
PP -> N
CC -> S
CB -> K
VH -> H
SC -> C
KC -> N
SB -> B
BP -> P
KP -> K
SN -> H
KF -> K
KH -> B
HV -> V
HS -> K
NF -> B
ON -> H
BO -> P
VO -> K
OH -> C
HF -> O
BK -> H
"""
