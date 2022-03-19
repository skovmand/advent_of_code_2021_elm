module Day18Test exposing (suite)

import Day18 exposing (Snail(..), Type(..))
import Expect
import Test


suite : Test.Test
suite =
    Test.concat
        [ parserTests
        , flatParserTests
        , listToStringTest
        , explodeTest
        , splitTests
        , magnitudeTests
        , part1Example
        , part1Answer
        , part2Example
        , part2Answer
        ]


parserTests : Test.Test
parserTests =
    Test.describe "Day 18 Parsing"
        [ Test.test "Parses [[1,9],[8,5]]" <|
            \_ ->
                Day18.parseSnail "[[1,9],[8,5]]"
                    |> Expect.equal (Just (Pair (Pair (Number 1) (Number 9)) (Pair (Number 8) (Number 5))))
        ]


flatParserTests : Test.Test
flatParserTests =
    Test.describe "Day 18 Flat Parsing"
        [ Test.test "Parses [[1,9],[8,5]]" <|
            \_ ->
                Day18.flatParseSnail "[[1,9],[8,5]]"
                    |> Expect.equal
                        (Just
                            [ { level = 1, token = Opener }
                            , { level = 2, token = Opener }
                            , { level = 2, token = Value 1 }
                            , { level = 2, token = Comma }
                            , { level = 2, token = Value 9 }
                            , { level = 2, token = Closer }
                            , { level = 1, token = Comma }
                            , { level = 2, token = Opener }
                            , { level = 2, token = Value 8 }
                            , { level = 2, token = Comma }
                            , { level = 2, token = Value 5 }
                            , { level = 2, token = Closer }
                            , { level = 1, token = Closer }
                            ]
                        )
        ]


listToStringTest : Test.Test
listToStringTest =
    Test.describe "Token list to string"
        [ Test.test "[[1,9],[8,5]]" <|
            \() ->
                [ { level = 1, token = Opener }
                , { level = 2, token = Opener }
                , { level = 2, token = Value 1 }
                , { level = 2, token = Comma }
                , { level = 2, token = Value 9 }
                , { level = 2, token = Closer }
                , { level = 1, token = Comma }
                , { level = 2, token = Opener }
                , { level = 2, token = Value 8 }
                , { level = 2, token = Comma }
                , { level = 2, token = Value 5 }
                , { level = 2, token = Closer }
                , { level = 1, token = Closer }
                ]
                    |> Day18.tokenListToString
                    |> Expect.equal "[[1,9],[8,5]]"
        ]


explodeTest : Test.Test
explodeTest =
    Test.describe "Explode tests"
        [ Test.test "[[[[[9,8],1],2],3],4]" <|
            \() ->
                "[[[[[9,8],1],2],3],4]"
                    |> Day18.flatParseSnail
                    |> Maybe.andThen Day18.explode
                    -- [[[[0,9],2],3],4]
                    |> Expect.equal
                        (Just
                            [ { level = 1, token = Opener }
                            , { level = 2, token = Opener }
                            , { level = 3, token = Opener }
                            , { level = 4, token = Opener }
                            , { level = 4, token = Value 0 }
                            , { level = 4, token = Comma }
                            , { level = 4, token = Value 9 }
                            , { level = 4, token = Closer }
                            , { level = 3, token = Comma }
                            , { level = 3, token = Value 2 }
                            , { level = 3, token = Closer }
                            , { level = 2, token = Comma }
                            , { level = 2, token = Value 3 }
                            , { level = 2, token = Closer }
                            , { level = 1, token = Comma }
                            , { level = 1, token = Value 4 }
                            , { level = 1, token = Closer }
                            ]
                        )
        , Test.test "[7,[6,[5,[4,[3,2]]]]]" <|
            \() ->
                "[7,[6,[5,[4,[3,2]]]]]"
                    |> Day18.flatParseSnail
                    |> Maybe.andThen Day18.explode
                    -- "[7,[6,[5,[7,0]]]]"
                    |> Expect.equal
                        (Just
                            [ { level = 1, token = Opener }
                            , { level = 1, token = Value 7 }
                            , { level = 1, token = Comma }
                            , { level = 2, token = Opener }
                            , { level = 2, token = Value 6 }
                            , { level = 2, token = Comma }
                            , { level = 3, token = Opener }
                            , { level = 3, token = Value 5 }
                            , { level = 3, token = Comma }
                            , { level = 4, token = Opener }
                            , { level = 4, token = Value 7 }
                            , { level = 4, token = Comma }
                            , { level = 4, token = Value 0 }
                            , { level = 4, token = Closer }
                            , { level = 3, token = Closer }
                            , { level = 2, token = Closer }
                            , { level = 1, token = Closer }
                            ]
                        )
        , Test.test "[[6,[5,[4,[3,2]]]],1]" <|
            \() ->
                "[[6,[5,[4,[3,2]]]],1]"
                    |> Day18.flatParseSnail
                    |> Maybe.andThen Day18.explode
                    -- Just "[[6,[5,[7,0]]],3]"
                    |> Expect.equal
                        (Just
                            [ { level = 1, token = Opener }
                            , { level = 2, token = Opener }
                            , { level = 2, token = Value 6 }
                            , { level = 2, token = Comma }
                            , { level = 3, token = Opener }
                            , { level = 3, token = Value 5 }
                            , { level = 3, token = Comma }
                            , { level = 4, token = Opener }
                            , { level = 4, token = Value 7 }
                            , { level = 4, token = Comma }
                            , { level = 4, token = Value 0 }
                            , { level = 4, token = Closer }
                            , { level = 3, token = Closer }
                            , { level = 2, token = Closer }
                            , { level = 1, token = Comma }
                            , { level = 1, token = Value 3 }
                            , { level = 1, token = Closer }
                            ]
                        )
        , Test.test "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" <|
            \() ->
                "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
                    |> Day18.flatParseSnail
                    |> Maybe.andThen Day18.explode
                    -- "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
                    |> Expect.equal
                        (Just
                            [ { level = 1, token = Opener }
                            , { level = 2, token = Opener }
                            , { level = 2, token = Value 3 }
                            , { level = 2, token = Comma }
                            , { level = 3, token = Opener }
                            , { level = 3, token = Value 2 }
                            , { level = 3, token = Comma }
                            , { level = 4, token = Opener }
                            , { level = 4, token = Value 8 }
                            , { level = 4, token = Comma }
                            , { level = 4, token = Value 0 }
                            , { level = 4, token = Closer }
                            , { level = 3, token = Closer }
                            , { level = 2, token = Closer }
                            , { level = 1, token = Comma }
                            , { level = 2, token = Opener }
                            , { level = 2, token = Value 9 }
                            , { level = 2, token = Comma }
                            , { level = 3, token = Opener }
                            , { level = 3, token = Value 5 }
                            , { level = 3, token = Comma }
                            , { level = 4, token = Opener }
                            , { level = 4, token = Value 4 }
                            , { level = 4, token = Comma }
                            , { level = 5, token = Opener }
                            , { level = 5, token = Value 3 }
                            , { level = 5, token = Comma }
                            , { level = 5, token = Value 2 }
                            , { level = 5, token = Closer }
                            , { level = 4, token = Closer }
                            , { level = 3, token = Closer }
                            , { level = 2, token = Closer }
                            , { level = 1, token = Closer }
                            ]
                        )
        , Test.test "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" <|
            \() ->
                "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
                    |> Day18.flatParseSnail
                    |> Maybe.andThen Day18.explode
                    -- [[3,[2,[8,0]]],[9,[5,[7,0]]]]
                    |> Expect.equal
                        (Just
                            [ { level = 1, token = Opener }
                            , { level = 2, token = Opener }
                            , { level = 2, token = Value 3 }
                            , { level = 2, token = Comma }
                            , { level = 3, token = Opener }
                            , { level = 3, token = Value 2 }
                            , { level = 3, token = Comma }
                            , { level = 4, token = Opener }
                            , { level = 4, token = Value 8 }
                            , { level = 4, token = Comma }
                            , { level = 4, token = Value 0 }
                            , { level = 4, token = Closer }
                            , { level = 3, token = Closer }
                            , { level = 2, token = Closer }
                            , { level = 1, token = Comma }
                            , { level = 2, token = Opener }
                            , { level = 2, token = Value 9 }
                            , { level = 2, token = Comma }
                            , { level = 3, token = Opener }
                            , { level = 3, token = Value 5 }
                            , { level = 3, token = Comma }
                            , { level = 4, token = Opener }
                            , { level = 4, token = Value 7 }
                            , { level = 4, token = Comma }
                            , { level = 4, token = Value 0 }
                            , { level = 4, token = Closer }
                            , { level = 3, token = Closer }
                            , { level = 2, token = Closer }
                            , { level = 1, token = Closer }
                            ]
                        )
        ]


splitTests : Test.Test
splitTests =
    Test.describe "Splitting of snailfish"
        [ Test.test "[[[[0,7],4],[15,[0,13]]],[1,1]]" <|
            \() ->
                -- "[[[[0,7],4],[15,[0,13]]],[1,1]]" written as a flat list of tokens (because we can't represent 15 and 13 as single chars)
                [ { level = 1, token = Opener }, { level = 2, token = Opener }, { level = 3, token = Opener }, { level = 4, token = Opener }, { level = 4, token = Value 0 }, { level = 4, token = Comma }, { level = 4, token = Value 7 }, { level = 4, token = Closer }, { level = 3, token = Comma }, { level = 3, token = Value 4 }, { level = 3, token = Closer }, { level = 2, token = Comma }, { level = 3, token = Opener }, { level = 3, token = Value 15 }, { level = 3, token = Comma }, { level = 4, token = Opener }, { level = 4, token = Value 0 }, { level = 4, token = Comma }, { level = 4, token = Value 13 }, { level = 4, token = Closer }, { level = 3, token = Closer }, { level = 2, token = Closer }, { level = 1, token = Comma }, { level = 2, token = Opener }, { level = 2, token = Value 1 }, { level = 2, token = Comma }, { level = 2, token = Value 1 }, { level = 2, token = Closer }, { level = 1, token = Closer } ]
                    |> Day18.split
                    -- [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
                    |> Expect.equal
                        (Just
                            [ { level = 1, token = Opener }
                            , { level = 2, token = Opener }
                            , { level = 3, token = Opener }
                            , { level = 4, token = Opener }
                            , { level = 4, token = Value 0 }
                            , { level = 4, token = Comma }
                            , { level = 4, token = Value 7 }
                            , { level = 4, token = Closer }
                            , { level = 3, token = Comma }
                            , { level = 3, token = Value 4 }
                            , { level = 3, token = Closer }
                            , { level = 2, token = Comma }
                            , { level = 3, token = Opener }
                            , { level = 4, token = Opener }
                            , { level = 4, token = Value 7 }
                            , { level = 4, token = Comma }
                            , { level = 4, token = Value 8 }
                            , { level = 4, token = Closer }
                            , { level = 3, token = Comma }
                            , { level = 4, token = Opener }
                            , { level = 4, token = Value 0 }
                            , { level = 4, token = Comma }
                            , { level = 4, token = Value 13 }
                            , { level = 4, token = Closer }
                            , { level = 3, token = Closer }
                            , { level = 2, token = Closer }
                            , { level = 1, token = Comma }
                            , { level = 2, token = Opener }
                            , { level = 2, token = Value 1 }
                            , { level = 2, token = Comma }
                            , { level = 2, token = Value 1 }
                            , { level = 2, token = Closer }
                            , { level = 1, token = Closer }
                            ]
                        )
        , Test.test "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]" <|
            \() ->
                -- "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]" written as flat list of tokens
                [ { level = 1, token = Opener }, { level = 2, token = Opener }, { level = 3, token = Opener }, { level = 4, token = Opener }, { level = 4, token = Value 0 }, { level = 4, token = Comma }, { level = 4, token = Value 7 }, { level = 4, token = Closer }, { level = 3, token = Comma }, { level = 3, token = Value 4 }, { level = 3, token = Closer }, { level = 2, token = Comma }, { level = 3, token = Opener }, { level = 4, token = Opener }, { level = 4, token = Value 7 }, { level = 4, token = Comma }, { level = 4, token = Value 8 }, { level = 4, token = Closer }, { level = 3, token = Comma }, { level = 4, token = Opener }, { level = 4, token = Value 0 }, { level = 4, token = Comma }, { level = 4, token = Value 13 }, { level = 4, token = Closer }, { level = 3, token = Closer }, { level = 2, token = Closer }, { level = 1, token = Comma }, { level = 2, token = Opener }, { level = 2, token = Value 1 }, { level = 2, token = Comma }, { level = 2, token = Value 1 }, { level = 2, token = Closer }, { level = 1, token = Closer } ]
                    |> Day18.split
                    -- [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
                    |> Expect.equal
                        (Just
                            [ { level = 1, token = Opener }
                            , { level = 2, token = Opener }
                            , { level = 3, token = Opener }
                            , { level = 4, token = Opener }
                            , { level = 4, token = Value 0 }
                            , { level = 4, token = Comma }
                            , { level = 4, token = Value 7 }
                            , { level = 4, token = Closer }
                            , { level = 3, token = Comma }
                            , { level = 3, token = Value 4 }
                            , { level = 3, token = Closer }
                            , { level = 2, token = Comma }
                            , { level = 3, token = Opener }
                            , { level = 4, token = Opener }
                            , { level = 4, token = Value 7 }
                            , { level = 4, token = Comma }
                            , { level = 4, token = Value 8 }
                            , { level = 4, token = Closer }
                            , { level = 3, token = Comma }
                            , { level = 4, token = Opener }
                            , { level = 4, token = Value 0 }
                            , { level = 4, token = Comma }
                            , { level = 5, token = Opener }
                            , { level = 5, token = Value 6 }
                            , { level = 5, token = Comma }
                            , { level = 5, token = Value 7 }
                            , { level = 5, token = Closer }
                            , { level = 4, token = Closer }
                            , { level = 3, token = Closer }
                            , { level = 2, token = Closer }
                            , { level = 1, token = Comma }
                            , { level = 2, token = Opener }
                            , { level = 2, token = Value 1 }
                            , { level = 2, token = Comma }
                            , { level = 2, token = Value 1 }
                            , { level = 2, token = Closer }
                            , { level = 1, token = Closer }
                            ]
                        )
        ]


magnitudeTests : Test.Test
magnitudeTests =
    Test.describe "Magnitude tests"
        [ Test.test "[[9,1],[1,9]]" <|
            \_ ->
                "[[9,1],[1,9]]"
                    |> Day18.parseSnail
                    |> Maybe.map Day18.magnitude
                    |> Expect.equal (Just 129)
        ]


generalExampleInput : String
generalExampleInput =
    """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"""


part1Example : Test.Test
part1Example =
    Test.test "Part 1 example" <|
        \_ ->
            generalExampleInput
                |> Day18.flatParseInput
                |> Maybe.andThen Day18.solvePart1
                |> Expect.equal (Just 4140)


part1Answer : Test.Test
part1Answer =
    Test.test "Day 1 answer" <|
        \_ ->
            day18Input
                |> Day18.flatParseInput
                |> Maybe.andThen Day18.solvePart1
                |> Expect.equal (Just 3665)


part2Example : Test.Test
part2Example =
    Test.test "Part 2 example" <|
        \_ ->
            generalExampleInput
                |> Day18.flatParseInput
                |> Maybe.andThen Day18.solvePart2
                |> Expect.equal (Just 3993)


part2Answer : Test.Test
part2Answer =
    Test.test "Part 2 answer" <|
        \_ ->
            day18Input
                |> Day18.flatParseInput
                |> Maybe.andThen Day18.solvePart2
                |> Expect.equal (Just 4775)


day18Input : String
day18Input =
    """
[[[1,4],[1,6]],[0,[5,[6,3]]]]
[[[8,2],5],[[[9,8],[3,5]],[2,1]]]
[[[[6,2],[0,6]],[[9,8],[7,8]]],[[6,3],[[8,8],3]]]
[[[1,[2,1]],[5,7]],[[[3,1],[3,1]],[[8,4],[8,5]]]]
[[[[6,4],7],[[1,6],5]],[7,[9,5]]]
[[7,[[5,3],[0,9]]],[[[6,2],[6,8]],[5,[5,7]]]]
[9,5]
[[[[7,8],[8,0]],[[3,8],[0,7]]],[[1,[1,2]],2]]
[[[4,[5,5]],[[6,8],[4,3]]],[[9,9],[4,[3,6]]]]
[[[[2,8],7],[[6,1],[1,0]]],[[6,2],9]]
[[[8,1],3],[9,[[1,4],[4,1]]]]
[[[[0,1],[3,9]],[[4,3],6]],[[[4,8],8],[[8,3],[9,5]]]]
[[[[7,3],7],[[5,9],0]],7]
[[[5,[1,6]],3],[[3,5],9]]
[[[[2,5],[1,8]],[[6,5],[0,1]]],[[[4,1],1],[0,[9,6]]]]
[[[4,8],[[3,6],[3,8]]],[[[2,3],3],[[9,8],[7,9]]]]
[[[[5,6],0],[9,[4,4]]],[[[3,1],[3,6]],[[6,0],3]]]
[[[[4,3],4],4],[[[1,6],7],[8,[6,0]]]]
[[[0,2],1],5]
[[[[7,2],[9,0]],[8,[0,1]]],2]
[[[1,6],[[6,2],5]],[[1,[8,2]],[[9,8],7]]]
[[[8,1],9],[[[4,3],2],[[2,9],6]]]
[[[[9,4],0],[4,0]],4]
[[[5,[2,8]],[[5,3],[6,4]]],[8,3]]
[[0,5],[[[3,4],7],[[0,2],[9,1]]]]
[[[8,[7,9]],[[1,8],6]],[[4,[6,0]],0]]
[[[1,0],[[6,7],4]],[[[2,5],[9,7]],[[7,8],0]]]
[[9,[[7,1],3]],[[[9,2],[4,3]],[2,[1,8]]]]
[[[5,[9,6]],4],[1,[[9,2],[6,8]]]]
[6,[[[6,1],7],6]]
[[4,[[5,6],9]],[[9,[6,6]],[[6,1],[8,2]]]]
[[1,[9,5]],[[[5,8],9],5]]
[[[[6,6],[1,8]],6],[[[4,9],4],[8,[9,8]]]]
[[[[6,5],[4,4]],[[0,2],8]],[[[0,6],[4,5]],3]]
[[[1,[6,9]],[9,[5,8]]],[5,2]]
[[2,[[2,8],[3,3]]],[[[1,9],9],6]]
[[3,2],[9,[2,2]]]
[4,[3,[6,[2,0]]]]
[[[[1,0],4],3],[[0,9],[[9,8],[7,1]]]]
[[[2,6],[3,8]],[[5,5],[2,3]]]
[6,[[[8,8],4],[[8,1],[6,6]]]]
[[[5,9],[5,3]],7]
[[[5,[1,2]],[6,[7,2]]],[[[0,5],3],3]]
[[8,[[7,3],[9,7]]],[[2,[3,9]],[[1,7],[5,7]]]]
[8,[4,6]]
[[[4,4],[[4,5],[2,5]]],[[[9,1],0],[[2,9],1]]]
[[[2,[2,8]],9],[5,[6,9]]]
[[[[4,1],5],[6,[2,7]]],[1,2]]
[[[6,[3,5]],0],[[0,3],4]]
[[[[3,2],[8,0]],[5,1]],[[[9,7],3],[[6,5],[2,6]]]]
[[1,[0,[1,4]]],[[[8,6],[6,9]],[[4,9],8]]]
[[[[5,2],[4,3]],[0,[3,5]]],[0,[1,7]]]
[[[8,1],[3,[8,1]]],[[[7,9],[6,2]],[[0,8],2]]]
[[[2,[9,7]],[[6,6],[2,7]]],[[8,[6,4]],0]]
[[3,0],[[6,3],1]]
[[[[5,5],2],[9,7]],[[0,[3,5]],7]]
[[[[4,8],2],0],[[4,[7,9]],[6,6]]]
[[[1,0],[[9,4],[8,8]]],2]
[[[6,1],9],[5,2]]
[[[7,[0,3]],[[5,5],7]],[5,[[0,5],[5,3]]]]
[[[[8,0],4],[[5,5],[9,4]]],[[[9,0],[2,5]],[6,[8,1]]]]
[[[7,8],[0,[5,4]]],[[[7,6],[0,9]],[7,2]]]
[[[4,[0,2]],[3,[4,9]]],[[[4,7],8],3]]
[[1,[5,[7,3]]],8]
[[[[1,3],[6,8]],3],[[6,1],8]]
[[[[7,9],5],[[6,2],4]],[[5,[6,9]],1]]
[[2,[3,[9,3]]],[[6,[2,7]],[4,8]]]
[7,[[6,2],[[6,7],[5,0]]]]
[[[9,[8,6]],1],[[4,8],[[6,1],[0,1]]]]
[[[[4,6],[4,0]],[[2,4],0]],[[[0,5],[9,8]],[[3,4],[2,5]]]]
[9,[3,[[5,5],[3,1]]]]
[[[5,[7,1]],3],[[[8,2],5],[[2,8],[0,0]]]]
[[[[8,3],0],[[5,0],5]],[[3,[8,2]],[[8,2],3]]]
[[4,[[9,4],5]],[[[1,6],[0,2]],[0,8]]]
[[[0,0],[[1,8],2]],[[[1,8],1],[0,[0,8]]]]
[[[6,[1,5]],5],[[2,[0,1]],9]]
[[7,[2,[2,8]]],[4,[[1,1],5]]]
[1,[[4,[0,5]],4]]
[[3,[[3,1],[1,2]]],[[[5,3],8],[5,2]]]
[[[3,[2,0]],6],[[9,3],[[3,0],[1,6]]]]
[4,[[6,[5,9]],[[4,1],[6,6]]]]
[8,[3,0]]
[[[[5,3],[8,8]],[[5,1],4]],[[6,6],[8,2]]]
[[1,[[7,1],5]],[[[2,3],7],[[7,6],0]]]
[9,[[4,3],[[6,2],0]]]
[[[[4,0],4],[1,7]],[[[3,8],8],[[9,1],1]]]
[[[0,1],[9,9]],7]
[[[[1,7],0],[1,5]],[1,[2,2]]]
[[[[6,1],[3,3]],[6,[9,0]]],[[7,0],3]]
[[[[6,1],[9,8]],[[2,2],2]],[8,[3,6]]]
[[6,[5,0]],[7,[1,7]]]
[[4,[[6,1],6]],[[2,5],7]]
[8,[8,[[6,4],1]]]
[[[[0,2],4],[[2,6],2]],0]
[[2,[[6,1],9]],[[7,[0,5]],[5,[9,4]]]]
[3,[[8,7],[[8,9],6]]]
[[[[7,8],[1,1]],[[2,6],[3,7]]],4]
[[[[6,1],1],5],5]
[[9,[4,[6,6]]],[[5,1],[8,2]]]
[[5,[[7,3],4]],9]
"""
