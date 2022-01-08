module Day18 exposing (Snail(..), Token, Type(..), explode, flatParseInput, flatParseSnail, magnitude, parseSnail, solvePart1, split, tokenListToString)

{-| Day 18: Snailfish
<https://adventofcode.com/2021/day/18>
-}

import Parser exposing ((|.), (|=))
import Utilities exposing (maybeAll)



-------------------------------------------------------
-- Recursive parser, used for calculating magnitude
-------------------------------------------------------


type Snail
    = RegularNumber Int
    | Pair Snail Snail


parseSnail : String -> Maybe Snail
parseSnail line =
    Parser.run snailParser line
        |> Result.toMaybe


snailParser : Parser.Parser Snail
snailParser =
    Parser.oneOf
        [ snailRegularNumberParser
        , snailPairParser
        ]


snailRegularNumberParser : Parser.Parser Snail
snailRegularNumberParser =
    Parser.succeed RegularNumber
        |= Parser.int


snailPairParser : Parser.Parser Snail
snailPairParser =
    Parser.succeed Pair
        |. Parser.token "["
        |= Parser.lazy (\() -> snailParser)
        |. Parser.token ","
        |= Parser.lazy (\() -> snailParser)
        |. Parser.token "]"



---------------------------------------------------------------------------
-- Magnitude calculations, oh so nice when we have a recursive structure
---------------------------------------------------------------------------


magnitude : Snail -> Int
magnitude snail =
    case snail of
        Pair a b ->
            3 * magnitude a + 2 * magnitude b

        RegularNumber i ->
            i



-------------------------------------------------------
-- Flat parser, used for snailfish addition
-------------------------------------------------------


type alias Level =
    Int


type alias FlatSnail =
    List Token


type alias Token =
    { level : Int, token : Type }


type Type
    = Opener
    | Closer
    | Comma
    | Value Int


flatParseInput : String -> Maybe (List FlatSnail)
flatParseInput input =
    input
        |> String.trim
        |> String.lines
        |> List.map flatParseSnail
        |> maybeAll


flatParseSnail : String -> Maybe FlatSnail
flatParseSnail input =
    parseFlatSnailfish 0 (String.split "" input) []


parseFlatSnailfish : Level -> List String -> FlatSnail -> Maybe FlatSnail
parseFlatSnailfish currentLevel input acc =
    case input of
        [] ->
            Just (List.reverse acc)

        "[" :: rest ->
            parseFlatSnailfish (currentLevel + 1) rest ({ level = currentLevel + 1, token = Opener } :: acc)

        "]" :: rest ->
            parseFlatSnailfish (currentLevel - 1) rest ({ level = currentLevel, token = Closer } :: acc)

        "," :: rest ->
            parseFlatSnailfish currentLevel rest ({ level = currentLevel, token = Comma } :: acc)

        "0" :: rest ->
            parseFlatSnailfish currentLevel rest ({ level = currentLevel, token = Value 0 } :: acc)

        "1" :: rest ->
            parseFlatSnailfish currentLevel rest ({ level = currentLevel, token = Value 1 } :: acc)

        "2" :: rest ->
            parseFlatSnailfish currentLevel rest ({ level = currentLevel, token = Value 2 } :: acc)

        "3" :: rest ->
            parseFlatSnailfish currentLevel rest ({ level = currentLevel, token = Value 3 } :: acc)

        "4" :: rest ->
            parseFlatSnailfish currentLevel rest ({ level = currentLevel, token = Value 4 } :: acc)

        "5" :: rest ->
            parseFlatSnailfish currentLevel rest ({ level = currentLevel, token = Value 5 } :: acc)

        "6" :: rest ->
            parseFlatSnailfish currentLevel rest ({ level = currentLevel, token = Value 6 } :: acc)

        "7" :: rest ->
            parseFlatSnailfish currentLevel rest ({ level = currentLevel, token = Value 7 } :: acc)

        "8" :: rest ->
            parseFlatSnailfish currentLevel rest ({ level = currentLevel, token = Value 8 } :: acc)

        "9" :: rest ->
            parseFlatSnailfish currentLevel rest ({ level = currentLevel, token = Value 9 } :: acc)

        _ ->
            Nothing


tokenListToString : FlatSnail -> String
tokenListToString tokens =
    tokens
        |> List.map
            (\{ token } ->
                case token of
                    Opener ->
                        "["

                    Closer ->
                        "]"

                    Comma ->
                        ","

                    Value v ->
                        String.fromInt v
            )
        |> String.concat



-----------------------------
-- PART 1
-----------------------------


solvePart1 : List FlatSnail -> Maybe Int
solvePart1 input =
    addAllSnailFishes input
        |> Maybe.map tokenListToString
        |> Maybe.andThen parseSnail
        |> Maybe.map magnitude


addAllSnailFishes : List FlatSnail -> Maybe FlatSnail
addAllSnailFishes snailFishes =
    case snailFishes of
        head :: rest ->
            List.foldl (\snailFish acc -> addSnailFishes acc snailFish |> reduceSnailFish) head rest
                |> Just

        _ ->
            Nothing



-------------------------------
-- Addition of two snailfishes
-------------------------------


addSnailFishes : FlatSnail -> FlatSnail -> FlatSnail
addSnailFishes fish1 fish2 =
    { level = 1, token = Opener }
        :: mapListLevel 1 fish1
        ++ List.singleton { level = 1, token = Comma }
        ++ mapListLevel 1 fish2
        ++ List.singleton { level = 1, token = Closer }


mapListLevel : Int -> FlatSnail -> FlatSnail
mapListLevel amount tokenList =
    List.map (\token -> { token | level = token.level + amount }) tokenList



----------------------------------------------------
-- Reduction of a snailfish using explode and split
----------------------------------------------------


{-| Reduce a snailfish until it cannot be reduced any further
-}
reduceSnailFish : FlatSnail -> FlatSnail
reduceSnailFish inputSnailFish =
    case explode inputSnailFish of
        Just explodedSnailfish ->
            reduceSnailFish explodedSnailfish

        Nothing ->
            case split inputSnailFish of
                Just splitSnailfish ->
                    reduceSnailFish splitSnailfish

                Nothing ->
                    inputSnailFish



------------
-- Explode
------------


explode : FlatSnail -> Maybe FlatSnail
explode snailfish =
    explodeBeforeAfter snailfish []
        |> Maybe.map
            (\explosion ->
                let
                    leftList =
                        addValueToList explosion.leftNumber explosion.leftListReversed
                            |> List.reverse

                    rightList =
                        addValueToList explosion.rightNumber explosion.rightList
                in
                leftList ++ (explosion.replacement :: rightList)
            )


explodeBeforeAfter : FlatSnail -> FlatSnail -> Maybe { leftListReversed : FlatSnail, rightList : FlatSnail, leftNumber : Int, rightNumber : Int, replacement : Token }
explodeBeforeAfter snailfish leftList =
    case snailfish of
        a :: b :: c :: d :: e :: rest ->
            let
                levels =
                    List.map .level [ a, b, c, d, e ]
            in
            if List.all (\elem -> elem > 4) levels && c.token == Comma then
                case ( b.token, c.token, d.token ) of
                    ( Value vb, Comma, Value vd ) ->
                        Just { leftListReversed = leftList, rightList = rest, leftNumber = vb, rightNumber = vd, replacement = { level = a.level - 1, token = Value 0 } }

                    _ ->
                        explodeBeforeAfter (b :: c :: d :: e :: rest) (a :: leftList)

            else
                explodeBeforeAfter (b :: c :: d :: e :: rest) (a :: leftList)

        a :: rest ->
            explodeBeforeAfter rest (a :: leftList)

        [] ->
            Nothing


addValueToList : Int -> FlatSnail -> FlatSnail
addValueToList value list =
    addValueToListHelp value list []


addValueToListHelp : Int -> FlatSnail -> FlatSnail -> FlatSnail
addValueToListHelp value list acc =
    case list of
        [] ->
            List.reverse acc

        v :: rest ->
            case v.token of
                Value i ->
                    List.reverse acc ++ ({ level = v.level, token = Value (value + i) } :: rest)

                _ ->
                    addValueToListHelp value rest (v :: acc)



----------
-- Split
----------


split : FlatSnail -> Maybe FlatSnail
split list =
    splitListValue list []


splitListValue : FlatSnail -> FlatSnail -> Maybe FlatSnail
splitListValue list acc =
    case list of
        [] ->
            Nothing

        v :: rest ->
            case v.token of
                Value i ->
                    let
                        leftValue =
                            i // 2

                        rightValue =
                            ceiling (toFloat i / 2.0)
                    in
                    if i > 9 then
                        let
                            replacement =
                                [ { level = v.level + 1, token = Opener }
                                , { level = v.level + 1, token = Value leftValue }
                                , { level = v.level + 1, token = Comma }
                                , { level = v.level + 1, token = Value rightValue }
                                , { level = v.level + 1, token = Closer }
                                ]
                        in
                        Just (List.reverse acc ++ replacement ++ rest)

                    else
                        splitListValue rest (v :: acc)

                _ ->
                    splitListValue rest (v :: acc)
