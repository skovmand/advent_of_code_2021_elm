module Day5 exposing (..)

{-| Day 5: Hydrothermal Venture
<https://adventofcode.com/2021/day/5>
-}

import Dict exposing (Dict)
import Utilities exposing (maybeAll)



-------------------
-- PARSING
-------------------


type alias Coordinate =
    ( Int, Int )


type alias CoordinateRange =
    ( Coordinate, Coordinate )


parseInput : String -> Maybe (List CoordinateRange)
parseInput input =
    input
        |> String.trim
        |> String.lines
        |> List.map parseCoordinateRanges
        |> maybeAll


parseCoordinateRanges : String -> Maybe CoordinateRange
parseCoordinateRanges input =
    case String.split " -> " input of
        [ startInput, endInput ] ->
            let
                startCoordinate =
                    parseCoordinate startInput

                endCoordinate =
                    parseCoordinate endInput
            in
            Maybe.map2 (\start end -> ( start, end )) startCoordinate endCoordinate

        _ ->
            Nothing


parseCoordinate : String -> Maybe Coordinate
parseCoordinate input =
    case String.split "," input of
        [ firstNumber, secondNumber ] ->
            Maybe.map2 (\first second -> ( first, second )) (String.toInt firstNumber) (String.toInt secondNumber)

        _ ->
            Nothing



-----------------------
-- PART 1 + 2
-----------------------


solvePart1 : List CoordinateRange -> Int
solvePart1 input =
    input
        |> List.filter (\range -> rangeType range /= Diagonal)
        |> solvePart2


solvePart2 : List CoordinateRange -> Int
solvePart2 input =
    input
        |> List.concatMap rangeToList
        |> applyCoordinates
        |> countCoordinatesWithTwoOrMore


type RangeType
    = Horizontal
    | Vertical
    | Diagonal


rangeType : CoordinateRange -> RangeType
rangeType ( ( ax, ay ), ( bx, by ) ) =
    if ax == bx then
        Vertical

    else if ay == by then
        Horizontal

    else
        Diagonal


rangeToList : CoordinateRange -> List Coordinate
rangeToList range =
    case rangeType range of
        Vertical ->
            unfoldVerticalRange range

        Horizontal ->
            unfoldHorizontalRange range

        Diagonal ->
            unfoldDiagonalRange range


unfoldVerticalRange : CoordinateRange -> List Coordinate
unfoldVerticalRange ( ( ax, ay ), ( _, by ) ) =
    List.range (min ay by) (max ay by)
        |> List.map (\y -> ( ax, y ))


unfoldHorizontalRange : CoordinateRange -> List Coordinate
unfoldHorizontalRange ( ( ax, ay ), ( bx, _ ) ) =
    List.range (min ax bx) (max ax bx)
        |> List.map (\x -> ( x, ay ))


unfoldDiagonalRange : CoordinateRange -> List Coordinate
unfoldDiagonalRange ( ( ax, ay ), ( bx, by ) ) =
    let
        xRange =
            if ax > bx then
                List.range bx ax
                    |> List.reverse

            else
                List.range ax bx

        yRange =
            if ay > by then
                List.range by ay
                    |> List.reverse

            else
                List.range ay by
    in
    List.map2 (\a b -> ( a, b )) xRange yRange


{-| Apply a list of coordinates to an empty dict
-}
applyCoordinates : List Coordinate -> Dict Coordinate Int
applyCoordinates coords =
    List.foldl
        (\coord dictAcc -> updateCoordinateCount coord dictAcc)
        Dict.empty
        coords


updateCoordinateCount : Coordinate -> Dict Coordinate Int -> Dict Coordinate Int
updateCoordinateCount coord dict =
    Dict.update coord
        (\c ->
            case c of
                Just count ->
                    Just (count + 1)

                Nothing ->
                    Just 1
        )
        dict


{-| Dict.foldl is faster than using List.filter over Dict.values
-}
countCoordinatesWithTwoOrMore : Dict Coordinate Int -> Int
countCoordinatesWithTwoOrMore dict =
    dict
        |> Dict.foldl
            (\_ val acc ->
                if val > 1 then
                    acc + 1

                else
                    acc
            )
            0
