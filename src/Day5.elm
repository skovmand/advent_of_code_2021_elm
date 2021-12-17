module Day5 exposing (..)

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
-- PART 1
-----------------------


solvePart1 : List CoordinateRange -> Int
solvePart1 input =
    input
        |> List.filterMap rangeToList
        |> List.concat
        |> applyCoordinates
        |> countCoordinatesWithTwoOrMore


type RangeType
    = Horizontal
    | Vertical
    | Other


rangeType : CoordinateRange -> RangeType
rangeType ( ( ax, ay ), ( bx, by ) ) =
    if ax == bx then
        Vertical

    else if ay == by then
        Horizontal

    else
        Other


rangeToList : CoordinateRange -> Maybe (List Coordinate)
rangeToList range =
    case rangeType range of
        Vertical ->
            Just (unfoldVerticalRange range)

        Horizontal ->
            Just (unfoldHorizontalRange range)

        Other ->
            Nothing


unfoldVerticalRange : CoordinateRange -> List Coordinate
unfoldVerticalRange ( ( ax, ay ), ( _, by ) ) =
    List.range ( min ay by ) ( max ay by )
    |> List.map (\y -> ( ax, y ))


unfoldHorizontalRange : CoordinateRange -> List Coordinate
unfoldHorizontalRange ( ( ax, ay ), ( bx, _ ) ) =
    List.range ( min ax bx ) ( max ax bx )
    |> List.map (\x -> ( x, ay ))


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


countCoordinatesWithTwoOrMore : Dict Coordinate Int -> Int
countCoordinatesWithTwoOrMore dict =
    dict
        |> Dict.values
        |> List.filter (\v -> v > 1)
        |> List.length
