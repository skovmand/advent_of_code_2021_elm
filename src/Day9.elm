module Day9 exposing (parseInput, solvePart1, solvePart2)

import Dict exposing (Dict)
import Set exposing (Set)
import Utilities exposing (maybeAll)


parseInput : String -> Maybe (Dict ( Int, Int ) Int)
parseInput input =
    input
        |> String.trim
        |> String.lines
        |> List.map toIntegers
        |> maybeAll
        |> Maybe.map fillDict


toIntegers : String -> Maybe (List Int)
toIntegers line =
    line
        |> String.split ""
        |> List.map String.toInt
        |> maybeAll


fillDict : List (List Int) -> Dict ( Int, Int ) Int
fillDict intLines =
    List.indexedMap Tuple.pair intLines
        |> List.concatMap
            (\( y, line ) ->
                List.indexedMap (\x point -> ( x, y, point )) line
            )
        |> List.foldl
            (\( x, y, point ) dictAcc -> Dict.insert ( x, y ) point dictAcc)
            Dict.empty


solvePart1 : Dict ( Int, Int ) Int -> Maybe Int
solvePart1 dict =
    let
        lowPoints =
            findLowPoints dict
    in
    lowPoints
        |> List.map (\point -> Dict.get point dict)
        |> maybeAll
        |> Maybe.map (List.map ((+) 1))
        |> Maybe.map List.sum


solvePart2 : Dict ( Int, Int ) Int -> Maybe Int
solvePart2 dict =
    let
        lowPoints =
            findLowPoints dict
    in
    lowPoints
        |> List.map (expandPool dict)
        |> List.map Set.size
        |> List.sort
        |> List.reverse
        |> (\list ->
                case list of
                    a :: b :: c :: _ ->
                        Just (a * b * c)

                    _ ->
                        Nothing
           )


findLowPoints : Dict ( Int, Int ) Int -> List ( Int, Int )
findLowPoints dict =
    Dict.foldl
        (\coord value lowPoints ->
            let
                minAdjacentHeight =
                    adjacentHeights coord dict
                        |> List.minimum
                        -- For convenience, should never happen
                        |> Maybe.withDefault -1
            in
            if value < minAdjacentHeight then
                coord :: lowPoints

            else
                lowPoints
        )
        []
        dict


adjacentHeights : ( Int, Int ) -> Dict ( Int, Int ) Int -> List Int
adjacentHeights ( x, y ) dict =
    [ ( x + 1, y ), ( x - 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
        |> List.filterMap (\coord -> Dict.get coord dict)


expandPool : Dict ( Int, Int ) Int -> ( Int, Int ) -> Set ( Int, Int )
expandPool allPoints startPoint =
    expandStep allPoints (Set.fromList [ startPoint ]) startPoint


type Direction
    = Up
    | Down
    | Left
    | Right


expandStep : Dict ( Int, Int ) Int -> Set ( Int, Int ) -> ( Int, Int ) -> Set ( Int, Int )
expandStep allPoints pointSet currentPoint =
    [ Up, Left, Down, Right ]
        |> List.foldl
            (\direction poolPointAcc ->
                let
                    nextPoint_ =
                        nextPoint allPoints direction currentPoint
                            |> nothingIf9 allPoints
                            |> nothingIfAlreadyInSet poolPointAcc

                    updatedAcc =
                        case nextPoint_ of
                            Just point ->
                                Set.insert point poolPointAcc

                            Nothing ->
                                poolPointAcc
                in
                case nextPoint_ of
                    Just coord ->
                        expandStep allPoints updatedAcc coord

                    Nothing ->
                        updatedAcc
            )
            pointSet


{-| If a coordinate has value 9, treat is as Nothing
-}
nothingIf9 : Dict ( Int, Int ) Int -> Maybe ( Int, Int ) -> Maybe ( Int, Int )
nothingIf9 allPoints coordinate =
    coordinate
        -- TODO: Dict.get -> Dict.member
        |> Maybe.andThen (\point -> Dict.get point allPoints)
        |> Maybe.andThen
            (\value ->
                case value of
                    9 ->
                        Nothing

                    _ ->
                        coordinate
            )


nothingIfAlreadyInSet : Set ( Int, Int ) -> Maybe ( Int, Int ) -> Maybe ( Int, Int )
nothingIfAlreadyInSet setOfPoints coordinate =
    coordinate
        |> Maybe.andThen
            (\point ->
                if Set.member point setOfPoints then
                    Nothing

                else
                    coordinate
            )


{-| Given a currentPoint and a direction, return the next point coordinates or Nothing.
-}
nextPoint : Dict ( Int, Int ) Int -> Direction -> ( Int, Int ) -> Maybe ( Int, Int )
nextPoint allPoints direction currentPoint =
    let
        next =
            calcNextPoint direction currentPoint
    in
    if Dict.member next allPoints then
        Just next

    else
        Nothing


calcNextPoint : Direction -> ( Int, Int ) -> ( Int, Int )
calcNextPoint direction currentPoint =
    case direction of
        Up ->
            Tuple.mapSecond (\y -> y - 1) currentPoint

        Down ->
            Tuple.mapSecond (\y -> y + 1) currentPoint

        Left ->
            Tuple.mapFirst (\x -> x - 1) currentPoint

        Right ->
            Tuple.mapFirst (\x -> x + 1) currentPoint
