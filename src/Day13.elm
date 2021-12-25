module Day13 exposing (parseInput, solvePart1, solvePart2)

{-| Day 13: Transparent Origami
-}

import Dict exposing (Dict)
import Set exposing (Set)
import Utilities exposing (maybeAll)



---------------
-- PARSE
---------------


type Instruction
    = FoldAlongY Int
    | FoldAlongX Int


type alias BoardState =
    { boardSize : ( Int, Int )
    , coords : Set ( Int, Int )
    }


type alias ParsedInput =
    { boardState : BoardState, instructions : List Instruction }


parseInput : String -> Maybe ParsedInput
parseInput input =
    let
        splitInput =
            input
                |> String.trim
                |> String.split "\n\n"
    in
    case splitInput of
        [ coords, instructions ] ->
            Maybe.map2
                ParsedInput
                (parseBoardState coords)
                (parseInstructions instructions)

        _ ->
            Nothing


parseBoardState : String -> Maybe BoardState
parseBoardState input =
    input
        |> String.lines
        |> List.map toCoordinate
        |> maybeAll
        |> Maybe.map Set.fromList
        |> Maybe.andThen withBoardSize


withBoardSize : Set ( Int, Int ) -> Maybe BoardState
withBoardSize coords =
    toBoardSize coords
        |> Maybe.map (\boardSize -> BoardState boardSize coords)


toBoardSize : Set ( Int, Int ) -> Maybe ( Int, Int )
toBoardSize coords =
    let
        maxX =
            coords |> Set.toList |> List.map (\( x, _ ) -> x) |> List.maximum

        maxY =
            coords |> Set.toList |> List.map (\( _, y ) -> y) |> List.maximum
    in
    Maybe.map2 Tuple.pair maxX maxY


toCoordinate : String -> Maybe ( Int, Int )
toCoordinate line =
    case line |> String.split "," |> List.map String.toInt |> maybeAll of
        Just [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing


parseInstructions : String -> Maybe (List Instruction)
parseInstructions input =
    input
        |> String.lines
        |> List.map parseInstruction
        |> maybeAll


parseInstruction : String -> Maybe Instruction
parseInstruction input =
    case input |> String.replace "fold along " "" |> String.split "=" of
        [ axis, coord ] ->
            String.toInt coord
                |> Maybe.andThen (parseAxis axis)

        _ ->
            Nothing


parseAxis : String -> Int -> Maybe Instruction
parseAxis axis coord =
    case axis of
        "x" ->
            Just (FoldAlongX coord)

        "y" ->
            Just (FoldAlongY coord)

        _ ->
            Nothing



-----------------------------
-- PART 1
-----------------------------


solvePart1 : ParsedInput -> Maybe Int
solvePart1 { instructions, boardState } =
    case instructions of
        instruction :: _ ->
            applyInstruction instruction boardState
                |> .coords
                |> Set.size
                |> Just

        _ ->
            Nothing


solvePart2 : ParsedInput -> Int
solvePart2 { instructions, boardState } =
    instructions
        |> List.foldl
            (\instruction boardStateAcc -> applyInstruction instruction boardStateAcc)
            boardState
        |> .coords
        |> writePaperToScreen
        |> (\_ -> 0)


applyInstruction : Instruction -> BoardState -> BoardState
applyInstruction instruction { boardSize, coords } =
    case instruction of
        FoldAlongX xCoord ->
            let
                -- partition coords into left and right part, removing coords on the fold line
                ( left, right ) =
                    Set.filter (\( x, _ ) -> x /= xCoord) coords
                        |> Set.partition (\( x, _ ) -> x < xCoord)

                -- transpose the right part so it starts at x=0
                transposedRight =
                    Set.map (\( x, y ) -> ( x - (xCoord + 1), y )) right

                newBoardSize =
                    ( xCoord, Tuple.second boardSize )
            in
            -- mirror the top part top to bottom, union it with the unmirrored bottom part
            -- then mirror the whole union
            mirrorLeftRight newBoardSize left
                |> Set.union transposedRight
                |> mirrorLeftRight newBoardSize
                |> (\flippedCoords -> { boardSize = newBoardSize, coords = flippedCoords })

        FoldAlongY yCoord ->
            let
                -- partition coords into top and bottom part, removing coords on the fold line
                ( top, bottom ) =
                    Set.filter (\( _, y ) -> y /= yCoord) coords
                        |> Set.partition (\( _, y ) -> y < yCoord)

                -- transpose the bottom part so it starts at y=0
                transposedBottom =
                    Set.map (\( x, y ) -> ( x, y - (yCoord + 1) )) bottom

                newBoardSize =
                    ( Tuple.first boardSize, yCoord )
            in
            -- mirror the top part top to bottom, union it with the unmirrored bottom part
            -- then mirror the whole union
            mirrorUpsideDown newBoardSize top
                |> Set.union transposedBottom
                |> mirrorUpsideDown newBoardSize
                |> (\flippedCoords -> { boardSize = newBoardSize, coords = flippedCoords })


{-| Mirror a board top-to-bottom, so the top row becomes the bottom row, and vice versa
-}
mirrorUpsideDown : ( Int, Int ) -> Set ( Int, Int ) -> Set ( Int, Int )
mirrorUpsideDown ( _, boardSizeY ) coords =
    Set.map (\( x, y ) -> ( x, (boardSizeY - 1) - y )) coords


{-| Mirror a board side-to-side, so the leftmost row becomes the rightmost row, and so on
-}
mirrorLeftRight : ( Int, Int ) -> Set ( Int, Int ) -> Set ( Int, Int )
mirrorLeftRight ( boardSizeX, _ ) coords =
    Set.map (\( x, y ) -> ( (boardSizeX - 1) - x, y )) coords



-----------------------------------
-- UTILITY: WRITE PAPER TO SCREEN
-----------------------------------


writePaperToScreen : Set ( Int, Int ) -> Set ( Int, Int )
writePaperToScreen coords =
    let
        ( x, y ) =
            toBoardSize coords |> Maybe.withDefault ( 0, 0 )

        dictWithEmptyFields =
            buildEmptyDict ( x, y )

        dictWithAddedDots =
            Set.foldl
                (\coord dict ->
                    Dict.insert coord "#" dict
                )
                dictWithEmptyFields
                coords
    in
    List.range 0 y
        |> List.foldl
            (\row acc ->
                let
                    _ =
                        Dict.filter
                            (\( _, localY ) _ -> localY == row)
                            dictWithAddedDots
                            |> Dict.toList
                            |> List.map (\( _, val ) -> val)
                            |> String.join ""
                            |> Debug.log "D13 OUTPUT"
                in
                acc
            )
            coords


buildEmptyDict : ( Int, Int ) -> Dict ( Int, Int ) String
buildEmptyDict ( x, y ) =
    let
        xRange =
            List.range 0 x

        yRange =
            List.range 0 y
    in
    List.foldl
        (\yValue outerDict ->
            List.foldl
                (\xValue innerDict ->
                    Dict.insert ( xValue, yValue ) "." innerDict
                )
                outerDict
                xRange
        )
        Dict.empty
        yRange
