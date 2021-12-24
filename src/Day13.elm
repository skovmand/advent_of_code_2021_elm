module Day13 exposing (parseInput, solvePart1)

{-| Day 13: Transparent Origami
-}

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


applyInstruction : Instruction -> BoardState -> BoardState
applyInstruction instruction { boardSize, coords } =
    case instruction of
        FoldAlongX coord ->
            let
                ( left, right ) =
                    Set.partition
                        (\( x, _ ) -> x < coord)
                        coords
            in
            right
                |> flipVertical boardSize coord
                |> (\boardState -> { boardState | coords = Set.union left boardState.coords })

        FoldAlongY coord ->
            let
                ( top, bottom ) =
                    Set.partition
                        (\( _, y ) -> y < coord)
                        coords
            in
            bottom
                |> flipHorizontal boardSize coord
                |> (\boardState -> { boardState | coords = Set.union top boardState.coords })


flipVertical : ( Int, Int ) -> Int -> Set ( Int, Int ) -> BoardState
flipVertical ( boardSizeX, boardSizeY ) flipAtX coords =
    let
        newBoardSize =
            ( flipAtX, boardSizeY )
    in
    Set.map
        (transposeAlongVerticalAxis boardSizeX flipAtX)
        coords
        |> BoardState newBoardSize


transposeAlongVerticalAxis : Int -> Int -> ( Int, Int ) -> ( Int, Int )
transposeAlongVerticalAxis boardSizeX flipAtX ( x, y ) =
    let
        remainingRows =
            boardSizeX - 1 - flipAtX

        transposedToOrigin0 =
            x - (flipAtX + 1)

        mirroredXCoord =
            remainingRows - transposedToOrigin0
    in
    ( mirroredXCoord, y )


flipHorizontal : ( Int, Int ) -> Int -> Set ( Int, Int ) -> BoardState
flipHorizontal ( boardSizeX, boardSizeY ) flipAtY coords =
    let
        newBoardSize =
            ( boardSizeX, flipAtY )
    in
    Set.map
        (transposeAlongHorizontalAxis boardSizeY flipAtY)
        coords
        |> BoardState newBoardSize


transposeAlongHorizontalAxis : Int -> Int -> ( Int, Int ) -> ( Int, Int )
transposeAlongHorizontalAxis boardSizeY flipAtY ( x, y ) =
    let
        remainingRows =
            boardSizeY - 1 - flipAtY

        transposedToOrigin0 =
            y - (flipAtY + 1)

        mirroredYCoord =
            remainingRows - transposedToOrigin0
    in
    ( x, mirroredYCoord )
