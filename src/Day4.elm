module Day4 exposing (..)

import List.Extra
import Set exposing (..)
import Utilities exposing (maybeAll)



-------------------
-- PARSING
-------------------


type alias Draw =
    Int


type alias Draws =
    List Draw


type alias Board =
    List (List Int)


parseInput : String -> Maybe { draws : Draws, boards : List Board }
parseInput input =
    input
        |> String.trim
        |> String.split "\n\n"
        |> toDrawsAndBoards


toDrawsAndBoards : List String -> Maybe { draws : Draws, boards : List Board }
toDrawsAndBoards lines =
    case lines of
        [] ->
            Nothing

        lineWithDraws :: maybeBoardLines ->
            case maybeBoardLines of
                [] ->
                    Nothing

                boardLines ->
                    Maybe.map2
                        (\draws boards -> { draws = draws, boards = boards })
                        (parseDraws lineWithDraws)
                        (parseBoards boardLines)


{-| Parse a string of draws, e.g. "1,2,3,4"
-}
parseDraws : String -> Maybe Draws
parseDraws drawLine =
    drawLine
        |> String.split ","
        |> List.map String.toInt
        |> maybeAll


parseBoards : List String -> Maybe (List Board)
parseBoards boardLines =
    boardLines
        |> List.map parseBoardLines
        |> maybeAll


{-| Convert a string of 5 lines to a board
-}
parseBoardLines : String -> Maybe Board
parseBoardLines input =
    case String.split "\n" input of
        [ n1, n2, n3, n4, n5 ] ->
            [ n1, n2, n3, n4, n5 ]
                |> List.map stringToBoardLine
                |> maybeAll

        _ ->
            Nothing


stringToBoardLine : String -> Maybe (List Int)
stringToBoardLine string =
    String.split " " string
        |> List.filter (\element -> element /= "")
        |> List.map String.toInt
        |> maybeAll



-----------------------
-- PART 1
-----------------------


solvePart1 : { draws : Draws, boards : List Board } -> Maybe Int
solvePart1 input =
    input
        |> bingoHistory
        |> List.Extra.find (\( _, _, boards ) -> boards /= [])
        |> Maybe.andThen toFinalScore


solvePart2 : { draws : Draws, boards : List Board } -> Maybe Int
solvePart2 input =
    input
        |> bingoHistory
        |> List.reverse
        |> List.Extra.find (\( _, _, boards ) -> boards /= [])
        |> Maybe.andThen toFinalScore


{-| The BingoHistory is a list of tuples containing:
The current draw
The set of all draws so far in the game (including the current draw)
The list of solved boards after this draw
-}
type alias BingoHistory =
    List ( Draw, Set Draw, List Board )


type alias BoardSolvingAcc =
    { boardsWithoutBingo : List Board, draws : Set Draw, bingoHistory : BingoHistory }


{-| The idea here is to reduce over draws, keeping an accumulator containing boards without bingo,
the list of all draws so far, and the bingo history, which is a list of which boards got bingo at
which draw. We can use the bingo history to solve both part 1 and 2.
-}
bingoHistory : { draws : Draws, boards : List Board } -> BingoHistory
bingoHistory { draws, boards } =
    List.foldl
        boardSolveStep
        (BoardSolvingAcc boards Set.empty [])
        draws
        |> .bingoHistory
        |> List.reverse


boardSolveStep : Draw -> BoardSolvingAcc -> BoardSolvingAcc
boardSolveStep draw acc =
    let
        draws =
            Set.insert draw acc.draws

        ( boardsWithBingoThisRound, boardsWithoutBingoThisRound ) =
            List.partition (boardHasBingo draws) acc.boardsWithoutBingo

        bingoHistoryEntry =
            ( draw, draws, boardsWithBingoThisRound ) :: acc.bingoHistory
    in
    { boardsWithoutBingo = boardsWithoutBingoThisRound
    , draws = draws
    , bingoHistory = bingoHistoryEntry
    }


boardHasBingo : Set Draw -> Board -> Bool
boardHasBingo draws board =
    boardHasHorizontalBingo draws board || boardHasVerticalBingo draws board


boardHasHorizontalBingo : Set Draw -> Board -> Bool
boardHasHorizontalBingo draws board =
    let
        rowWithBingo =
            board
                |> List.Extra.find (\row -> List.all (\number -> Set.member number draws) row)
    in
    case rowWithBingo of
        Just _ ->
            True

        Nothing ->
            False


boardHasVerticalBingo : Set Draw -> Board -> Bool
boardHasVerticalBingo draws board =
    let
        rowWithBingo =
            board
                |> List.Extra.transpose
                |> List.Extra.find (\row -> List.all (\number -> Set.member number draws) row)
    in
    case rowWithBingo of
        Just _ ->
            True

        Nothing ->
            False


toFinalScore : ( Draw, Set Draw, List Board ) -> Maybe Int
toFinalScore ( draw, draws, boards ) =
    case boards of
        [ oneBoard ] ->
            Just (oneBoard |> sumUnmarkedNumbers draws |> (*) draw)

        _ ->
            Nothing


sumUnmarkedNumbers : Set Draw -> Board -> Int
sumUnmarkedNumbers draws board =
    board
        |> List.foldr (++) []
        |> List.filter (\number -> not (Set.member number draws))
        |> List.sum
