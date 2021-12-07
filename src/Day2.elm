module Day2 exposing (..)


type alias Position =
    { depth : Int
    , forward : Int
    }



-------------------------------------------------------------------------
-- Part 1: What is the position of the boat after executing the commands?
-------------------------------------------------------------------------


part1Answer : List Command -> Int
part1Answer commands =
    let
        initial : Position
        initial =
            Position 0 0
    in
    List.foldl
        (\cmd acc ->
            case cmd of
                Forward amount ->
                    { acc | forward = acc.forward + amount }

                Down amount ->
                    { acc | depth = acc.depth + amount }

                Up amount ->
                    { acc | depth = acc.depth - amount }
        )
        initial
        commands
        |> multiplyPositions


multiplyPositions : Position -> Int
multiplyPositions position =
    position.depth * position.forward


parseInput : String -> List Command
parseInput input =
    input
        |> String.lines
        |> List.filterMap toCommand


type Command
    = Forward Int
    | Down Int
    | Up Int


toCommand : String -> Maybe Command
toCommand string =
    case String.split " " string of
        [ "forward", stringAmount ] ->
            stringAmount
                |> String.toInt
                |> Maybe.andThen (\amount -> Just (Forward amount))

        [ "down", stringAmount ] ->
            stringAmount
                |> String.toInt
                |> Maybe.andThen (\amount -> Just (Down amount))

        [ "up", stringAmount ] ->
            stringAmount
                |> String.toInt
                |> Maybe.andThen (\amount -> Just (Up amount))

        _ ->
            Nothing
