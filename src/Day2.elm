module Day2 exposing (..)

-------------------------------------------------------------------------
-- Part 1: What is the position of the boat after executing the commands?
-------------------------------------------------------------------------


type alias Position =
    { depth : Int
    , forward : Int
    }


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



-----------------------------------------------------------------------------------
-- Part 2: Calculate the boat position using the new interpretation of the commands
-----------------------------------------------------------------------------------


type alias PositionWithAim =
    { depth : Int
    , forward : Int
    , aim : Int
    }


part2Answer : List Command -> Int
part2Answer commands =
    let
        initial : PositionWithAim
        initial =
            PositionWithAim 0 0 0
    in
    List.foldl
        (\cmd acc ->
            case cmd of
                Forward amount ->
                    { acc | forward = acc.forward + amount, depth = acc.depth + acc.aim * amount }

                Down amount ->
                    { acc | aim = acc.aim + amount }

                Up amount ->
                    { acc | aim = acc.aim - amount }
        )
        initial
        commands
        |> multiplyPositionWithAim


multiplyPositionWithAim : PositionWithAim -> Int
multiplyPositionWithAim position =
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
            parseAmount Forward stringAmount

        [ "down", stringAmount ] ->
            parseAmount Down stringAmount

        [ "up", stringAmount ] ->
            parseAmount Up stringAmount

        _ ->
            Nothing


parseAmount : (Int -> Command) -> String -> Maybe Command
parseAmount cmdConstructor amountString =
    amountString
        |> String.toInt
        |> Maybe.andThen (\amount -> Just (cmdConstructor amount))
