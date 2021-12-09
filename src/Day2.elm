module Day2 exposing (parseInput, solvePart1, solvePart2)

-------------------------------------------------------------------------
-- Part 1: What is the position of the boat after executing the commands?
-------------------------------------------------------------------------


solvePart1 : List Command -> Int
solvePart1 commands =
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
        { depth = 0, forward = 0 }
        commands
        |> multiplyPositions


multiplyPositions : { depth : Int, forward : Int } -> Int
multiplyPositions position =
    position.depth * position.forward



-----------------------------------------------------------------------------------
-- Part 2: Calculate the boat position using the new interpretation of the commands
-----------------------------------------------------------------------------------


solvePart2 : List Command -> Int
solvePart2 commands =
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
        { depth = 0, forward = 0, aim = 0 }
        commands
        |> multiplyPositionWithAim


multiplyPositionWithAim : { depth : Int, forward : Int, aim : Int } -> Int
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
        |> Maybe.map cmdConstructor
