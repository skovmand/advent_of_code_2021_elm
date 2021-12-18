module Day7 exposing (parseInput, solvePart1, solvePart2)

import Utilities exposing (maybeAll)


parseInput : String -> Maybe (List Int)
parseInput input =
    input
        |> String.split ","
        |> List.map String.toInt
        |> maybeAll


solvePart1 : List Int -> Maybe Int
solvePart1 input =
    calcFuelUsage plainDistance input


solvePart2 : List Int -> Maybe Int
solvePart2 input =
    calcFuelUsage newFuelFormula input


calcFuelUsage : (Int -> Int -> Int) -> List Int -> Maybe Int
calcFuelUsage fuelCalcFn input =
    let
        range =
            Maybe.map2 List.range (List.minimum input) (List.maximum input)
                |> Maybe.withDefault []
    in
    List.foldl
        (\pos acc ->
            let
                totalFuel =
                    List.foldl
                        (\submarinePos posAcc -> fuelCalcFn submarinePos pos :: posAcc)
                        []
                        input
                        |> List.sum
            in
            totalFuel :: acc
        )
        []
        range
        |> List.minimum


plainDistance : Int -> Int -> Int
plainDistance submarinePos pos =
    abs (submarinePos - pos)


newFuelFormula : Int -> Int -> Int
newFuelFormula submarinePos pos =
    plainDistance submarinePos pos
        |> (\dist -> (dist * dist + dist) // 2)
