module Day1 exposing (parseInput, solvePart1, solvePart2)


parseInput : String -> List Int
parseInput input =
    input
        |> String.lines
        |> List.filterMap String.toInt


type Change
    = Increase
    | Decrease
    | Unchanged


change : ( Int, Int ) -> Change
change ( first, second ) =
    if first > second then
        Decrease

    else if first < second then
        Increase

    else
        Unchanged


pairs : List Int -> List ( Int, Int )
pairs input =
    List.map2 Tuple.pair
        input
        (List.drop 1 input)


solvePart1 : List Int -> Int
solvePart1 input =
    input
        |> pairs
        |> List.map change
        |> List.filter (\elem -> elem == Increase)
        |> List.length


triples : List Int -> List ( Int, Int, Int )
triples input =
    List.map3 (\num1 num2 num3 -> ( num1, num2, num3 ))
        input
        (List.drop 1 input)
        (List.drop 2 input)


sumTriple : ( Int, Int, Int ) -> Int
sumTriple ( num1, num2, num3 ) =
    num1 + num2 + num3


solvePart2 : List Int -> Int
solvePart2 input =
    input
        |> triples
        |> List.map sumTriple
        |> solvePart1
