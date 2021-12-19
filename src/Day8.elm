module Day8 exposing (parseInput, solvePart1, solvePart2)

import DigitDict exposing (..)
import List.Extra
import Set exposing (Set)
import Utilities exposing (maybeAll)


parseInput : String -> Maybe (List ( List String, List String ))
parseInput input =
    input
        |> String.trim
        |> String.lines
        |> List.map (String.split " | ")
        |> List.map parseLine
        |> maybeAll


parseLine : List String -> Maybe ( List String, List String )
parseLine splitLine =
    case splitLine of
        [ patterns, digits ] ->
            let
                patternsList =
                    String.split " " patterns

                digitsList =
                    String.split " " digits
            in
            case ( List.length patternsList, List.length digitsList ) of
                ( 10, 4 ) ->
                    Just ( patternsList, digitsList )

                _ ->
                    Nothing

        _ ->
            Nothing


solvePart1 : List ( List String, List String ) -> Int
solvePart1 input =
    input
        |> List.concatMap (\( _, digits ) -> digits)
        |> List.filter (\digit -> List.member (String.length digit) [ 2, 3, 4, 7 ])
        |> List.length


solvePart2 : List ( List String, List String ) -> Maybe Int
solvePart2 input =
    input
        |> List.map deduceOutputValue
        |> maybeAll
        |> Maybe.map List.sum


deduceOutputValue : ( List String, List String ) -> Maybe Int
deduceOutputValue ( patterns, digits ) =
    let
        patternSets =
            List.map
                (\pattern ->
                    String.split "" pattern
                        |> Set.fromList
                )
                patterns

        digitSets =
            List.map
                (\digitSet ->
                    String.split "" digitSet
                        |> Set.fromList
                )
                digits
    in
    DigitDict.new
        |> add1478 patternSets
        |> Maybe.andThen (add069 patternSets)
        |> Maybe.andThen (add235 patternSets)
        |> Maybe.andThen (calculateOutputValue digitSets)


{-| Get 1, 4, 7, 8 from the input (uniquely given by their length of segments)
-}
add1478 : List (Set String) -> DigitDict -> Maybe DigitDict
add1478 patternSets dict =
    let
        maybe1 =
            List.Extra.find (\pattern -> Set.size pattern == 2) patternSets

        maybe4 =
            List.Extra.find (\pattern -> Set.size pattern == 4) patternSets

        maybe7 =
            List.Extra.find (\pattern -> Set.size pattern == 3) patternSets

        maybe8 =
            List.Extra.find (\pattern -> Set.size pattern == 7) patternSets
    in
    Maybe.map4
        (\one four seven eight ->
            dict
                |> DigitDict.insert one 1
                |> DigitDict.insert four 4
                |> DigitDict.insert seven 7
                |> DigitDict.insert eight 8
        )
        maybe1
        maybe4
        maybe7
        maybe8


add069 : List (Set String) -> DigitDict -> Maybe DigitDict
add069 patternSets dict =
    let
        setsWithLength6 =
            List.filter (\set -> Set.size set == 6)
                patternSets

        -- The only other digit containing all segments from 4 is 9
        maybe9 =
            DigitDict.getSetByValue 4 dict
                |> Maybe.map
                    (\four -> List.filter (\set -> Set.diff four set == Set.empty) setsWithLength6)
                |> Maybe.andThen List.head

        maybe0 =
            maybe9
                -- Get all sets that are not 9
                |> Maybe.map (\nineSet -> List.filter (\set -> set /= nineSet) setsWithLength6)
                -- Get the characters for 1, pass data in tuple
                |> Maybe.map2 (\oneSet setsWithLength6Without9 -> ( oneSet, setsWithLength6Without9 )) (DigitDict.getSetByValue 1 dict)
                -- The only digit left containing all segments from 1 is 0
                |> Maybe.map
                    (\( oneSet, setsWithLength6Without9 ) ->
                        List.filter (\set -> Set.diff oneSet set == Set.empty) setsWithLength6Without9
                    )
                |> Maybe.andThen List.head

        maybe6 =
            Maybe.map2 (\nineSet zeroSet -> ( nineSet, zeroSet )) maybe9 maybe0
                |> Maybe.map
                    (\( nineSet, zeroSet ) ->
                        List.filter (\set -> set /= nineSet && set /= zeroSet) setsWithLength6
                    )
                |> Maybe.andThen List.head
    in
    Maybe.map3
        (\zero six nine ->
            dict
                |> DigitDict.insert zero 0
                |> DigitDict.insert six 6
                |> DigitDict.insert nine 9
        )
        maybe0
        maybe6
        maybe9


add235 : List (Set String) -> DigitDict -> Maybe DigitDict
add235 patternSets dict =
    let
        setsWithLength5 =
            List.filter (\set -> Set.size set == 5)
                patternSets

        -- The only other digit containing all segments from 1 is 3
        maybe3 =
            DigitDict.getSetByValue 1 dict
                |> Maybe.map
                    (\oneSet -> List.filter (\set -> Set.diff oneSet set == Set.empty) setsWithLength5)
                |> Maybe.andThen List.head

        -- The set that equals 9 when unioned with 7, is 5
        maybe5 =
            [ 7, 9 ]
                |> List.map (\val -> DigitDict.getSetByValue val dict)
                |> maybeAll
                |> Maybe.andThen
                    (\list ->
                        case list of
                            [ a, b ] ->
                                Just ( a, b )

                            _ ->
                                Nothing
                    )
                |> Maybe.map
                    (\( sevenSet, nineSet ) -> List.filter (\set -> Set.union set sevenSet == nineSet) setsWithLength5)
                |> Maybe.andThen List.head

        maybe2 =
            Maybe.map2 (\set3 set5 -> ( set3, set5 )) maybe3 maybe5
                |> Maybe.map
                    (\( set3, set5 ) ->
                        List.filter (\set -> set /= set3 && set /= set5) setsWithLength5
                    )
                |> Maybe.andThen List.head
    in
    Maybe.map3
        (\set2 set3 set5 ->
            dict
                |> DigitDict.insert set2 2
                |> DigitDict.insert set3 3
                |> DigitDict.insert set5 5
        )
        maybe2
        maybe3
        maybe5


{-| Transform the scrambled outputs to an integer
-}
calculateOutputValue : List (Set String) -> DigitDict -> Maybe Int
calculateOutputValue digits dict =
    digits
        |> List.map (\digit -> DigitDict.get digit dict)
        |> maybeAll
        |> Maybe.map listOfIntsToInt


listOfIntsToInt : List Int -> Int
listOfIntsToInt list =
    List.foldr
        (\digit ( sum, factor ) ->
            ( sum + digit * factor, factor * 10 )
        )
        ( 0, 1 )
        list
        |> (\( sum, _ ) -> sum)
