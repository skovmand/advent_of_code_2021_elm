module DigitDict exposing (DigitDict, get, getSetByValue, insert, new)

{-| A data structure to store and look up digits by a set of characters.
Helpers for day 8 part 2.
-}

import List.Extra
import Set exposing (Set)


type alias DigitDict =
    List ( Set String, Int )


{-| Make a new digit set
-}
new : DigitDict
new =
    []


get : Set String -> DigitDict -> Maybe Int
get set digitDict =
    List.Extra.find
        (\( entrySet, _ ) -> entrySet == set)
        digitDict
        |> Maybe.map (\( _, int ) -> int)


getSetByValue : Int -> DigitDict -> Maybe (Set String)
getSetByValue value digitDict =
    List.Extra.find
        (\( _, entryValue ) -> entryValue == value)
        digitDict
        |> Maybe.map (\( set, _ ) -> set)


insert : Set String -> Int -> DigitDict -> DigitDict
insert set value dict =
    ( set, value ) :: dict
