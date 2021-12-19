module DigitDict exposing (..)

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


insert : Set String -> Int -> DigitDict -> DigitDict
insert set value dict =
    ( set, value ) :: dict


get : Set String -> DigitDict -> Maybe Int
get set digitDict =
    List.Extra.find
        (\( entrySet, _ ) -> entrySet == set)
        digitDict
        |> Maybe.map (\( _, int ) -> int)
