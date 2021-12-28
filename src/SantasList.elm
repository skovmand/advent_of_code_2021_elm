module SantasList exposing (last, maybeListOf2)

{-| Custom List functions
-}


{-| Get the last element of a list
-}
last : List a -> Maybe a
last list =
    case list of
        [] ->
            Nothing

        [ value ] ->
            Just value

        _ :: rest ->
            last rest


{-| Utility for converting a list of 2 elements to a Maybe 2-tuple
-}
maybeListOf2 : Maybe (List a) -> Maybe ( a, a )
maybeListOf2 list =
    case list of
        Just [ a, b ] ->
            Just ( a, b )

        _ ->
            Nothing
