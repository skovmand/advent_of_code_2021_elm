module Utilities exposing (maybeAll)

{-| Utilities reused in several puzzles
-}


{-| Are all elements in a list of the Just variant? If so, transform
it from List (Maybe a) to Maybe (List a).

maybeAll [Just 1, Just 2, Just 3] == Just ["1","2","3"]
maybeAll [Just 1, Just 2, Nothing] == Nothing

-}
maybeAll : List (Maybe a) -> Maybe (List a)
maybeAll list =
    maybeAllInner list []
        |> Maybe.map List.reverse


maybeAllInner : List (Maybe a) -> List a -> Maybe (List a)
maybeAllInner list acc =
    case list of
        [] ->
            Just acc

        (Just a) :: rest ->
            maybeAllInner rest (a :: acc)

        Nothing :: _ ->
            Nothing
