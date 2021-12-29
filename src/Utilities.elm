module Utilities exposing (maybeAll, unwrapMaybe, unwrapMaybeWithMessage, binaryToBase10)

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


unwrapMaybe : Maybe a -> a
unwrapMaybe maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.todo "Crashing due to unwrapped Maybe with Nothing value"


unwrapMaybeWithMessage : String -> Maybe a -> a
unwrapMaybeWithMessage message maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            let
                fullMessage =
                    "Crash due to unwrapped Maybe with Nothing value. Message: " ++ message
            in
            Debug.todo fullMessage


binaryToBase10 : List Char -> Int
binaryToBase10 bits =
    List.foldr
        (\bit acc ->
            if bit == '1' then
                { position = acc.position + 1, sum = acc.sum + 2 ^ acc.position }

            else
                { acc | position = acc.position + 1 }
        )
        { position = 0, sum = 0 }
        bits
        |> .sum
