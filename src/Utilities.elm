module Utilities exposing (maybeMap)

{-| Utilities reused in several puzzles
-}


{-| Given a function with signature (a -> Maybe b) and a list of things to parse,
attempt to parse all elements in the list, and return a result as Maybe (List b),
with Maybe on the outside. Useful to see if all parse results were successful or not.

For example:
maybeMap String.toInt ["1", "2", "3"] == Just (1, 2, 3)
maybeMap String.toInt ["HEJ", "2", "3"] == Nothing

Or, given a list of maybes, use it to put the Maybe on the outside:
maybeMap identity [Just 1, Just 2, Just 3] == Just ["1","2","3"]
maybeMap identity [Just 1, Just 2, Nothing] == Nothing

-}
maybeMap : (a -> Maybe b) -> List a -> Maybe (List b)
maybeMap parseFn list =
    List.foldr (maybeMapStep parseFn) (Just []) list


maybeMapStep : (a -> Maybe b) -> a -> Maybe (List b) -> Maybe (List b)
maybeMapStep parseFn input maybeAcc =
    maybeAcc
        |> Maybe.andThen
            (\acc ->
                Maybe.map (\parsed -> parsed :: acc) (parseFn input)
            )
