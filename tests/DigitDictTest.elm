module DigitDictTest exposing (..)

import DigitDict
import Expect
import Set
import Test exposing (..)


suite : Test
suite =
    describe "digit dict"
        [ test "creates a new dict" <|
            \() ->
                DigitDict.new
                    |> Expect.equal []
        , test "inserts a value in the dict" <|
            \() ->
                DigitDict.new
                    |> DigitDict.insert (Set.fromList [ "a", "b", "z" ]) 7
                    |> Expect.equal [ ( Set.fromList [ "a", "b", "z" ], 7 ) ]
        , test "gets an existing value" <|
            \() ->
                DigitDict.new
                    |> DigitDict.insert (Set.fromList [ "a", "b", "z" ]) 7
                    |> DigitDict.get (Set.fromList [ "a", "b", "z" ])
                    |> Expect.equal (Just 7)
        , test "gets an absent value" <|
            \() ->
                DigitDict.new
                    |> DigitDict.insert (Set.fromList [ "a", "b", "z" ]) 7
                    |> DigitDict.get (Set.fromList [ "a", "b", "i" ])
                    |> Expect.equal Nothing
        ]
