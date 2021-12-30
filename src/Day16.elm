module Day16 exposing (OperatorType(..), Packet(..), parseInput, readOuterPacket, solvePart1, solvePart2)

import List
import Utilities exposing (binaryToBase10, maybeAll, unwrapMaybeWithMessage)


{-| Day 16: Packet Decoder
<https://adventofcode.com/2021/day/16>

This was a really fun one. Took me a some time, and is quite verbose, but I'm happy with the result.
It's always to much fun to see the machine turning bits into rules and logic.

-}



---------------
-- PARSE
---------------


parseInput : String -> Maybe InputStream
parseInput input =
    input
        |> String.split ""
        |> List.map toBinary
        |> maybeAll
        |> Maybe.map (String.join "")
        |> Maybe.map String.toList


toBinary : String -> Maybe String
toBinary char =
    case char of
        "0" ->
            Just "0000"

        "1" ->
            Just "0001"

        "2" ->
            Just "0010"

        "3" ->
            Just "0011"

        "4" ->
            Just "0100"

        "5" ->
            Just "0101"

        "6" ->
            Just "0110"

        "7" ->
            Just "0111"

        "8" ->
            Just "1000"

        "9" ->
            Just "1001"

        "A" ->
            Just "1010"

        "B" ->
            Just "1011"

        "C" ->
            Just "1100"

        "D" ->
            Just "1101"

        "E" ->
            Just "1110"

        "F" ->
            Just "1111"

        _ ->
            Nothing



-----------------------------
-- PART 1
-----------------------------


type Packet
    = Operator Version OperatorType (List Packet)
    | LiteralValue Version Int


type OperatorType
    = Sum
    | Product
    | Minimum
    | Maximum
    | GreaterThan
    | LessThan
    | EqualTo


type alias Version =
    Int


solvePart1 : InputStream -> Maybe Int
solvePart1 input =
    readOuterPacket input
        |> Maybe.map (List.singleton >> sumVersionNumbers)


sumVersionNumbers : List Packet -> Int
sumVersionNumbers packets =
    List.foldl
        (\packet sum ->
            case packet of
                LiteralValue version _ ->
                    sum + version

                Operator version _ subpackets ->
                    sum + version + sumVersionNumbers subpackets
        )
        0
        packets



-----------------------------
-- PART 2
-----------------------------


solvePart2 : InputStream -> Maybe Int
solvePart2 input =
    readOuterPacket input
        |> Maybe.map calculate


calculate : Packet -> Int
calculate packet =
    case packet of
        LiteralValue _ value ->
            value

        Operator _ operatorType subpackets ->
            let
                calculatedSubpackets =
                    List.map calculate subpackets
            in
            case operatorType of
                Sum ->
                    List.sum calculatedSubpackets

                Product ->
                    List.product calculatedSubpackets

                Minimum ->
                    List.minimum calculatedSubpackets
                        |> unwrapMaybeWithMessage "EMPTY LIST FOR MINIMUM"

                Maximum ->
                    List.maximum calculatedSubpackets
                        |> unwrapMaybeWithMessage "EMPTY LIST FOR MAXIMUM"

                GreaterThan ->
                    applyListOperator (>) calculatedSubpackets

                LessThan ->
                    applyListOperator (<) calculatedSubpackets

                EqualTo ->
                    applyListOperator (==) calculatedSubpackets


applyListOperator : (Int -> Int -> Bool) -> List Int -> Int
applyListOperator operator subpackets =
    case subpackets of
        [ a, b ] ->
            if operator a b then
                1

            else
                0

        _ ->
            Debug.todo "EXPECTED LIST OF TWO VALUES"



---------------------
-- BITS READING LOGIC
---------------------


type alias InputStream =
    List Char


{-| The BITS transmission has a single outer package
-}
readOuterPacket : InputStream -> Maybe Packet
readOuterPacket input =
    readNPackets 1 input
        |> Maybe.map Tuple.first
        |> Maybe.andThen List.head


{-| Read many packets until the end of the input stream is reached.
This is used for "sub"-input streams where a given stream is read to its end.
-}
readManyPackets : List Packet -> InputStream -> List Packet
readManyPackets parsed input =
    case readPacket input of
        Just ( packet, rest ) ->
            readManyPackets (packet :: parsed) rest

        Nothing ->
            parsed |> List.reverse


{-| Read a predefined number of packets
-}
readNPackets : Int -> InputStream -> Maybe ( List Packet, InputStream )
readNPackets n input =
    doReadNPackets n input []


doReadNPackets : Int -> InputStream -> List Packet -> Maybe ( List Packet, InputStream )
doReadNPackets n input read =
    if n < 1 then
        Just ( List.reverse read, input )

    else
        readPacket input
            |> Maybe.andThen (\( packet, rest ) -> doReadNPackets (n - 1) rest (packet :: read))


{-| Read a single packet
-}
readPacket : InputStream -> Maybe ( Packet, InputStream )
readPacket input =
    readHeaderAndVersion input
        |> Maybe.andThen delegateParsing


delegateParsing : ( ( PacketType, Version ), InputStream ) -> Maybe ( Packet, InputStream )
delegateParsing ( ( packetType, packetVersion ), rest ) =
    if rest == [] then
        Nothing

    else
        case packetType of
            LiteralValueType ->
                let
                    ( literalInt, restAfterContents ) =
                        parseLiteralValue rest
                in
                Just ( LiteralValue packetVersion literalInt, restAfterContents )

            OperatorType typeId ->
                let
                    ( subpackets, restAfterContents ) =
                        parseOperator rest

                    typeVariant =
                        typeFromTypeId typeId |> unwrapMaybeWithMessage "INVALID TYPE ID"
                in
                Just ( Operator packetVersion typeVariant subpackets, restAfterContents )


type PacketType
    = LiteralValueType
    | OperatorType Int


{-| Read the first six bits to determine packet version and packet type id
-}
readHeaderAndVersion : InputStream -> Maybe ( ( PacketType, Int ), InputStream )
readHeaderAndVersion input =
    case input of
        v1 :: v2 :: v3 :: t1 :: t2 :: t3 :: rest ->
            let
                packetTypeId =
                    binaryToBase10 [ t1, t2, t3 ]

                valueType =
                    if packetTypeId == 4 then
                        LiteralValueType

                    else
                        OperatorType packetTypeId
            in
            Just ( ( valueType, binaryToBase10 [ v1, v2, v3 ] ), rest )

        _ ->
            Nothing


parseLiteralValue : InputStream -> ( Int, InputStream )
parseLiteralValue input =
    doParseLiteralValue input []


doParseLiteralValue : InputStream -> List InputStream -> ( Int, InputStream )
doParseLiteralValue input bitAcc =
    case input of
        continueReading :: b1 :: b2 :: b3 :: b4 :: rest ->
            let
                result =
                    [ b1, b2, b3, b4 ]
            in
            if continueReading == '1' then
                doParseLiteralValue rest (result :: bitAcc)

            else
                let
                    parsed =
                        (result :: bitAcc)
                            |> List.reverse
                            |> List.concat
                            |> binaryToBase10
                in
                ( parsed, rest )

        _ ->
            Debug.todo "PARSING LITERAL VALUE"


parseOperator : InputStream -> ( List Packet, InputStream )
parseOperator input =
    let
        ( lengthTypeId, rest ) =
            readLengthTypeId input
    in
    case lengthTypeId of
        SubpacketsBits bits ->
            let
                subpackets =
                    readManyPackets [] (List.take bits rest)
            in
            ( subpackets, List.drop bits rest )

        SubpacketCount count ->
            let
                ( subpackets, restAfterRead ) =
                    readNPackets count rest
                        |> unwrapMaybeWithMessage "READING N PACKETS"
            in
            ( subpackets, restAfterRead )


type LengthTypeId
    = SubpacketsBits Int
    | SubpacketCount Int


readLengthTypeId : InputStream -> ( LengthTypeId, InputStream )
readLengthTypeId input =
    case input of
        '0' :: rest ->
            let
                lengthTypeId =
                    List.take 15 rest
                        |> binaryToBase10
                        |> SubpacketsBits
            in
            ( lengthTypeId, List.drop 15 rest )

        '1' :: rest ->
            let
                lengthTypeId =
                    List.take 11 rest
                        |> binaryToBase10
                        |> SubpacketCount
            in
            ( lengthTypeId, List.drop 11 rest )

        _ ->
            Debug.todo "READ LENGTH TYPE ID"


typeFromTypeId : Int -> Maybe OperatorType
typeFromTypeId typeId =
    case typeId of
        0 ->
            Just Sum

        1 ->
            Just Product

        2 ->
            Just Minimum

        3 ->
            Just Maximum

        5 ->
            Just GreaterThan

        6 ->
            Just LessThan

        7 ->
            Just EqualTo

        _ ->
            Nothing
