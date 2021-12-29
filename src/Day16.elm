module Day16 exposing (Packet(..), parseInput, readOuterPacket, solvePart1)

import List
import Utilities exposing (binaryToBase10, maybeAll, unwrapMaybeWithMessage)


{-| Day 16: Packet Decoder
<https://adventofcode.com/2021/day/16>
-}



---------------
-- PARSE
---------------


parseInput : String -> Maybe (List Char)
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
    = Operator Version TypeId (List Packet)
    | LiteralValue Version Int


type alias TypeId =
    Int


type alias Version =
    Int


solvePart1 : List Char -> Maybe Int
solvePart1 input =
    readOuterPacket input
        |> Maybe.map List.singleton
        |> Maybe.map sumVersionNumbers


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


readOuterPacket : List Char -> Maybe Packet
readOuterPacket input =
    input
        |> readManyPackets []
        |> List.head



{- Packets can be a list of packets, e.g. many literal values in a row.
   Packets can be also be a single operator packet with many nested packets.
   So we need to return a list of packets from readPackets.
-}


{-| Read many packets until the end of the input stream is reached
-}
readManyPackets : List Packet -> List Char -> List Packet
readManyPackets parsed input =
    case readPacket input of
        Just ( packet, rest ) ->
            readManyPackets (packet :: parsed) rest

        Nothing ->
            parsed |> List.reverse


{-| Read a predefined number of packets
-}
readNPackets : Int -> List Char -> Maybe ( List Packet, List Char )
readNPackets n input =
    doReadNPackets n input []


doReadNPackets : Int -> List Char -> List Packet -> Maybe ( List Packet, List Char )
doReadNPackets n input read =
    if n < 1 then
        Just ( List.reverse read, input )

    else
        readPacket input
            |> Maybe.andThen (\( packet, rest ) -> doReadNPackets (n - 1) rest (packet :: read))


{-| Read a single packet, returning the remaining stream
-}
readPacket : List Char -> Maybe ( Packet, List Char )
readPacket input =
    readHeaderAndVersion input
        |> Maybe.andThen delegateParsing


delegateParsing : ( ( PacketType, Version ), List Char ) -> Maybe ( Packet, List Char )
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
                in
                Just ( Operator packetVersion typeId subpackets, restAfterContents )


type PacketType
    = LiteralValueType
    | OperatorType Int


{-| Read the first six bits to determine packet version and packet type id
-}
readHeaderAndVersion : List Char -> Maybe ( ( PacketType, Int ), List Char )
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


parseLiteralValue : List Char -> ( Int, List Char )
parseLiteralValue input =
    doParseLiteralValue input []


doParseLiteralValue : List Char -> List (List Char) -> ( Int, List Char )
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


parseOperator : List Char -> ( List Packet, List Char )
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


readLengthTypeId : List Char -> ( LengthTypeId, List Char )
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
