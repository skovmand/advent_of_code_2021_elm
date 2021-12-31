module Day16Test exposing (..)

import Day16 exposing (OperatorType(..), Packet(..))
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "day 16"
        [ test "part 1 parses literal value" <|
            \() ->
                "D2FE28"
                    |> Day16.parseInput
                    |> Maybe.andThen Day16.readOuterPacket
                    |> Expect.equal (Just (LiteralValue 6 2021))
        , test "part 1 parses BITS message 38006F45291200" <|
            \() ->
                "38006F45291200"
                    |> Day16.parseInput
                    |> Maybe.andThen Day16.readOuterPacket
                    |> Expect.equal (Just (Operator 1 LessThan [ LiteralValue 6 10, LiteralValue 2 20 ]))
        , test "part 1 parses BITS message EE00D40C823060" <|
            \() ->
                "EE00D40C823060"
                    |> Day16.parseInput
                    |> Maybe.andThen Day16.readOuterPacket
                    |> Expect.equal (Just (Operator 7 Maximum [ LiteralValue 2 1, LiteralValue 4 2, LiteralValue 1 3 ]))
        , test "part 1 sums version headers in 8A004A801A8002F478" <|
            \() ->
                "8A004A801A8002F478"
                    |> Day16.parseInput
                    |> Maybe.andThen Day16.solvePart1
                    |> Expect.equal (Just 16)
        , test "part 1 sums version headers in 620080001611562C8802118E34" <|
            \() ->
                "620080001611562C8802118E34"
                    |> Day16.parseInput
                    |> Maybe.andThen Day16.solvePart1
                    |> Expect.equal (Just 12)
        , test "part 1 sums version headers in C0015000016115A2E0802F182340" <|
            \() ->
                "C0015000016115A2E0802F182340"
                    |> Day16.parseInput
                    |> Maybe.andThen Day16.solvePart1
                    |> Expect.equal (Just 23)
        , test "part 1 sums version headers in A0016C880162017C3686B18A3D4780" <|
            \() ->
                "A0016C880162017C3686B18A3D4780"
                    |> Day16.parseInput
                    |> Maybe.andThen Day16.solvePart1
                    |> Expect.equal (Just 31)
        , test "part 1 answer" <|
            \() ->
                puzzleInput
                    |> Day16.parseInput
                    |> Maybe.andThen Day16.solvePart1
                    |> Expect.equal (Just 843)
        , test "part 2 calculates C200B40A82" <|
            \() ->
                "C200B40A82"
                    |> Day16.parseInput
                    |> Maybe.andThen Day16.solvePart2
                    |> Expect.equal (Just 3)
        , test "part 2 calculates 04005AC33890" <|
            \() ->
                "04005AC33890"
                    |> Day16.parseInput
                    |> Maybe.andThen Day16.solvePart2
                    |> Expect.equal (Just 54)
        , test "part 2 calculates 880086C3E88112" <|
            \() ->
                "880086C3E88112"
                    |> Day16.parseInput
                    |> Maybe.andThen Day16.solvePart2
                    |> Expect.equal (Just 7)
        , test "part 2 calculates CE00C43D881120" <|
            \() ->
                "CE00C43D881120"
                    |> Day16.parseInput
                    |> Maybe.andThen Day16.solvePart2
                    |> Expect.equal (Just 9)
        , test "part 2 calculates D8005AC2A8F0" <|
            \() ->
                "D8005AC2A8F0"
                    |> Day16.parseInput
                    |> Maybe.andThen Day16.solvePart2
                    |> Expect.equal (Just 1)
        , test "part 2 calculates F600BC2D8F" <|
            \() ->
                "F600BC2D8F"
                    |> Day16.parseInput
                    |> Maybe.andThen Day16.solvePart2
                    |> Expect.equal (Just 0)
        , test "part 2 calculates 9C005AC2F8F0" <|
            \() ->
                "9C005AC2F8F0"
                    |> Day16.parseInput
                    |> Maybe.andThen Day16.solvePart2
                    |> Expect.equal (Just 0)
        , test "part 2 calculates 9C0141080250320F1802104A08" <|
            \() ->
                "9C0141080250320F1802104A08"
                    |> Day16.parseInput
                    |> Maybe.andThen Day16.solvePart2
                    |> Expect.equal (Just 1)
        , test "part 2 answer" <|
            \() ->
                puzzleInput
                    |> Day16.parseInput
                    |> Maybe.andThen Day16.solvePart2
                    |> Expect.equal (Just 5390807940351)
        ]


puzzleInput : String
puzzleInput =
    "220D62004EF14266BBC5AB7A824C9C1802B360760094CE7601339D8347E20020264D0804CA95C33E006EA00085C678F31B80010B88319E1A1802D8010D4BC268927FF5EFE7B9C94D0C80281A00552549A7F12239C0892A04C99E1803D280F3819284A801B4CCDDAE6754FC6A7D2F89538510265A3097BDF0530057401394AEA2E33EC127EC3010060529A18B00467B7ABEE992B8DD2BA8D292537006276376799BCFBA4793CFF379D75CA1AA001B11DE6428402693BEBF3CC94A314A73B084A21739B98000010338D0A004CF4DCA4DEC80488F004C0010A83D1D2278803D1722F45F94F9F98029371ED7CFDE0084953B0AD7C633D2FF070C013B004663DA857C4523384F9F5F9495C280050B300660DC3B87040084C2088311C8010C84F1621F080513AC910676A651664698DF62EA401934B0E6003E3396B5BBCCC9921C18034200FC608E9094401C8891A234080330EE31C643004380296998F2DECA6CCC796F65224B5EBBD0003EF3D05A92CE6B1B2B18023E00BCABB4DA84BCC0480302D0056465612919584662F46F3004B401600042E1044D89C200CC4E8B916610B80252B6C2FCCE608860144E99CD244F3C44C983820040E59E654FA6A59A8498025234A471ED629B31D004A4792B54767EBDCD2272A014CC525D21835279FAD49934EDD45802F294ECDAE4BB586207D2C510C8802AC958DA84B400804E314E31080352AA938F13F24E9A8089804B24B53C872E0D24A92D7E0E2019C68061A901706A00720148C404CA08018A0051801000399B00D02A004000A8C402482801E200530058AC010BA8018C00694D4FA2640243CEA7D8028000844648D91A4001088950462BC2E600216607480522B00540010C84914E1E0002111F21143B9BFD6D9513005A4F9FC60AB40109CBB34E5D89C02C82F34413D59EA57279A42958B51006A13E8F60094EF81E66D0E737AE08"
