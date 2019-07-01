module FormattingTests exposing (basicTests, instanceTests, mapTests, paddingTests, premapTests, roundToTests, wrapTests)

import Expect
import Formatting exposing (..)
import String exposing (reverse, toUpper)
import Test exposing (..)


basicTests : Test
basicTests =
    describe "Basics"
        [ describe "defaultTest"
            [ test "string arg and const string" <|
                \_ ->
                    Expect.equal "Hello Kris!"
                        (print (s "Hello " |> bs string |> bs (s "!")) "Kris")
            , test "int arg and const tring" <|
                \_ ->
                    Expect.equal "We need 5 cats."
                        (print (s "We need " |> bs int |> bs (s " cats.")) 5)
            , test "float arg" <|
                \_ ->
                    Expect.equal "Height: 1.72"
                        (print (s "Height: " |> bs float) 1.72)
            ]
        ]


mapTests : Test
mapTests =
    let
        format =
            string |> bs (s "!")

        check ( f, expected ) =
            Expect.equal expected
                (print (map f format) "Hello")
    in
    describe "map"
        [ test "pass through identity" <| \_ -> check ( identity, "Hello!" )
        , test "upper case" <| \_ -> check ( toUpper, "HELLO!" )
        , test "reverse" <| \_ -> check ( reverse, "!olleH" )
        ]


premapTests : Test
premapTests =
    let
        record =
            { name = "Kris"
            , height = 1.72
            }
    in
    describe "premap"
        [ test "string field premap" <| \_ -> Expect.equal "Name: Kris" (print (s "Name: " |> bs (premap .name string)) record)
        , test "float field premap" <| \_ -> Expect.equal "Height: 1.72" (print (s "Height: " |> bs (premap .height float)) record)
        ]


wrapTests : Test
wrapTests =
    describe "wrap"
        [ test "wrap around string" <| \_ -> Expect.equal "This is *great*!" (print (s "This is " |> bs (wrap "*" string) |> bs (s "!")) "great")
        , test "wrap quotes around int" <| \_ -> Expect.equal "Received '50'" (print (s "Received " |> bs (wrap "'" int)) 50)
        ]


paddingTests : Test
paddingTests =
    describe "padding"
        [ test "pad float" <| \_ -> Expect.equal "___1.72___" (print (pad 10 '_' float) 1.72)
        , test "pad left float" <| \_ -> Expect.equal "______1.72" (print (padLeft 10 '_' float) 1.72)
        , test "pad right float" <| \_ -> Expect.equal "1.72______" (print (padRight 10 '_' float) 1.72)
        , test "pad no-op" <| \_ -> Expect.equal "1.7234567891" (print (pad 10 '.' float) 1.7234567891)
        , test "pad left no-op" <| \_ -> Expect.equal "1.7234567891" (print (padLeft 10 '.' float) 1.7234567891)
        , test "pad right no-op" <| \_ -> Expect.equal "1.7234567891" (print (padRight 10 '.' float) 1.7234567891)
        ]


roundToTests : Test
roundToTests =
    let
        check ( expected, formatter, value ) =
            Expect.equal expected
                (print formatter value)
    in
    describe "roundTo"
        [ test "integer" <| \_ -> check ( "1235", roundTo 0, 1234.56 )
        , test "no round float" <| \_ -> check ( "1234.0", roundTo 1, 1234 )
        , test "two decimals" <| \_ -> check ( "1234.00", roundTo 2, 1234.0 )
        , test "zero at 2 decimals" <| \_ -> check ( "0.00", roundTo 2, 0 )
        , test "zero" <| \_ -> check ( "0", roundTo 0, 0 )
        , test "float round to 2" <| \_ -> check ( "1234.57", roundTo 2, 1234.567 )
        , test "float round to 6" <| \_ -> check ( "1234.567000", roundTo 6, 1234.567 )
        , test "neg. float" <| \_ -> check ( "-1235", roundTo 0, -1234.56 )
        , test "neg. float to 1" <| \_ -> check ( "-1234.0", roundTo 1, -1234 )
        , test "neg. float to 2" <| \_ -> check ( "-1234.57", roundTo 2, -1234.567 )
        , test "net. float to 6" <| \_ -> check ( "-1234.567000", roundTo 6, -1234.567 )
        , test "round up" <| \_ -> check ( "100.0", roundTo 1, 99.99 )
        , test "round to 1" <| \_ -> check ( "2.0", roundTo 1, 1.95 )
        , test "round to 2 up" <| \_ -> check ( "5.18", roundTo 2, 5.175 )
        , test "round to 3" <| \_ -> check ( "0.014", roundTo 3, 0.0135 )
        , test "round to 4" <| \_ -> check ( "7.1316", roundTo 4, 7.13155 )
        , test "round to 2 down" <| \_ -> check ( "0.12", roundTo 2, 0.123 )
        , test "round up negative" <| \_ -> check ( "-0.12", roundTo 2, -0.123 )
        , test "roudn zero" <| \_ -> check ( "0.00", roundTo 2, 0 )
        , test "no round req" <| \_ -> check ( "-0.46", roundTo 2, -0.46 )
        , test "no round req .99" <| \_ -> check ( "-0.99", roundTo 2, -0.99 )
        , test "no round req .46" <| \_ -> check ( "-1.46", roundTo 2, -1.46 )
        , test "neg round down" <| \_ -> check ( "-1.00", roundTo 2, -0.999 )
        , test "neg no round required" <| \_ -> check ( "-2.46", roundTo 2, -2.46 )
        , test "neg round down 2.999" <| \_ -> check ( "-3.00", roundTo 2, -2.999 )
        ]


instanceTests : Test
instanceTests =
    describe "Tests for specific uses."
        [ test "Test formatting price" <|
            \_ ->
                Expect.equal "Price:  12345.43" <|
                    print (s "Price:" |> bs (padLeft 10 ' ' <| roundTo 2))
                        12345.4321
        ]
