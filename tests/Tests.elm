module Tests exposing (suite)

import Expect
import Main
import Test exposing (Test)
import Time


suite : Test
suite =
    Test.concat
        [ Test.describe "test fibo"
            [ Test.test "fibo 0" <|
                \_ -> Main.fibo 0 |> Expect.equal 1
            , Test.test "fibo 1" <|
                \_ -> Main.fibo 1 |> Expect.equal 2
            , Test.test "fibo 2" <|
                \_ -> Main.fibo 2 |> Expect.equal 3
            , Test.test "fibo 3" <|
                \_ -> Main.fibo 3 |> Expect.equal 5
            , Test.test "fibo 4" <|
                \_ -> Main.fibo 4 |> Expect.equal 8
            , Test.test "fibo 5" <|
                \_ -> Main.fibo 5 |> Expect.equal 13
            , Test.test "fibo 6" <|
                \_ -> Main.fibo 6 |> Expect.equal 21
            , Test.test "fibo 7" <|
                \_ -> Main.fibo 7 |> Expect.equal 34
            , Test.test "fibo 10" <|
                \_ -> Main.fibo 10 |> Expect.equal 144
            , Test.test "fibo 12" <|
                \_ -> Main.fibo 12 |> Expect.equal 377
            , Test.test "fibo 20" <|
                \_ -> Main.fibo 20 |> Expect.equal 17711
            , Test.test "fibo 30" <|
                \_ -> Main.fibo 30 |> Expect.equal 2178309
            , Test.test "fibo 50" <|
                \_ -> Main.fibo 50 |> Expect.equal 32951280099
            ]
        , Test.describe "test processCard"
            [ Test.test "processCard False rank 0" <|
                \_ ->
                    let
                        now =
                            Time.millisToPosix 10000

                        expected =
                            Main.Card "" { contentType = Main.Raw, value = "" } 0 <|
                                Time.millisToPosix (10000 + 864 * 10 ^ 5)

                        card =
                            Main.Card "" { contentType = Main.Raw, value = "" } 0 now
                    in
                    Main.processCard now card False |> Expect.equal expected
            , Test.test "processCard True rank 0" <|
                \_ ->
                    let
                        now =
                            Time.millisToPosix 10000

                        expected =
                            Main.Card "" { contentType = Main.Raw, value = "" } 1 <|
                                Time.millisToPosix (10000 + 864 * 10 ^ 5 * 2)

                        card =
                            Main.Card "" { contentType = Main.Raw, value = "" } 0 now
                    in
                    Main.processCard now card True |> Expect.equal expected
            , Test.test "processCard bad answer" <|
                \_ ->
                    let
                        now =
                            Time.millisToPosix 10000

                        card =
                            Main.Card "" { contentType = Main.Raw, value = "" } 5 now

                        res =
                            Main.processCard now card False

                        year =
                            Time.toYear Time.utc res.nextReviewAt

                        month =
                            Time.toMonth Time.utc res.nextReviewAt

                        day =
                            Time.toDay Time.utc res.nextReviewAt
                    in
                    Expect.equal ( day, month, year ) ( 2, Time.Jan, 1970 )
            , Test.test "processCard good answer" <|
                \_ ->
                    let
                        now =
                            Time.millisToPosix 10000

                        card =
                            Main.Card "" { contentType = Main.Raw, value = "" } 5 now

                        res =
                            Main.processCard now card True

                        year =
                            Time.toYear Time.utc res.nextReviewAt

                        month =
                            Time.toMonth Time.utc res.nextReviewAt

                        day =
                            Time.toDay Time.utc res.nextReviewAt
                    in
                    Expect.equal ( day, month, year ) ( 22, Time.Jan, 1970 )
            , Test.test "processCard good answer and forget" <|
                \_ ->
                    let
                        now =
                            Time.millisToPosix 10000

                        card =
                            Main.Card "" { contentType = Main.Raw, value = "" } 12 now

                        res =
                            Main.processCard now card True

                        year =
                            Time.toYear Time.utc res.nextReviewAt

                        month =
                            Time.toMonth Time.utc res.nextReviewAt

                        day =
                            Time.toDay Time.utc res.nextReviewAt
                    in
                    Expect.equal ( res.rank, ( day, month, year ) ) ( 12, ( 13, Time.Jan, 1971 ) )
            ]
        ]
