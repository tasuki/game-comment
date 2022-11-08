module LittleGolemTests exposing (..)

import Expect
import LittleGolem exposing (..)
import LittleGolemRecords exposing (..)
import Parser
import Test exposing (..)


propertyParserTest =
    describe "PropertyParser"
        [ test "Can parse a property" <|
            \_ ->
                Expect.equal
                    (Ok ( "EV", "twixt.ch.23.1.1" ))
                    (Parser.run propertyParser "EV[twixt.ch.23.1.1]B[aa]")
        ]


nodeParserTest =
    describe "NodeParser"
        [ test "Can parse a node" <|
            \_ ->
                Expect.equal
                    (Ok
                        [ ( "FF", "4" )
                        , ( "EV", "twixt.ch.23.1.1" )
                        , ( "PB", "spd_iv" )
                        , ( "PW", "tasuki" )
                        , ( "SZ", "24" )
                        ]
                    )
                    (Parser.run
                        nodeParser
                        "FF[4]EV[twixt.ch.23.1.1]PB[spd_iv]PW[tasuki]SZ[24];B[aa]"
                    )
        ]


parserTest =
    describe "Parser"
        [ test "Can parse twixtSpdIv with correct properties" <|
            let
                parsed : List Node
                parsed =
                    Result.withDefault [] <| Parser.run parser twixtSpdIv
            in
            Expect.all
                [ \_ ->
                    Expect.equal
                        (Just
                            [ ( "FF", "4" )
                            , ( "EV", "twixt.ch.23.1.1" )
                            , ( "PB", "spd_iv" )
                            , ( "PW", "tasuki" )
                            , ( "SZ", "24" )
                            , ( "SO", "https://www.littlegolem.net" )
                            ]
                        )
                        (List.head parsed)
                , \_ ->
                    Expect.equal
                        (Just [ ( "b", "ch" ) ])
                        (List.tail parsed |> Maybe.andThen List.head)
                ]
        ]
