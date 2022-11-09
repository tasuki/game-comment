module LittleGolemTests exposing (..)

import Expect
import GameRecord exposing (Move(..), Play, Player(..), Record)
import LittleGolem exposing (..)
import LittleGolemRecords exposing (..)
import Parser
import Test exposing (..)



-- parser tests


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



-- test concrete games


getMove : List Play -> Int -> Maybe Play
getMove ms move =
    List.drop (move - 1) ms |> List.head


recordMoves : String -> ( Record, List Play )
recordMoves game =
    let
        record =
            parse game |> Result.withDefault GameRecord.empty
    in
    ( record, record.moves )


parseTest =
    describe "Parse"
        [ test "Can parse twixtSpdIv" <|
            let
                ( record, moves ) =
                    recordMoves twixtSpdIv
            in
            Expect.all
                [ \_ -> Expect.equal "tasuki" record.black
                , \_ -> Expect.equal "spd_iv" record.white
                , \_ -> Expect.equal GameRecord.TwixT record.game
                , \_ -> Expect.equal 24 record.size
                , \_ -> Expect.equal "" record.result
                , \_ -> Expect.equal (Just { player = White, move = Place 3 8 }) (getMove moves 1)
                , \_ -> Expect.equal (Just { player = Black, move = Swap }) (getMove moves 2)
                , \_ -> Expect.equal (Just { player = White, move = Place 10 10 }) (getMove moves 3)
                , \_ -> Expect.equal (Just { player = Black, move = Place 9 4 }) (getMove moves 38)
                , \_ -> Expect.equal (Just { player = White, move = Resign }) (getMove moves 39)
                ]
        , test "Can parse goRio" <|
            let
                ( record, moves ) =
                    recordMoves goRio
            in
            Expect.all
                [ \_ -> Expect.equal "tasuki" record.black
                , \_ -> Expect.equal "Richard Malaschitz ?" record.white
                , \_ -> Expect.equal GameRecord.Go record.game
                , \_ -> Expect.equal 37 record.size
                , \_ -> Expect.equal (Just { player = Black, move = Place 35 4 }) (getMove moves 1)
                , \_ -> Expect.equal (Just { player = White, move = Place 34 34 }) (getMove moves 2)
                , \_ -> Expect.equal (Just { player = Black, move = Place 4 34 }) (getMove moves 3)
                , \_ -> Expect.equal (Just { player = White, move = Place 4 4 }) (getMove moves 4)
                , \_ -> Expect.equal (Just { player = White, move = Place 37 30 }) (getMove moves 698)
                ]
        ]
