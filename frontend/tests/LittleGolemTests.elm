module LittleGolemTests exposing (..)

import Expect
import GameRecord exposing (Coords, GameSource(..), Move, Play(..), Player(..), Record)
import LittleGolem exposing (..)
import LittleGolemRecords exposing (..)
import Parser
import Test exposing (..)



-- parser tests


playParserTest =
    describe "PlayParser"
        [ test "Can parse swap" <|
            \_ ->
                Expect.equal
                    (Ok Swap)
                    (Parser.run playParser "swap")
        , test "Can parse simple move" <|
            \_ ->
                Expect.equal
                    (Ok <| Place { x = 1, y = 2 })
                    (Parser.run playParser "ab")
        , test "Can parse a move with first part encoded" <|
            \_ ->
                Expect.equal
                    (Ok <| Place { x = 100, y = 5 })
                    (Parser.run playParser "{{196}}e")
        , test "Can parse a move with second part encoded" <|
            \_ ->
                Expect.equal
                    (Ok <| Place { x = 1, y = 100 })
                    (Parser.run playParser "a{{196}}")
        , test "Can parse a move with both parts encoded" <|
            \_ ->
                Expect.equal
                    (Ok <| Place { x = 90, y = 100 })
                    (Parser.run playParser "{{186}}{{196}}")
        , test "Can parse move with draw offered" <|
            \_ ->
                Expect.equal
                    (Ok <| Place { x = 1, y = 100 })
                    (Parser.run playParser "a{{196}}|draw")
        , test "Can parse move with { as first char" <|
            \_ ->
                Expect.equal
                    (Ok <| Place { x = 27, y = 100 })
                    (Parser.run playParser "{{{196}}")
        , test "Can parse move with } as last char" <|
            \_ ->
                Expect.equal
                    (Ok <| Place { x = 100, y = 29 })
                    (Parser.run playParser "{{196}}}")
        , test "Can't parse move in unknown format" <|
            \_ ->
                Expect.err
                    (Parser.run playParser "a{{196}}|x")
        ]


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
                        ";FF[4]EV[twixt.ch.23.1.1]PB[spd_iv]PW[tasuki]SZ[24];B[aa]"
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


getMove : List Move -> Int -> Maybe Move
getMove ms move =
    List.drop (move - 1) ms |> List.head


recordMoves : String -> ( Record, List Move )
recordMoves game =
    let
        record =
            parse (GameSource "" "") game |> Result.withDefault (GameRecord.empty GameRecord.TwixT 24)
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
                , \_ -> Expect.equal (Just { player = White, play = Place <| Coords 3 8 }) (getMove moves 1)
                , \_ -> Expect.equal (Just { player = Black, play = Swap }) (getMove moves 2)
                , \_ -> Expect.equal (Just { player = White, play = Place <| Coords 10 10 }) (getMove moves 3)
                , \_ -> Expect.equal (Just { player = Black, play = Place <| Coords 9 4 }) (getMove moves 38)
                , \_ -> Expect.equal (Just { player = White, play = Resign }) (getMove moves 39)
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
                , \_ -> Expect.equal (Just { player = Black, play = Place <| Coords 35 4 }) (getMove moves 1)
                , \_ -> Expect.equal (Just { player = White, play = Place <| Coords 34 34 }) (getMove moves 2)
                , \_ -> Expect.equal (Just { player = Black, play = Place <| Coords 4 34 }) (getMove moves 3)
                , \_ -> Expect.equal (Just { player = White, play = Place <| Coords 4 4 }) (getMove moves 4)
                , \_ -> Expect.equal (Just { player = White, play = Place <| Coords 37 30 }) (getMove moves 698)
                ]
        , test "Can parse twixtDJB" <|
            let
                ( record, moves ) =
                    recordMoves twixtDJB
            in
            Expect.all
                [ \_ -> Expect.equal "David J Bush" record.black
                , \_ -> Expect.equal "tasuki" record.white
                , \_ -> Expect.equal GameRecord.TwixT record.game
                , \_ -> Expect.equal 30 record.size
                , \_ -> Expect.equal (Just { player = White, play = Place <| Coords 4 3 }) (getMove moves 1)
                , \_ -> Expect.equal (Just { player = Black, play = Place <| Coords 15 16 }) (getMove moves 2)
                ]
        , test "Can parse hexLazy" <|
            let
                ( record, moves ) =
                    recordMoves hexLazy
            in
            Expect.all
                [ \_ -> Expect.equal "tasuki" record.black
                , \_ -> Expect.equal "lazyplayer" record.white
                , \_ -> Expect.equal GameRecord.Hex record.game
                , \_ -> Expect.equal 19 record.size
                , \_ -> Expect.equal (Just { player = Black, play = Place <| Coords 1 3 }) (getMove moves 1)
                , \_ -> Expect.equal (Just { player = White, play = Place <| Coords 5 15 }) (getMove moves 2)
                , \_ -> Expect.equal (Just { player = White, play = Place <| Coords 14 7 }) (getMove moves 36)
                , \_ -> Expect.equal (Just { player = Black, play = Resign }) (getMove moves 37)
                ]
        , test "Can parse twixtCassiel" <|
            let
                ( record, moves ) =
                    recordMoves twixtCassiel
            in
            Expect.all
                [ \_ -> Expect.equal "tasuki" record.black
                , \_ -> Expect.equal "Cassiel" record.white
                , \_ -> Expect.equal GameRecord.TwixT record.game
                , \_ -> Expect.equal 48 record.size
                , \_ -> Expect.equal (Just { player = White, play = Place <| Coords 45 14 }) (getMove moves 1)
                , \_ -> Expect.equal (Just { player = Black, play = Place <| Coords 32 32 }) (getMove moves 2)
                ]
        , test "Can parse hexMP" <|
            let
                ( record, moves ) =
                    recordMoves hexMP
            in
            Expect.all
                [ \_ -> Expect.equal "tasuki" record.black
                , \_ -> Expect.equal "Marcin Pindral" record.white
                , \_ -> Expect.equal GameRecord.Hex record.game
                , \_ -> Expect.equal 19 record.size
                , \_ -> Expect.equal (Just { player = Black, play = Place <| Coords 1 3 }) (getMove moves 1)
                , \_ -> Expect.equal (Just { player = White, play = Place <| Coords 3 5 }) (getMove moves 2)
                ]
        , test "Can parse torusAdam" <|
            let
                ( record, moves ) =
                    recordMoves torusAdam
            in
            Expect.all
                [ \_ -> Expect.equal "tasuki" record.black
                , \_ -> Expect.equal "egozolwia" record.white
                , \_ -> Expect.equal GameRecord.ToroidGo record.game
                , \_ -> Expect.equal 11 record.size
                , \_ -> Expect.equal (Just { player = Black, play = Place <| Coords 11 1 }) (getMove moves 1)
                , \_ -> Expect.equal (Just { player = White, play = Place <| Coords 6 6 }) (getMove moves 2)
                ]
        ]



-- test getting identifier


toGameIdTest =
    describe "toGameId"
        [ test "Can return simple id" <|
            \_ ->
                Expect.equal
                    (Ok "2221003")
                    (toGameId "2221003")
        , test "Can return id from full url" <|
            \_ ->
                Expect.equal
                    (Ok "2221003")
                    (toGameId "https://www.littlegolem.net/jsp/game/game.jsp?gid=2221003")
        , test "Can return id from messed up url" <|
            \_ ->
                Expect.equal
                    (Ok "2221003")
                    (toGameId "littlegolem.net/jsp/game/game.jsp?gid=2221003&whatever=whatever")
        , test "Fails when given a random url" <|
            \_ ->
                Expect.err (toGameId "https://blog.tasuki.org/variations/")
        ]
