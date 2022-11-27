module TwixT exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import GameRecord as R
import Svg exposing (Svg)
import Svg.Attributes as SA


type alias Position =
    { pegs : List ( R.Player, R.Coords )
    , links : List ( R.Player, ( R.Coords, R.Coords ) )
    }


type alias Replay =
    { moves : Array R.Play
    , currentMove : Int
    , currentPosition : Position
    }



-- View


background : Int -> List (Svg msg)
background size =
    [ Svg.rect
        [ SA.x "-0.5"
        , SA.y "-0.5"
        , SA.width <| String.fromInt <| size
        , SA.height <| String.fromInt <| size
        , SA.fill "#CCC"
        ]
        []
    ]


drawBorders : Int -> List (Svg msg)
drawBorders size =
    let
        length =
            String.fromInt <| size - 3

        otherStartPoint =
            (String.fromInt <| size - 2) ++ ".5"

        border : String -> String -> String -> Svg msg
        border from path color =
            Svg.path
                [ SA.d <|
                    "M"
                        ++ from
                        ++ " "
                        ++ path
                , SA.stroke <| color
                , SA.strokeWidth ".25"
                , SA.strokeLinecap <| "square"
                ]
                []
    in
    [ border "1,0.5" ("h" ++ length) "white"
    , border "0.5,1" ("v" ++ length) "black"
    , border ("1," ++ otherStartPoint) ("h" ++ length) "white"
    , border (otherStartPoint ++ ",1") ("v" ++ length) "black"
    ]


drawGuidelines : Int -> List (Svg msg)
drawGuidelines size =
    let
        val : Int -> String
        val i =
            String.fromFloat <|
                if i == 0 then
                    1

                else if i == 1 then
                    toFloat (size - 1) / 2.0

                else
                    toFloat size - 2

        guide x1 y1 x2 y2 =
            Svg.line
                [ SA.x1 <| val x1
                , SA.y1 <| val y1
                , SA.x2 <| val x2
                , SA.y2 <| val y2
                , SA.stroke "#AAA"
                , SA.strokeWidth ".1"
                , SA.strokeLinecap <| "round"
                ]
                []
    in
    [ guide 0 0 1 2
    , guide 0 0 2 1
    , guide 0 2 1 0
    , guide 0 2 2 1
    , guide 2 0 0 1
    , guide 2 0 1 2
    , guide 2 2 0 1
    , guide 2 2 1 0
    ]


coordList : Int -> List R.Coords
coordList size =
    let
        isBorder : Int -> Bool
        isBorder i =
            (i == 0) || (i == (size - 1))

        nodes : List Int
        nodes =
            List.range 0 (size - 1)
    in
    List.concatMap (\x -> List.map (R.Coords x) nodes) nodes
        |> List.filter (\coord -> not (isBorder coord.x) || not (isBorder coord.y))


drawPoints : Int -> List (Svg msg)
drawPoints size =
    let
        toHole : R.Coords -> Svg msg
        toHole coords =
            Svg.circle
                [ SA.cx <| String.fromInt coords.x
                , SA.cy <| String.fromInt coords.y
                , SA.r "0.1"
                ]
                []
    in
    List.map toHole (coordList size)


drawPegs : Int -> Position -> List (Svg msg)
drawPegs size position =
    let
        pegs : Dict ( Int, Int ) R.Player
        pegs =
            List.map (\( player, coords ) -> ( ( coords.x, coords.y ), player )) position.pegs
                |> Dict.fromList

        drawCoords : R.Coords -> Svg msg
        drawCoords coords =
            let
                coordProps =
                    [ SA.cx <| String.fromInt coords.x, SA.cy <| String.fromInt coords.y ]

                styleProps =
                    case Dict.get ( coords.x, coords.y ) pegs of
                        Nothing ->
                            [ SA.r "0.4", SA.fill "transparent", SA.class "clickable" ]

                        Just player ->
                            [ SA.r "0.3", SA.fill <| R.color player ]
            in
            Svg.circle (coordProps ++ styleProps) []
    in
    List.map drawCoords (coordList size)


view : R.Record -> Replay -> List (Svg msg)
view record replay =
    let
        size =
            record.size
    in
    background size
        ++ drawBorders size
        ++ drawGuidelines size
        ++ drawPoints size
        ++ drawPegs size replay.currentPosition
