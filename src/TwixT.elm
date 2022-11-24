module TwixT exposing (..)

import Array
import GameRecord as R
import GameReplay as Replay
import Svg exposing (Svg)
import Svg.Attributes as SA


type alias Position =
    { blackPegs : List R.Coords
    , whitePegs : List R.Coords
    , blackLines : List ( R.Coords, R.Coords )
    , whiteLines : List ( R.Coords, R.Coords )
    }


emptyReplay : Replay.Replay Position
emptyReplay =
    { record = R.empty
    , moves = Array.empty
    , currentMove = 0
    , currentPosition = Position [] [] [] []
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


drawPoints : Int -> List (Svg msg)
drawPoints size =
    let
        isBorder : Int -> Bool
        isBorder i =
            (i == 0) || (i == (size - 1))

        nodes : List Int
        nodes =
            List.range 0 (size - 1)

        coords : List R.Coords
        coords =
            List.concatMap (\x -> List.map (R.Coords x) nodes) nodes
                |> List.filter (\coord -> not (isBorder coord.x) || not (isBorder coord.y))

        toHole : R.Coords -> Svg msg
        toHole crds =
            Svg.circle
                [ SA.cx <| String.fromInt crds.x
                , SA.cy <| String.fromInt crds.y
                , SA.r "0.1"
                ]
                []
    in
    List.map toHole coords


view : Int -> List (Svg msg)
view size =
    background size
        ++ drawBorders size
        ++ drawGuidelines size
        ++ drawPoints size
