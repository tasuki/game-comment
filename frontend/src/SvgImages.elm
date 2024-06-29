module SvgImages exposing (..)

import Html as H
import Svg exposing (Svg)
import Svg.Attributes as SA


defaultWidth : number
defaultWidth =
    2


defaultColor : String
defaultColor =
    "currentColor"


line : Int -> Int -> Int -> Int -> Svg msg
line =
    lineCustom defaultWidth defaultColor


lineCustom : Int -> String -> Int -> Int -> Int -> Int -> Svg msg
lineCustom strokeWidth color x1 y1 x2 y2 =
    Svg.line
        [ SA.x1 <| String.fromInt x1
        , SA.y1 <| String.fromInt y1
        , SA.x2 <| String.fromInt x2
        , SA.y2 <| String.fromInt y2
        , SA.strokeLinecap "round"
        , SA.strokeWidth <| String.fromInt strokeWidth
        , SA.stroke color
        ]
        []


bars : H.Html msg
bars =
    Svg.svg
        [ SA.width "20", SA.height "18" ]
        [ line 2 4 18 4
        , line 2 9 18 9
        , line 2 14 18 14
        ]


close : Maybe Int -> Maybe String -> H.Html msg
close maybeWidth maybeColor =
    let
        width =
            Maybe.withDefault defaultWidth maybeWidth

        color =
            Maybe.withDefault defaultColor maybeColor
    in
    Svg.svg [ SA.width "18", SA.height "18" ]
        [ lineCustom width color 5 5 13 13
        , lineCustom width color 5 13 13 5
        ]
