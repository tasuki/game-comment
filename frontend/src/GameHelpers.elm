module GameHelpers exposing (..)

import GameRecord as G
import Html as H
import Html.Events as HE
import Json.Decode as Json
import Svg.Attributes as SA


intsToStr : List Int -> String
intsToStr ints =
    List.map String.fromInt ints |> String.join " "


floatsToStr : List Float -> String
floatsToStr floats =
    List.map String.fromFloat floats |> String.join " "


coordList : Int -> Int -> List G.Coords
coordList from to =
    let
        nodes =
            List.range from to
    in
    List.concatMap (\x -> List.map (G.Coords x) nodes) nodes


classesProps : Maybe G.Move -> G.Player -> G.Coords -> List (H.Attribute msg)
classesProps lastMove player coords =
    if Maybe.map .play lastMove == Just (G.Place coords) then
        if player == G.Black then
            [ SA.class "last-move black" ]

        else
            [ SA.class "last-move white" ]

    else
        []


onEnter : msg -> H.Attribute msg
onEnter msg =
    -- This better be in some utils thing...
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    HE.on "keydown" (Json.andThen isEnter HE.keyCode)
