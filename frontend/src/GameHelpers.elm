module GameHelpers exposing (..)

import GameRecord as G
import Html as H
import Svg.Attributes as SA


intsToStr : List Int -> String
intsToStr ints =
    List.map String.fromInt ints |> String.join " "


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
