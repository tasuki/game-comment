module GameHelpers exposing (..)

import GameRecord as G
import Html as H
import Html.Events as HE
import Json.Decode as Json
import Svg exposing (Svg)


type alias GameView msg =
    Int -- boardSize
    -> List G.Move -- currentMoves
    -> Maybe G.Coords -- highlight
    -> String -- currentColour
    -> List G.Coords -- children
    -> Maybe G.Move -- lastPlayed
    -> G.Player -- onMove
    -> (G.Coords -> msg) -- playMsg
    -> (G.Coords -> msg) -- existingMsg
    -> Svg msg


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


isLastMove : Maybe G.Move -> G.Coords -> Bool
isLastMove lastMove coords =
    Maybe.map .play lastMove == Just (G.Place coords)


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
