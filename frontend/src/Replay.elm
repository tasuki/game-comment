module Replay exposing (..)

import Colours as C
import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Replay.GameTree as GT
import Replay.Tree as T


type alias GameView =
    GT.GameView


type alias Replay =
    { name : String
    , record : G.Record
    , gameTree : GameView
    }



-- Replay


emptyReplay : G.Record -> Replay
emptyReplay record =
    { name = G.recordName record
    , record = record
    , gameTree = GT.treeFromRecord record |> GT.treeToGameView
    }


withRecord : G.Record -> Maybe Replay -> Replay
withRecord record maybeReplay =
    case maybeReplay of
        Just r ->
            { r
                | gameTree =
                    GT.withRecord (GT.treeFromRecord record) r.gameTree
            }

        Nothing ->
            emptyReplay record



-- Moves


allMoves : Replay -> List G.Move
allMoves replay =
    T.allValues replay.gameTree


currentMoves : Replay -> List G.Move
currentMoves replay =
    T.currentValues replay.gameTree


lastPlayed : Replay -> Maybe G.Move
lastPlayed replay =
    T.getValue replay.gameTree.focus


currentMoveNumber : Replay -> Int
currentMoveNumber =
    currentMoves >> List.length


onMove : G.Game -> Replay -> G.Player
onMove game =
    currentMoveNumber >> G.onMove game



-- Replay manipulations


playCoords : G.Coords -> Replay -> Replay
playCoords coords replay =
    let
        move : G.Move
        move =
            { player = onMove replay.record.game replay
            , play = G.Place coords
            }
    in
    { replay | gameTree = GT.addOrVisitChild move replay.gameTree }


next : Replay -> Replay
next replay =
    { replay | gameTree = T.lookNext replay.gameTree |> Maybe.withDefault replay.gameTree }


prev : Replay -> Replay
prev replay =
    { replay | gameTree = T.lookPrev replay.gameTree |> Maybe.withDefault replay.gameTree }


start : Replay -> Replay
start replay =
    { replay | gameTree = T.lookStart replay.gameTree }


end : Replay -> Replay
end replay =
    { replay | gameTree = T.lookEnd replay.gameTree }


nextVariation : Replay -> Replay
nextVariation replay =
    { replay | gameTree = T.lookNextVar replay.gameTree }


prevVariation : Replay -> Replay
prevVariation replay =
    { replay | gameTree = T.lookPrevVar replay.gameTree }


jump : GameView -> Replay -> Replay
jump gameTree replay =
    { replay | gameTree = gameTree }



-- VIEW


currentColour : Replay -> String
currentColour replay =
    C.colourMain


chars : List Char
chars =
    String.toList "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"


viewMoveHtml : msg -> String -> Int -> G.Move -> H.Html msg
viewMoveHtml jumpMsg backgroundColour moveNum { player, play } =
    let
        char : Int -> Char
        char coord =
            List.drop (coord - 1) chars |> List.head |> Maybe.withDefault '.'

        moveStr =
            case play of
                G.Place coords ->
                    String.fromChar (char coords.x) ++ String.fromInt coords.y

                G.Swap ->
                    "Swap"

                G.Resign ->
                    "Resign"

        class =
            case player of
                G.Black ->
                    "player black "

                G.White ->
                    "player white "
    in
    H.button
        [ HE.onClick jumpMsg
        , HA.class class
        , HA.style "background-color" backgroundColour
        ]
        [ H.text <| String.fromInt moveNum ++ "." ++ moveStr ]


view : (GameView -> msg) -> (Int -> msg) -> H.Html msg -> Replay -> List (H.Html msg)
view jumpMsg deleteVarMsg gameNav replay =
    [ H.div [ HA.class "player-info" ]
        [ H.div [ HA.class "player black" ] [ H.text replay.record.black ]
        , H.div [ HA.class "player white" ] [ H.text replay.record.white ]
        , H.div [ HA.class "clear" ] []
        ]
    , gameNav
    , H.div [ HA.class "variations" ] []
    ]
