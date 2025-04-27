module Replay exposing (..)

import Colours as C
import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import List.Extra
import Replay.Tree as T


type alias GameView =
    T.Zipper G.Move


type alias Replay =
    { record : G.Record
    , gameTree : GameView
    }



-- GameView


addOrVisitChild : G.Move -> GameView -> GameView
addOrVisitChild move zipper =
    let
        ( index, newView ) =
            case T.findChildIndex move zipper of
                Just i ->
                    ( i, zipper )

                Nothing ->
                    T.addChild move zipper
    in
    T.descendToIndex index newView
        |> Maybe.withDefault zipper


treeWithRecord : List G.Move -> GameView -> GameView
treeWithRecord record zipper =
    let
        replaced : T.Zipper G.Move
        replaced =
            T.makeTree zipper
                |> T.replaceFirstVar (Nothing :: List.map Just record)
                |> T.makeZipper

        goToMove : Int -> Maybe GameView -> Maybe GameView
        goToMove num mz =
            if num <= 0 then
                mz

            else
                goToMove (num - 1) (Maybe.andThen T.descend mz)
    in
    goToMove (T.currentValues zipper |> List.length) (Just replaced)
        |> Maybe.withDefault zipper



-- Moves


allMoves : Replay -> List G.Move
allMoves replay =
    T.allValues replay.gameTree


currentMoves : Replay -> List G.Move
currentMoves replay =
    T.currentValues replay.gameTree


currentMoveNumber : Replay -> Int
currentMoveNumber =
    currentMoves >> List.length


dropCommonPart : Int -> List a -> List a -> ( Int, List a )
dropCommonPart dropped compareTo dropFrom =
    case ( compareTo, dropFrom ) of
        ( hct :: tct, hdf :: tdf ) ->
            if hct == hdf then
                dropCommonPart (dropped + 1) tct tdf

            else
                ( dropped, dropFrom )

        _ ->
            ( dropped, dropFrom )


currentVariation : Replay -> ( Int, List G.Move )
currentVariation replay =
    dropCommonPart 1 replay.record.moves (currentMoves replay)


isInMainVar : Replay -> Bool
isInMainVar replay =
    dropCommonPart 1 replay.record.moves (currentMoves replay)
        |> Tuple.second
        |> List.length
        |> (==) 0


findMoveNumber : G.Coords -> List G.Move -> Maybe Int
findMoveNumber coords moves =
    let
        matches : G.Move -> Bool
        matches m =
            case m.play of
                G.Place c ->
                    c == coords

                _ ->
                    False
    in
    moves
        |> List.Extra.findIndex matches
        |> Maybe.map ((+) 1)


lastPlayed : Replay -> Maybe G.Move
lastPlayed replay =
    T.getValue replay.gameTree.focus


onMove : G.Game -> Replay -> G.Player
onMove game =
    currentMoveNumber >> G.onMove game


children : Replay -> List G.Coords
children replay =
    T.getChildren replay.gameTree.focus
        |> List.filterMap T.getValue
        |> List.filterMap G.moveCoords



-- Replay


emptyReplay : G.Record -> Replay
emptyReplay record =
    { record = record
    , gameTree = T.createLockedTree record.moves |> T.makeZipper
    }


withRecord : G.Record -> Maybe Replay -> Replay
withRecord record maybeReplay =
    case maybeReplay of
        Just r ->
            { record = record
            , gameTree = treeWithRecord record.moves r.gameTree
            }

        Nothing ->
            emptyReplay record


playCoords : G.Coords -> Replay -> Replay
playCoords coords replay =
    let
        move : G.Move
        move =
            { player = onMove replay.record.game replay
            , play = G.Place coords
            }
    in
    { replay | gameTree = addOrVisitChild move replay.gameTree }


next : Replay -> Replay
next replay =
    { replay | gameTree = T.descend replay.gameTree |> Maybe.withDefault replay.gameTree }


prev : Replay -> Replay
prev replay =
    { replay | gameTree = T.ascend replay.gameTree |> Maybe.withDefault replay.gameTree }


start : Replay -> Replay
start replay =
    { replay | gameTree = T.ascendStart replay.gameTree }


end : Replay -> Replay
end replay =
    { replay | gameTree = T.descendEnd replay.gameTree }


nextVariation : Replay -> Replay
nextVariation replay =
    { replay | gameTree = T.nextVariation replay.gameTree }


prevVariation : Replay -> Replay
prevVariation replay =
    { replay | gameTree = T.prevVariation replay.gameTree }


cutVariation : Replay -> Replay
cutVariation replay =
    if T.isLocked replay.gameTree then
        replay

    else
        { replay | gameTree = T.cutVariation replay.gameTree }


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
