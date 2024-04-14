module Replay exposing (..)

import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import List.Extra


type alias Replay =
    { name : String
    , record : G.Record
    , alterable : Bool
    , lookingAt : LookAt
    , variations : List (Variation Moves)
    }


type alias Moves =
    List G.Move


type alias LookAt =
    { variation : Maybe Int
    , move : Int
    }


type alias Variation m =
    { colour : String
    , fromMove : Int
    , moves : m
    }


emptyReplay : G.Record -> Replay
emptyReplay record =
    { name = G.recordName record
    , record = record
    , alterable = True
    , lookingAt = { variation = Nothing, move = 0 }
    , variations = []
    }


withRecord : G.Record -> Maybe Replay -> Replay
withRecord record maybeReplay =
    let
        replay =
            case maybeReplay of
                Just r ->
                    { r | record = record }

                Nothing ->
                    emptyReplay record
    in
    { replay | alterable = False }


lookNext : LookAt -> LookAt
lookNext lookingAt =
    { lookingAt | move = lookingAt.move + 1 }


lookPrev : LookAt -> LookAt
lookPrev lookingAt =
    { lookingAt | move = lookingAt.move - 1 }



-- Variations


emptyVariation : Variation Moves
emptyVariation =
    { colour = "", fromMove = 0, moves = [] }


currentVariation : Replay -> Maybe ( Int, Variation Moves )
currentVariation replay =
    let
        getVarWithNum : Int -> Maybe ( Int, Variation Moves )
        getVarWithNum varNum =
            List.drop varNum replay.variations
                |> List.head
                |> Maybe.map (\var -> ( varNum, var ))
    in
    replay.lookingAt.variation
        |> Maybe.andThen getVarWithNum



-- Moves from replay


allMoves : Replay -> List G.Move
allMoves replay =
    case currentVariation replay of
        Nothing ->
            replay.record.moves

        Just ( _, var ) ->
            List.append
                (List.take var.fromMove replay.record.moves)
                var.moves


currentMoves : Replay -> List G.Move
currentMoves replay =
    allMoves replay |> List.take replay.lookingAt.move


lastMove : Replay -> Maybe G.Move
lastMove replay =
    allMoves replay |> List.take replay.lookingAt.move |> List.Extra.last


nextMove : Replay -> Maybe G.Move
nextMove replay =
    allMoves replay |> List.drop replay.lookingAt.move |> List.head



-- Replay manipulations


replaceAtIndex : Int -> a -> List a -> List a
replaceAtIndex index newElement list =
    let
        maybeReplace i el =
            if i == index then
                newElement

            else
                el
    in
    List.indexedMap maybeReplace list


addMove : G.Move -> Replay -> Replay
addMove move replay =
    let
        chopMoves : Variation Moves -> Moves
        chopMoves var =
            List.take (replay.lookingAt.move - var.fromMove) var.moves

        addMoveToVar : Variation Moves -> Variation Moves
        addMoveToVar var =
            { var | moves = var.moves ++ [ move ] }

        addVariation : Variation Moves -> Replay
        addVariation var =
            { replay
                | lookingAt =
                    { variation = Just <| List.length replay.variations
                    , move = replay.lookingAt.move + 1
                    }
                , variations =
                    replay.variations ++ [ addMoveToVar var ]
            }

        expandVariation : Int -> Variation Moves -> Replay
        expandVariation varNum var =
            { replay
                | lookingAt = lookNext replay.lookingAt
                , variations = replaceAtIndex varNum (addMoveToVar var) replay.variations
            }
    in
    case currentVariation replay of
        Nothing ->
            if replay.alterable && (List.length replay.record.moves == replay.lookingAt.move) then
                -- append move to alterable game record
                { replay
                    | record = G.addMove move replay.record
                    , lookingAt = { variation = Nothing, move = replay.lookingAt.move + 1 }
                }

            else
                -- create new variation
                addVariation { emptyVariation | fromMove = replay.lookingAt.move }

        Just ( varNum, var ) ->
            -- expand preexisting variation, potentially chopping
            expandVariation varNum { var | moves = chopMoves var }


playCoords : G.Coords -> Replay -> Replay
playCoords coords replay =
    let
        move : G.Move
        move =
            { player = G.onMove replay.lookingAt.move replay.record.game
            , play = G.Place coords
            }
    in
    if Just move == nextMove replay then
        next replay

    else
        addMove move replay


next : Replay -> Replay
next replay =
    case nextMove replay of
        Nothing ->
            replay

        Just _ ->
            { replay | lookingAt = lookNext replay.lookingAt }


prev : Replay -> Replay
prev replay =
    let
        lookAtPrev =
            lookPrev replay.lookingAt

        varFromMove : Int
        varFromMove =
            currentVariation replay
                |> Maybe.map (\( _, var ) -> var.fromMove)
                |> Maybe.withDefault 0

        lookingEarlierThanVariation =
            (replay.lookingAt.move - 1) <= varFromMove
    in
    case lastMove replay of
        Nothing ->
            replay

        Just _ ->
            if lookingEarlierThanVariation then
                { replay | lookingAt = { lookAtPrev | variation = Nothing } }

            else
                { replay | lookingAt = lookAtPrev }


start : Replay -> Replay
start replay =
    let
        lookAt =
            replay.lookingAt
    in
    jump { lookAt | move = 0 } replay


end : Replay -> Replay
end replay =
    case nextMove replay of
        Nothing ->
            replay

        Just _ ->
            end { replay | lookingAt = lookNext replay.lookingAt }


jump : LookAt -> Replay -> Replay
jump lookAt replay =
    { replay | lookingAt = lookAt }



-- VIEW


chars : List Char
chars =
    String.toList "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"


viewMoveHtml : msg -> Bool -> Int -> G.Move -> List (H.Html msg)
viewMoveHtml jumpMsg highlight moveNum { player, play } =
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

        highlightClass =
            if highlight then
                "highlight "

            else
                ""
    in
    [ H.button
        [ HE.onClick jumpMsg, HA.class (class ++ highlightClass) ]
        [ H.text <| String.fromInt moveNum ++ "." ++ moveStr ]
    ]


viewMove : (LookAt -> msg) -> Int -> Variation Moves -> Replay -> Int -> G.Move -> List (H.Html msg)
viewMove jumpMsg varNum var replay i =
    let
        moveNum =
            var.fromMove + i

        highlight =
            (replay.lookingAt.variation == Just varNum)
                && (replay.lookingAt.move == var.fromMove + i)
    in
    viewMoveHtml
        (jumpMsg { variation = Just varNum, move = moveNum })
        highlight
        moveNum


view : (LookAt -> msg) -> H.Html msg -> Replay -> List (H.Html msg)
view jumpMsg gameNav replay =
    let
        viewVar : Int -> Variation Moves -> List (H.Html msg)
        viewVar varNum var =
            List.indexedMap (\i -> viewMove jumpMsg varNum var replay (i + 1)) var.moves
                |> List.concat
    in
    [ H.div [ HA.class "player-info" ]
        [ H.div [ HA.class "player black" ] [ H.text replay.record.black ]
        , H.div [ HA.class "player white" ] [ H.text replay.record.white ]
        , H.div [ HA.class "clear" ] []
        ]
    , gameNav
    , H.div [ HA.class "replay" ] (List.indexedMap viewVar replay.variations |> List.concat)
    ]
