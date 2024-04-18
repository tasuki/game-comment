module Replay exposing (..)

import Colours as C
import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import List.Extra
import SvgImages as SI


type alias Replay =
    { name : String
    , record : G.Record
    , alterable : Bool
    , lookingAt : LookAt
    , variations : List (Variation Moves)
    }


type alias Moves =
    List G.Move


type alias IndexedMove =
    ( Int, G.Move )


type alias IndexedMoves =
    List IndexedMove


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


allVariations : Replay -> List (Variation IndexedMoves)
allVariations replay =
    let
        getIndexedMoves : Variation Moves -> IndexedMoves
        getIndexedMoves var =
            List.indexedMap (\i move -> ( var.fromMove + i + 1, move )) var.moves
    in
    { colour = ""
    , fromMove = 0
    , moves = List.indexedMap (\i m -> ( i + 1, m )) replay.record.moves
    }
        :: List.map
            (\var ->
                { colour = var.colour
                , fromMove = var.fromMove
                , moves = getIndexedMoves var
                }
            )
            replay.variations


variationsWithCurrentMove : Replay -> List Int
variationsWithCurrentMove replay =
    let
        hasMove : ( Int, Variation IndexedMoves ) -> Bool
        hasMove ( _, var ) =
            List.any (\( moveNum, _ ) -> moveNum == replay.lookingAt.move) var.moves
    in
    allVariations replay
        |> List.indexedMap Tuple.pair
        |> List.filter hasMove
        |> List.map (\( i, _ ) -> i)



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
                addVariation
                    { emptyVariation
                        | fromMove = replay.lookingAt.move
                        , colour = C.pickNext <| List.map (\v -> v.colour) replay.variations
                    }

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


goToFirstVarIfNoMain : Replay -> Replay
goToFirstVarIfNoMain replay =
    let
        eligibleVar : Maybe Int
        eligibleVar =
            List.indexedMap Tuple.pair replay.variations
                |> List.filter (\( _, var ) -> var.fromMove == replay.lookingAt.move)
                |> List.head
                |> Maybe.map (\( varNum, _ ) -> varNum)
    in
    case ( replay.lookingAt.variation, eligibleVar ) of
        ( Nothing, Just eligible ) ->
            -- we're at the end of main line and there's a continuing variation
            { replay
                | lookingAt =
                    { variation = Just <| eligible
                    , move = replay.lookingAt.move + 1
                    }
            }

        _ ->
            replay


next : Replay -> Replay
next replay =
    case nextMove replay of
        Nothing ->
            goToFirstVarIfNoMain replay

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


varToNum : Maybe ( Int, Variation a ) -> Int
varToNum var =
    case var of
        Nothing ->
            0

        Just ( i, _ ) ->
            i + 1


numToVar : Int -> Maybe Int
numToVar num =
    case num of
        0 ->
            Nothing

        i ->
            Just <| i - 1


switchVariation : (List Int -> List Int) -> (Int -> Int -> Bool) -> Replay -> Replay
switchVariation listOp dropIf replay =
    let
        nextVarWithMove : Maybe Int
        nextVarWithMove =
            variationsWithCurrentMove replay
                |> List.sort
                |> listOp
                |> List.Extra.dropWhile (\vwcm -> dropIf vwcm (varToNum <| currentVariation replay))
                |> List.head
    in
    case nextVarWithMove of
        Just nvwm ->
            { replay | lookingAt = { variation = numToVar nvwm, move = replay.lookingAt.move } }

        Nothing ->
            replay


nextVariation : Replay -> Replay
nextVariation =
    switchVariation identity (<=)


prevVariation : Replay -> Replay
prevVariation =
    switchVariation List.reverse (>=)


removeVar : Int -> Replay -> List (Variation Moves)
removeVar varNum replay =
    List.take varNum replay.variations
        ++ List.drop (varNum + 1) replay.variations


deleteCurrentVariation : Replay -> Replay
deleteCurrentVariation replay =
    case currentVariation replay of
        Just ( varNum, var ) ->
            { replay
                | variations = removeVar varNum replay
                , lookingAt =
                    { variation = Nothing
                    , move = var.fromMove
                    }
            }

        _ ->
            replay


deleteVariation : Int -> Replay -> Replay
deleteVariation varNum replay =
    let
        lookAt =
            case currentVariation replay of
                Just ( vn, var ) ->
                    if vn == varNum then
                        { variation = Nothing, move = var.fromMove }

                    else
                        replay.lookingAt

                _ ->
                    replay.lookingAt
    in
    { replay
        | variations = removeVar varNum replay
        , lookingAt = lookAt
    }


jump : LookAt -> Replay -> Replay
jump lookAt replay =
    { replay | lookingAt = lookAt }



-- VIEW


currentColour : Replay -> String
currentColour replay =
    case currentVariation replay of
        Just ( _, var ) ->
            var.colour

        Nothing ->
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


view : (LookAt -> msg) -> (Int -> msg) -> H.Html msg -> Replay -> List (H.Html msg)
view jumpMsg deleteVarMsg gameNav replay =
    let
        lookAt : LookAt
        lookAt =
            replay.lookingAt

        backgroundColour : String -> Int -> Int -> String
        backgroundColour varColour varNum moveNum =
            if (lookAt.variation == Just varNum) && (lookAt.move == moveNum) then
                varColour

            else
                "transparent"

        viewMove : String -> Int -> Int -> G.Move -> H.Html msg
        viewMove varColour varNum moveNum =
            viewMoveHtml
                (jumpMsg { variation = Just varNum, move = moveNum })
                (backgroundColour varColour varNum moveNum)
                moveNum

        viewVar : Int -> Variation Moves -> H.Html msg
        viewVar varNum var =
            H.div [ HA.class "variation", HA.style "border-color" var.colour ]
                (List.indexedMap (\i -> viewMove var.colour varNum (var.fromMove + i + 1)) var.moves
                    ++ [ H.button
                            [ HA.class "close", HE.onClick <| deleteVarMsg varNum ]
                            [ SI.close (Just 4) (Just var.colour) ]
                       ]
                )
    in
    [ H.div [ HA.class "player-info" ]
        [ H.div [ HA.class "player black" ] [ H.text replay.record.black ]
        , H.div [ HA.class "player white" ] [ H.text replay.record.white ]
        , H.div [ HA.class "clear" ] []
        ]
    , gameNav
    , H.div [ HA.class "variations" ] (List.indexedMap viewVar replay.variations)
    ]
