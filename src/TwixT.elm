module TwixT exposing (..)

import Dict exposing (Dict)
import GameRecord as G
import Replay as R
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE


type alias Peg =
    ( G.Player, G.Coords )


type alias Link =
    ( G.Player, ( G.Coords, G.Coords ) )


type alias Position =
    { pegs : List Peg
    , links : List Link
    }


emptyPosition =
    { pegs = []
    , links = []
    }


pegDict : List Peg -> Dict ( Int, Int ) G.Player
pegDict pegs =
    List.map (\( player, coords ) -> ( ( coords.x, coords.y ), player )) pegs
        |> Dict.fromList


isOnBoard : Int -> G.Coords -> Bool
isOnBoard size coord =
    let
        isBorder : Int -> Bool
        isBorder i =
            (i == 0) || (i == (size - 1))
    in
    not (isBorder coord.x) || not (isBorder coord.y)


newLinks : G.Player -> G.Coords -> Position -> List Link
newLinks player newPeg position =
    let
        linkCoords : List G.Coords
        linkCoords =
            [ G.Coords 1 2
            , G.Coords 2 1
            , G.Coords 2 -1
            , G.Coords 1 -2
            , G.Coords -1 -2
            , G.Coords -2 -1
            , G.Coords -2 1
            , G.Coords -1 2
            ]

        pegs =
            pegDict position.pegs

        hasOwnPeg : G.Coords -> Bool
        hasOwnPeg linkTo =
            Dict.get ( linkTo.x, linkTo.y ) pegs == Just player

        toCreate : List G.Coords
        toCreate =
            List.map (\l -> G.Coords (newPeg.x + l.x) (newPeg.y + l.y)) linkCoords
                |> List.filter hasOwnPeg

        crosses : Link -> Link -> Bool
        crosses new old =
            let
                ( newA, newB ) =
                    Tuple.second new

                ( oldA, oldB ) =
                    Tuple.second old

                dxNew =
                    newB.x - newA.x

                dyNew =
                    newB.y - newA.y

                dxOld =
                    oldB.x - oldA.x

                dyOld =
                    oldB.y - oldA.y

                p0 =
                    dyOld * (oldB.x - newA.x) - dxOld * (oldB.y - newA.y)

                p1 =
                    dyOld * (oldB.x - newB.x) - dxOld * (oldB.y - newB.y)

                p2 =
                    dyNew * (newB.x - oldA.x) - dxNew * (newB.y - oldA.y)

                p3 =
                    dyNew * (newB.x - oldB.x) - dxNew * (newB.y - oldB.y)
            in
            (p0 * p1 <= 0) && (p2 * p3 <= 0)

        crossesOpponentLink : Link -> Bool
        crossesOpponentLink new =
            let
                opponentLinks =
                    List.filter (\( linkOwner, _ ) -> linkOwner /= player) position.links
            in
            List.map (crosses new) opponentLinks |> List.any identity
    in
    List.map (\to -> ( player, ( newPeg, to ) )) toCreate
        |> List.filter (\l -> not <| crossesOpponentLink l)


add : G.Play -> Position -> Position
add { player, move } position =
    case move of
        G.Place coords ->
            { position
                | pegs = ( player, coords ) :: position.pegs
                , links = List.append (newLinks player coords position) position.links
            }

        _ ->
            position


remove : G.Play -> Position -> Position
remove { player, move } position =
    let
        shouldKeep : G.Coords -> Link -> Bool
        shouldKeep coords ( _, ( fst, snd ) ) =
            coords /= fst && coords /= snd
    in
    case move of
        G.Place coords ->
            { position
                | pegs = List.drop 1 position.pegs
                , links = List.filter (shouldKeep coords) position.links
            }

        _ ->
            position



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
                [ SA.d <| "M" ++ from ++ " " ++ path
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
        coord : Int -> String
        coord i =
            String.fromFloat <|
                if i == 0 then
                    1

                else if i == 1 then
                    toFloat (size - 1) / 2.0

                else
                    toFloat size - 2

        guide x1 y1 x2 y2 =
            Svg.line
                [ SA.x1 <| coord x1
                , SA.y1 <| coord y1
                , SA.x2 <| coord x2
                , SA.y2 <| coord y2
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


coordList : Int -> List G.Coords
coordList size =
    let
        nodes : List Int
        nodes =
            List.range 0 (size - 1)
    in
    List.concatMap (\x -> List.map (G.Coords x) nodes) nodes
        |> List.filter (isOnBoard size)


drawPoints : Int -> List (Svg msg)
drawPoints size =
    let
        toHole : G.Coords -> Svg msg
        toHole coords =
            Svg.circle
                [ SA.cx <| String.fromInt coords.x
                , SA.cy <| String.fromInt coords.y
                , SA.r "0.1"
                ]
                []
    in
    List.map toHole (coordList size)


drawLinks : Position -> List (Svg msg)
drawLinks position =
    let
        drawLink : Link -> Svg msg
        drawLink ( _, ( from, to ) ) =
            Svg.line
                [ SA.x1 <| String.fromInt from.x
                , SA.y1 <| String.fromInt from.y
                , SA.x2 <| String.fromInt to.x
                , SA.y2 <| String.fromInt to.y
                , SA.stroke "black"
                , SA.strokeWidth ".1"
                ]
                []
    in
    List.map drawLink position.links


drawPegs : Int -> Position -> G.Player -> (G.Coords -> msg) -> List (Svg msg)
drawPegs size position onMove playMsg =
    let
        pegs =
            pegDict position.pegs

        coordProps coords =
            [ SA.cx <| String.fromInt coords.x, SA.cy <| String.fromInt coords.y ]

        isClickable : G.Player -> Int -> Bool
        isClickable player dirCoord =
            onMove == player && dirCoord /= 0 && dirCoord /= (size - 1)

        styleProps coords =
            case Dict.get ( coords.x, coords.y ) pegs of
                Nothing ->
                    if isClickable G.Black coords.y || isClickable G.White coords.x then
                        [ SA.r "0.4", SA.fill "transparent", SA.class "clickable", SE.onClick <| playMsg coords ]

                    else
                        []

                Just player ->
                    [ SA.r "0.3", SA.stroke "black", SA.strokeWidth "0.1", SA.fill <| G.color player ]

        drawCoords : G.Coords -> Svg msg
        drawCoords coords =
            Svg.circle (coordProps coords ++ styleProps coords) []
    in
    List.map drawCoords (coordList size)


view : R.Replay Position -> (G.Coords -> msg) -> List (Svg msg)
view replay playMsg =
    let
        size =
            replay.record.size
    in
    background size
        ++ drawBorders size
        ++ drawGuidelines size
        ++ drawPoints size
        ++ drawLinks replay.currentPosition
        ++ drawPegs size replay.currentPosition (G.onMove replay.moves) playMsg
