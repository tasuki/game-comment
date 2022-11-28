module TwixT exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import GameRecord as R
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE


type alias Peg =
    ( R.Player, R.Coords )


type alias Link =
    ( R.Player, ( R.Coords, R.Coords ) )


type alias Position =
    { pegs : List Peg
    , links : List Link
    }


type alias Replay =
    { moves : Array R.Play
    , currentMove : Int
    , currentPosition : Position
    }


pegDict : List Peg -> Dict ( Int, Int ) R.Player
pegDict pegs =
    List.map (\( player, coords ) -> ( ( coords.x, coords.y ), player )) pegs
        |> Dict.fromList


isOnBoard : Int -> R.Coords -> Bool
isOnBoard size coord =
    let
        isBorder : Int -> Bool
        isBorder i =
            (i == 0) || (i == (size - 1))
    in
    not (isBorder coord.x) || not (isBorder coord.y)


newLinks : R.Player -> R.Coords -> Position -> List Link
newLinks player newPeg position =
    let
        linkCoords : List R.Coords
        linkCoords =
            [ R.Coords 1 2
            , R.Coords 2 1
            , R.Coords 2 -1
            , R.Coords 1 -2
            , R.Coords -1 -2
            , R.Coords -2 -1
            , R.Coords -2 1
            , R.Coords -1 2
            ]

        pegs =
            pegDict position.pegs

        shouldCreateLink : R.Coords -> Bool
        shouldCreateLink linkTo =
            Dict.get ( linkTo.x, linkTo.y ) pegs == Just player

        a : List R.Coords
        a =
            List.map (\l -> R.Coords (newPeg.x + l.x) (newPeg.y + l.y)) linkCoords |> List.filter shouldCreateLink
    in
    List.map (\to -> ( player, ( newPeg, to ) )) a


updatePosition : R.Play -> Position -> Position
updatePosition { player, move } position =
    case move of
        R.Place coords ->
            { position
                | pegs = ( player, coords ) :: position.pegs
                , links = List.append (newLinks player coords position) position.links
            }

        _ ->
            position


play : R.Coords -> Replay -> Replay
play coords replay =
    let
        move =
            { player = R.onMove replay.moves, move = R.Place coords }
    in
    { replay
        | moves = Array.append replay.moves (Array.fromList <| List.singleton move)
        , currentPosition = updatePosition move replay.currentPosition
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


coordList : Int -> List R.Coords
coordList size =
    let
        nodes : List Int
        nodes =
            List.range 0 (size - 1)
    in
    List.concatMap (\x -> List.map (R.Coords x) nodes) nodes
        |> List.filter (isOnBoard size)


drawPoints : Int -> List (Svg msg)
drawPoints size =
    let
        toHole : R.Coords -> Svg msg
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


drawPegs : Int -> Position -> R.Player -> (R.Coords -> msg) -> List (Svg msg)
drawPegs size position onMove playMsg =
    let
        pegs =
            pegDict position.pegs

        coordProps coords =
            [ SA.cx <| String.fromInt coords.x, SA.cy <| String.fromInt coords.y ]

        isClickable : R.Player -> Int -> Bool
        isClickable player dirCoord =
            onMove == player && dirCoord /= 0 && dirCoord /= (size - 1)

        styleProps coords =
            case Dict.get ( coords.x, coords.y ) pegs of
                Nothing ->
                    if isClickable R.Black coords.y || isClickable R.White coords.x then
                        [ SA.r "0.4", SA.fill "transparent", SA.class "clickable", SE.onClick <| playMsg coords ]

                    else
                        []

                Just player ->
                    [ SA.r "0.3", SA.stroke "black", SA.strokeWidth "0.1", SA.fill <| R.color player ]

        drawCoords : R.Coords -> Svg msg
        drawCoords coords =
            Svg.circle (coordProps coords ++ styleProps coords) []
    in
    List.map drawCoords (coordList size)


view : R.Record -> Replay -> (R.Coords -> msg) -> List (Svg msg)
view record replay playMsg =
    let
        size =
            record.size

        onMove : R.Player
        onMove =
            R.onMove replay.moves
    in
    background size
        ++ drawBorders size
        ++ drawGuidelines size
        ++ drawPoints size
        ++ drawLinks replay.currentPosition
        ++ drawPegs size replay.currentPosition onMove playMsg
