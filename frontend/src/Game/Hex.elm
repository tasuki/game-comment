module Game.Hex exposing (..)

import Array exposing (Array)
import GameHelpers as GH
import GameRecord as G
import Html as H
import Replay as R
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE


type alias Hex =
    Maybe G.Player


type alias Hexes =
    Array Hex


type alias Position =
    { size : Int
    , hexes : Hexes
    }


emptyPosition : Int -> Position
emptyPosition size =
    { size = size
    , hexes = Array.initialize (size * size) (always Nothing)
    }


getHex : G.Coords -> Position -> Hex
getHex coords position =
    Array.get ((coords.x - 1) + (coords.y - 1) * position.size) position.hexes
        |> Maybe.withDefault Nothing


setHex : Hex -> G.Coords -> Position -> Position
setHex hex coords position =
    { position
        | hexes =
            Array.set
                ((coords.x - 1) + (coords.y - 1) * position.size)
                hex
                position.hexes
    }


mirror : Position -> Position
mirror position =
    let
        flip : Hex -> Hex
        flip =
            Maybe.map G.otherPlayer

        setMirrored : G.Coords -> Position -> Position
        setMirrored coords =
            setHex (getHex { x = coords.y, y = coords.x } position |> flip) coords
    in
    List.foldl setMirrored (emptyPosition position.size) (GH.coordList 1 position.size)


add : G.Move -> Position -> Position
add { player, play } position =
    case play of
        G.Place coords ->
            setHex (Just player) coords position

        G.Swap ->
            mirror position

        _ ->
            position



-- VIEW


background : Float -> Float -> List (Svg msg)
background width height =
    [ Svg.rect
        [ SA.x "0"
        , SA.y "0"
        , SA.width <| String.fromFloat <| width
        , SA.height <| String.fromFloat <| height
        , SA.fill "#CCC"
        ]
        []
    ]


hexWidth =
    -- 2 * sqrt 3
    1.732


hexHeight =
    2


hexCenter : G.Coords -> ( String, String )
hexCenter coords =
    ( String.fromFloat (toFloat coords.x * hexWidth + ((toFloat coords.y - 1) * hexWidth / 2))
    , String.fromFloat (toFloat coords.y * hexHeight * 0.75)
    )


viewBoardHex : G.Coords -> Svg msg
viewBoardHex coords =
    let
        h2 =
            String.fromFloat <| hexHeight / 2

        h4 =
            String.fromFloat <| hexHeight / 4

        w2 =
            String.fromFloat <| hexWidth / 2

        ( centerX, centerY ) =
            hexCenter coords

        center =
            "M " ++ centerX ++ "," ++ centerY
    in
    Svg.path
        [ SA.d <|
            String.concat <|
                [ center, " m 0,-", h2 ]
                    ++ [ " l -", w2, ",", h4 ]
                    ++ [ " l 0,", h2 ]
                    ++ [ " l ", w2, ",", h4 ]
                    ++ [ " l ", w2, ",-", h4 ]
                    ++ [ " l 0,-", h2 ]
                    ++ [ " l -", w2, ",-", h4 ]
        , SA.stroke "#333"
        , SA.strokeWidth "0.05"
        , SA.fill "transparent"
        ]
        []


viewHexes : Position -> Maybe G.Move -> G.Player -> (G.Coords -> msg) -> List G.Coords -> List (Svg msg)
viewHexes position lastMove onMove playMsg =
    let
        viewHex : String -> String -> G.Player -> List (H.Attribute msg) -> Svg msg
        viewHex x y player attrs =
            Svg.circle
                (attrs
                    ++ [ SA.cx x
                       , SA.cy y
                       , SA.r "0.7"
                       , SA.stroke "black"
                       , SA.strokeWidth "0.1"
                       , SA.fill <| G.color player
                       ]
                )
                []

        viewEmpty : String -> String -> msg -> Svg msg
        viewEmpty x y msg =
            Svg.circle
                [ SA.cx x
                , SA.cy y
                , SA.r "0.7"
                , SA.fill "transparent"
                , SE.onClick <| msg
                ]
                []

        showPosition : G.Coords -> Svg msg
        showPosition coords =
            let
                ( centerX, centerY ) =
                    hexCenter coords
            in
            case getHex coords position of
                Just player ->
                    viewHex centerX centerY player (GH.classesProps lastMove player coords)

                Nothing ->
                    viewEmpty centerX centerY (playMsg coords)
    in
    List.map showPosition


view : R.Replay -> (G.Coords -> msg) -> Svg msg
view replay playMsg =
    let
        size =
            toFloat replay.record.size

        ( boardWidth, boardHeight ) =
            ( hexWidth * 1.5 * size + 1, hexHeight * size * 3 / 4 + 1.5 )

        coordList : List G.Coords
        coordList =
            GH.coordList 1 replay.record.size

        position =
            List.foldl add (emptyPosition replay.record.size) (R.currentMoves replay)
    in
    Svg.svg
        [ SA.viewBox (GH.floatsToStr [ 0, 0, boardWidth, boardHeight ])
        , SA.class "hex"
        ]
        (background boardWidth boardHeight
            ++ List.map viewBoardHex coordList
            ++ viewHexes
                position
                (R.lastMove replay)
                (G.onMove replay.lookingAt.move G.ToroidGo)
                playMsg
                coordList
        )
