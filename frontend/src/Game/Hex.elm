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


hexWidth : Float
hexWidth =
    sqrt 3


hexHeight : Float
hexHeight =
    2


hexCenter : G.Coords -> ( String, String )
hexCenter coords =
    ( String.fromFloat (toFloat coords.x * hexWidth + ((toFloat coords.y - 1) * hexWidth / 2))
    , String.fromFloat (toFloat coords.y * hexHeight * 0.75)
    )


viewBoardHex : String -> G.Coords -> Svg msg
viewBoardHex colour coords =
    let
        ( centerX, centerY ) =
            hexCenter coords

        center =
            "M " ++ centerX ++ "," ++ centerY
    in
    Svg.path
        [ SA.d <|
            String.concat
                [ center
                , " m 0,-1"
                , " l -0.866,0.5"
                , " l 0,1"
                , " l 0.866,0.5"
                , " l 0.866,-0.5"
                , " l 0,-1"
                , " l -0.866,-0.5"
                ]
        , SA.stroke "#0005"
        , SA.strokeWidth "0.05"
        , SA.fill colour
        ]
        []


type alias BoardSide =
    { color : String
    , start : G.Coords
    , shift : String
    , repeat : String
    , end : String
    }


viewBoardSides : Int -> List (Svg msg)
viewBoardSides size =
    let
        startCenter : G.Coords -> String
        startCenter coords =
            let
                ( centerX, centerY ) =
                    hexCenter coords
            in
            "M " ++ centerX ++ "," ++ centerY

        viewBoardSide : BoardSide -> Svg msg
        viewBoardSide bs =
            Svg.path
                [ SA.d <|
                    String.concat <|
                        [ startCenter bs.start
                        , bs.shift
                        ]
                            ++ List.repeat (size - 1) bs.repeat
                            ++ [ bs.end ]
                , SA.stroke bs.color
                , SA.strokeWidth "0.3"
                , SA.strokeLinecap "round"
                , SA.fill "transparent"
                ]
                []
    in
    List.map viewBoardSide
        [ BoardSide
            "black"
            { x = 1, y = 1 }
            " m -0.866,-0.5 m 0,-0.2"
            " l 0.866,-0.5 l 0.866,0.5"
            " l 0.866,-0.5 l 0.433,0.25"
        , BoardSide
            "black"
            { x = size, y = size }
            " m 0.866,0.5 m 0,0.2"
            " l -0.866,0.5 l -0.866,-0.5"
            " l -0.866,0.5 l -0.433,-0.25"
        , BoardSide
            "white"
            { x = 1, y = 1 }
            " m -0.866,-0.5 m -0.2,0.11"
            " l 0,1 l 0.866,0.5"
            " l 0,1 l 0.433,0.25"
        , BoardSide
            "white"
            { x = size, y = size }
            " m 0.866,0.5 m 0.2,-0.11"
            " l 0,-1 l -0.866,-0.5"
            " l 0,-1 l -0.433,-0.25"
        ]


drawChildren : List G.Coords -> List (Svg msg)
drawChildren children =
    let
        drawChild : G.Coords -> Svg msg
        drawChild coords =
            let
                ( centerX, centerY ) =
                    hexCenter coords
            in
            Svg.circle
                [ SA.cx centerX
                , SA.cy centerY
                , SA.r "0.2"
                , SA.fill "#0003"
                ]
                []
    in
    List.map drawChild children


viewHexes : Position -> Maybe G.Move -> (G.Coords -> msg) -> List G.Coords -> List (Svg msg)
viewHexes position lastMove playMsg =
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


view : GH.GameView msg
view boardSize currentMoves currentColour children lastPlayed onMove playMsg =
    let
        ( boardWidth, boardHeight ) =
            ( hexWidth * 1.5 * toFloat boardSize + 1, hexHeight * toFloat boardSize * 3 / 4 + 1.5 )

        coordList : List G.Coords
        coordList =
            GH.coordList 1 boardSize

        position =
            List.foldl add (emptyPosition boardSize) currentMoves
    in
    Svg.svg
        [ SA.viewBox (GH.floatsToStr [ 0, 0, boardWidth, boardHeight ])
        , SA.class "hex"
        ]
        (background boardWidth boardHeight
            ++ List.map (viewBoardHex <| currentColour) coordList
            ++ viewBoardSides boardSize
            ++ drawChildren children
            ++ viewHexes position lastPlayed playMsg coordList
        )
