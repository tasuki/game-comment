module Game.TwixT exposing (view)

import Dict exposing (Dict)
import GameHelpers as GH
import GameRecord as G
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


emptyPosition : Position
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
            (i == 1) || (i == size)
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
            -- https://stackoverflow.com/a/62625458/2591473
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
            (p0 * p1 < 0) && (p2 * p3 < 0)

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


mirror : Peg -> Peg
mirror ( player, coords ) =
    ( G.otherPlayer player, { x = coords.y, y = coords.x } )


add : G.Move -> Position -> Position
add { player, play } position =
    case play of
        G.Place coords ->
            { position
                | pegs = ( player, coords ) :: position.pegs
                , links = List.append (newLinks player coords position) position.links
            }

        G.Swap ->
            -- IF a swap can only happen on the second move, there are no links
            { position | pegs = List.map mirror position.pegs }

        _ ->
            position


positionFromMoves : List G.Move -> Position
positionFromMoves moves =
    List.foldl add emptyPosition moves



-- View


background : String -> Int -> List (Svg msg)
background colour size =
    [ Svg.rect
        [ SA.x "0.5"
        , SA.y "0.5"
        , SA.width <| String.fromInt <| size
        , SA.height <| String.fromInt <| size
        , SA.fill colour
        ]
        []
    ]


drawBorders : Int -> List (Svg msg)
drawBorders size =
    let
        length =
            String.fromInt <| size - 3

        otherStartPoint =
            (String.fromInt <| size - 1) ++ ".5"

        border : String -> String -> String -> Svg msg
        border from path color =
            Svg.path
                [ SA.d <| "M" ++ from ++ " " ++ path
                , SA.stroke color
                , SA.strokeWidth ".25"
                , SA.strokeLinecap "square"
                ]
                []
    in
    [ border "2,1.5" ("h" ++ length) "white"
    , border "1.5,2" ("v" ++ length) "black"
    , border ("2," ++ otherStartPoint) ("h" ++ length) "white"
    , border (otherStartPoint ++ ",2") ("v" ++ length) "black"
    ]


drawGuidelines : Int -> List (Svg msg)
drawGuidelines size =
    let
        coord : Int -> String
        coord i =
            String.fromFloat <|
                if i == 0 then
                    2

                else if i == 1 then
                    toFloat (size + 1) / 2.0

                else
                    toFloat size - 1

        guide x1 y1 x2 y2 =
            Svg.line
                [ SA.x1 <| coord x1
                , SA.y1 <| coord y1
                , SA.x2 <| coord x2
                , SA.y2 <| coord y2
                , SA.stroke "#0003"
                , SA.strokeWidth ".1"
                , SA.strokeLinecap "round"
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
    GH.coordList 1 size |> List.filter (isOnBoard size)


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


drawChildren : List G.Coords -> List (Svg msg)
drawChildren children =
    let
        drawChild : G.Coords -> Svg msg
        drawChild coords =
            Svg.circle
                [ SA.cx <| String.fromInt coords.x
                , SA.cy <| String.fromInt coords.y
                , SA.r "0.3"
                , SA.fill "#0003"
                ]
                []
    in
    List.map drawChild children


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


drawPegs : Int -> Position -> Maybe G.Move -> G.Player -> (G.Coords -> msg) -> (G.Coords -> msg) -> List (Svg msg)
drawPegs size position lastMove onMove playMsg existingMsg =
    let
        pegs =
            pegDict position.pegs

        isPlayable : G.Player -> Int -> Bool
        isPlayable player dirCoord =
            onMove == player && dirCoord /= 1 && dirCoord /= size

        viewPeg : G.Coords -> G.Player -> Svg msg
        viewPeg coords player =
            Svg.circle
                [ SA.cx <| String.fromInt coords.x
                , SA.cy <| String.fromInt coords.y
                , SA.r "0.35"
                , SA.stroke "black"
                , SA.strokeWidth "0.1"
                , SA.fill <| G.color player
                , SE.onClick <| existingMsg coords
                ]
                []

        highlightLastMove : G.Coords -> Svg msg
        highlightLastMove coords =
            Svg.circle
                [ SA.cx <| String.fromInt coords.x
                , SA.cy <| String.fromInt coords.y
                , SA.r "0.5"
                , SA.fill "#F33"
                ]
                []

        showPosition : G.Coords -> Svg msg
        showPosition coords =
            case Dict.get ( coords.x, coords.y ) pegs of
                Just player ->
                    if GH.isLastMove lastMove coords then
                        Svg.g []
                            [ highlightLastMove coords
                            , viewPeg coords player
                            ]

                    else
                        viewPeg coords player

                Nothing ->
                    if isPlayable G.Black coords.y || isPlayable G.White coords.x then
                        Svg.circle
                            [ SA.cx <| String.fromInt coords.x
                            , SA.cy <| String.fromInt coords.y
                            , SA.r "0.4"
                            , SA.fill "transparent"
                            , SA.class "clickable"
                            , SE.onClick <| playMsg coords
                            ]
                            []

                    else
                        Svg.g [] []
    in
    List.map showPosition (coordList size)


viewHighlight : Maybe G.Coords -> List (Svg msg)
viewHighlight maybeCoords =
    case maybeCoords of
        Just coords ->
            [ Svg.circle
                [ SA.cx <| String.fromInt coords.x
                , SA.cy <| String.fromInt coords.y
                , SA.fill <| "transparent"
                , SA.r <| "0.35"
                , SA.stroke "#F33"
                , SA.strokeWidth ".13"
                ]
                []
            ]

        Nothing ->
            []


view : GH.GameView msg
view boardSize currentMoves highlight currentColour children lastPlayed onMove playMsg existingMsg =
    let
        position =
            positionFromMoves currentMoves
    in
    Svg.svg
        [ SA.viewBox (GH.floatsToStr [ 0.5, 0.5, toFloat boardSize, toFloat boardSize ]), SA.class "twixt" ]
        (background currentColour boardSize
            ++ drawBorders boardSize
            ++ drawGuidelines boardSize
            ++ drawChildren children
            ++ drawPoints boardSize
            ++ drawLinks position
            ++ drawPegs boardSize position lastPlayed onMove playMsg existingMsg
            ++ viewHighlight highlight
        )
