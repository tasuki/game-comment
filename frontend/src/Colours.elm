module Colours exposing (..)

import Dict exposing (Dict)
import List.Extra



-- variations
-- based on https://www.hsluv.org/
-- H ?, S 75.7, L 71.0


colourMain : String
colourMain =
    -- H  63.4
    "#CCAA55"


coloursVars : List String
coloursVars =
    [ "#81BE54" -- H 116
    , "#5BBDBB" -- H 190
    , "#9EABEC" -- H 260
    , "#E58FEC" -- H 304
    , "#F09780" -- H  24
    ]


varsDict : Dict String Int
varsDict =
    List.foldl (\key dict -> Dict.insert key 0 dict) Dict.empty coloursVars


updateCount : Maybe Int -> Maybe Int
updateCount count =
    case count of
        Just n ->
            Just (n + 1)

        Nothing ->
            Just 1


countStrings : List String -> Dict String Int
countStrings strings =
    List.foldl
        (\str dict -> Dict.update str updateCount dict)
        varsDict
        strings


minOccurences : Dict String Int -> Int
minOccurences stringCounts =
    Dict.toList stringCounts
        |> List.map (\( _, count ) -> count)
        |> List.minimum
        |> Maybe.withDefault 0


pickNext : List String -> String
pickNext existing =
    let
        counts : Dict String Int
        counts =
            countStrings existing

        _ =
            Debug.log "counts" counts

        minOcc : Int
        minOcc =
            minOccurences counts

        _ =
            Debug.log "minOcc" minOcc

        isMin : String -> Bool
        isMin colour =
            (Dict.get colour counts |> Maybe.withDefault 0) <= minOcc
    in
    List.Extra.find isMin coloursVars
        |> Maybe.withDefault colourMain
