module Colours exposing (..)

import Dict exposing (Dict)
import List.Extra



-- variations
-- based on https://www.hsluv.org/


coloursVars : List String
coloursVars =
    [ "#9AC664" -- [110.0, 70.0, 75.0]
    , "#7AC6BD" -- [180.0, 60.0, 75.0]
    , "#ACB6EB" -- [260.0, 70.0, 75.0]
    , "#D9A8E4" -- [300.0, 60.0, 75.0]
    , "#EDA79D" -- [20.0, 70.0, 75.0]
    , "#D5B565" -- [65.0, 70.0, 75.0]
    ]


colourMain : String
colourMain =
    List.Extra.last coloursVars |> Maybe.withDefault "#CA5"


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

        minOcc : Int
        minOcc =
            minOccurences counts

        isMin : String -> Bool
        isMin colour =
            (Dict.get colour counts |> Maybe.withDefault 0) <= minOcc
    in
    List.Extra.find isMin coloursVars
        |> Maybe.withDefault colourMain
