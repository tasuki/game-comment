module ApiClient exposing (..)

import Bytes as B
import Bytes.Extra as BE
import Config
import GameRecord as G
import Http
import LittleGolem as LG


baseUrl =
    Config.apiBaseUrl


type alias SgfResult =
    Result String G.Record


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad url: " ++ url

        Http.Timeout ->
            "Timeout error"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad boy"


responseToResult : Http.Response B.Bytes -> Result Http.Error String
responseToResult res =
    let
        toStr val =
            if val < 127 then
                Char.fromCode val |> String.fromChar

            else
                "{{" ++ String.fromInt val ++ "}}"
    in
    case res of
        Http.GoodStatus_ _ bytes ->
            Ok <| String.concat <| List.map toStr <| BE.toByteValues bytes

        _ ->
            Err <| Http.BadBody "Actually discarded error sorry"


decodeResult : Result Http.Error String -> SgfResult
decodeResult result =
    Result.mapError httpErrorToString result |> Result.andThen LG.parse


getLittleGolemSgf : (SgfResult -> msg) -> String -> Cmd msg
getLittleGolemSgf msg gameId =
    Http.get
        { url = baseUrl ++ "/game/" ++ gameId
        , expect = Http.expectBytesResponse (decodeResult >> msg) responseToResult
        }
