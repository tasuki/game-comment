module ApiClient exposing (..)

import GameRecord as G
import Http
import LittleGolem as LG


baseUrl =
    "http://localhost:6483" -- TODO!


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
            "Bad status: " ++ (String.fromInt status)

        Http.BadBody body ->
            "Bad boy"


decodeResult : Result Http.Error String -> SgfResult
decodeResult result =
    Result.mapError httpErrorToString result |> Result.andThen LG.parse


getLittleGolemSgf : (SgfResult -> msg) -> String -> Cmd msg
getLittleGolemSgf msg gameId =
    Http.get
        { url = baseUrl ++ "/game/" ++ gameId
        , expect = Http.expectString (decodeResult >> msg)
        }
