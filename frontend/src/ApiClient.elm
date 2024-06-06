module ApiClient exposing (..)

import Bytes as B
import Bytes.Extra as BE
import Comments as C
import Config
import GameRecord as G
import Http
import LittleGolem as LG


baseUrl =
    Config.apiBaseUrl


type alias SgfResult =
    Result String G.Record


type alias CommentsResult =
    Result String (List C.Comment)



-- General things


resolve : (body -> Result String a) -> Http.Response body -> Result Http.Error a
resolve toResult response =
    -- taken verbatim from Http.resolve
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            Result.mapError Http.BadBody (toResult body)


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
            "Bad boy: " ++ body


decodeErrors : Result Http.Error a -> Result String a
decodeErrors =
    Result.mapError httpErrorToString


sgfResponseToResult : Http.Response B.Bytes -> Result Http.Error String
sgfResponseToResult =
    let
        toStr val =
            if val < 127 then
                Char.fromCode val |> String.fromChar

            else
                "{{" ++ String.fromInt val ++ "}}"
    in
    resolve (BE.toByteValues >> List.map toStr >> String.concat >> Ok)


getLittleGolemSgf : (SgfResult -> msg) -> String -> Cmd msg
getLittleGolemSgf msg gameId =
    Http.get
        { url = baseUrl ++ "/games/lg/" ++ gameId
        , expect =
            Http.expectBytesResponse
                (decodeErrors >> Result.andThen LG.parse >> msg)
                sgfResponseToResult
        }


getComments : (CommentsResult -> msg) -> String -> String -> Cmd msg
getComments msg source gameId =
    Http.get
        { url = baseUrl ++ "/games/" ++ source ++ "/" ++ gameId ++ "/comments"
        , expect = Http.expectJson (decodeErrors >> msg) C.commentsDecoder
        }
