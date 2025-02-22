module ApiClient exposing (..)

import Bytes as B
import Bytes.Extra as BE
import Comments as C
import Config
import GameRecord as G
import Http
import Json.Decode as D
import LittleGolem as LG
import User as U


baseUrl =
    Config.apiBaseUrl


type alias Error =
    { msg : String }


errorDecoder : D.Decoder Error
errorDecoder =
    D.map Error (D.field "msg" D.string)


emptyDecoder : D.Decoder ()
emptyDecoder =
    D.succeed ()


type alias UserCreatedResult =
    Result Error ()


type alias SessionResult =
    Result Error U.SessionData


type alias SgfResult =
    Result String G.Record


type alias CommentsResult =
    Result String (List C.CommentResponse)



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
            "Oops: " ++ body


decodeStatusError : Result Http.Error a -> Result String a
decodeStatusError =
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


handleError : (Result Error a -> msg) -> Result Http.Error a -> msg
handleError msg result =
    case result of
        Ok a ->
            msg (Ok a)

        Err httpError ->
            msg (Err { msg = httpErrorToString httpError })


expectMaybeError : (Result Http.Error a -> msg) -> D.Decoder a -> D.Decoder Error -> Http.Expect msg
expectMaybeError toMsg decoder ed =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    case D.decodeString ed body of
                        Ok value ->
                            Err (Http.BadBody value.msg)

                        Err _ ->
                            Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case D.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err <| Http.BadBody (D.errorToString err)



-- Endpoints


createUser : (UserCreatedResult -> msg) -> U.CreateUser -> Cmd msg
createUser msg userData =
    Http.post
        { url = baseUrl ++ "/users"
        , body = Http.jsonBody <| U.createUserEncoder userData
        , expect = expectMaybeError (handleError msg) emptyDecoder errorDecoder
        }


createSession : (SessionResult -> msg) -> U.CreateSession -> Cmd msg
createSession msg createSessionData =
    Http.post
        { url = baseUrl ++ "/sessions"
        , body = Http.jsonBody <| U.createSessionEncoder createSessionData
        , expect = expectMaybeError (handleError msg) U.sessionDataDecoder errorDecoder
        }


getSgf : (SgfResult -> msg) -> G.GameSource -> Cmd msg
getSgf msg gameSource =
    let
        (G.GameSource source gameId) =
            gameSource
    in
    Http.get
        { url = baseUrl ++ "/games/" ++ source ++ "/" ++ gameId
        , expect =
            Http.expectBytesResponse
                (decodeStatusError >> Result.andThen (LG.parse gameSource) >> msg)
                sgfResponseToResult
        }


getComments : (CommentsResult -> msg) -> G.GameSource -> Cmd msg
getComments msg gameSource =
    let
        (G.GameSource source gameId) =
            gameSource
    in
    Http.get
        { url = baseUrl ++ "/games/" ++ source ++ "/" ++ gameId ++ "/comments"
        , expect = Http.expectJson (decodeStatusError >> msg) C.commentsDecoder
        }
