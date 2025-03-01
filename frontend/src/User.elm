module User exposing (..)

import Json.Decode as D
import Json.Encode as E
import Jwt
import List.Extra
import Random


type alias CreateUser =
    { username : String
    , password : String
    , favorite : String
    , email : Maybe String
    }


createUserEncoder : CreateUser -> E.Value
createUserEncoder createUser =
    E.object
        [ ( "username", E.string createUser.username )
        , ( "password", E.string createUser.password )
        , ( "favorite", E.string createUser.favorite )
        , ( "email", createUser.email |> Maybe.map E.string |> Maybe.withDefault E.null )
        ]


type alias CreateSession =
    { username : String
    , password : String
    }


createSessionEncoder : CreateSession -> E.Value
createSessionEncoder createSession =
    E.object
        [ ( "username", E.string createSession.username )
        , ( "password", E.string createSession.password )
        ]


type alias SessionData =
    { authToken : String
    }


sessionDataDecoder : D.Decoder SessionData
sessionDataDecoder =
    D.map SessionData
        (D.field "authToken" D.string)


type alias TokenData =
    { id : Int
    , username : String
    }


tokenDecoder : D.Decoder TokenData
tokenDecoder =
    D.map2 TokenData
        (D.field "id" D.int)
        (D.field "username" D.string)


type alias User =
    { name : String
    , token : String
    }


authTokenToUser : String -> Result String User
authTokenToUser authToken =
    case Jwt.decodeToken tokenDecoder authToken of
        Ok td ->
            Ok { name = td.username, token = authToken }

        Err err ->
            Err <| Jwt.errorToString err



-- Generating passwords


vowels : List Char
vowels =
    String.toList "aeiouy"


consonants : List Char
consonants =
    String.toList "bcdfghjklmnpqrstvwxz"


randomElement : List Char -> Random.Generator Char
randomElement list =
    Random.map
        (\i -> List.Extra.getAt i list |> Maybe.withDefault '.')
        (Random.int 0 (List.length list - 1))


passwordGen : Int -> Random.Generator String
passwordGen size =
    let
        vowelGen =
            randomElement vowels

        consonantGen =
            randomElement consonants

        genList n =
            if n <= 0 then
                Random.constant []

            else if modBy 2 n == 1 then
                Random.map2 (::) consonantGen (genList (n - 1))

            else
                Random.map2 (::) vowelGen (genList (n - 1))
    in
    Random.map String.fromList (genList size)
