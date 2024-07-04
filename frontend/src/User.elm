module User exposing (..)

import Json.Encode as E


type alias CreateUser =
    { username : String
    , password : String
    , email : Maybe String
    }


createUserEncoder : CreateUser -> E.Value
createUserEncoder createUser =
    E.object
        [ ( "username", E.string createUser.username )
        , ( "password", E.string createUser.password )
        , ( "email", createUser.email |> Maybe.map E.string |> Maybe.withDefault E.null )
        ]


vowels : List Char
vowels =
    String.toList "aeiouy"


consonants : List Char
consonants =
    String.toList "bcdfghjklmnpqrstvwxz"


passwordGen size =
    []
