module ApiClient exposing (..)

import GameRecord exposing (Record, empty)
import Http
import Json.Decode as D


baseUrl =
    "https://www.littlegolem.net"


type alias SgfResult =
    Result Http.Error Record


sgfResultDecoder : D.Decoder Record
sgfResultDecoder =
    -- TODO!!!
    D.succeed empty


getLittleGolemSgf : (SgfResult -> msg) -> String -> Cmd msg
getLittleGolemSgf ctor gameId =
    Http.get
        { url = baseUrl ++ "/servlet/sgf/" ++ gameId ++ "/game.sgf"
        , expect = Http.expectJson ctor sgfResultDecoder
        }
