module LittleGolem exposing (..)

import GameRecord exposing (..)
import Parser exposing (..)


type alias Property =
    ( String, String )


type alias Node =
    List Property



-- type Property
--     = FF Int
--     | GM Int
--     | EV String
--     | PB String
--     | PW String
--     | SZ Int
--     | KM Float
--     | VR String
--     | B String
--     | W String
--     | R String


propertyParser : Parser Property
propertyParser =
    succeed Tuple.pair
        |= (getChompedString <| chompUntil "[")
        |. symbol "["
        |= (getChompedString <| chompUntil "]")
        |. symbol "]"


nodeHelp : List Property -> Parser (Step (List Property) (List Property))
nodeHelp revProps =
    oneOf
        [ succeed (\_ -> Done (List.reverse revProps))
            |= oneOf [ symbol ";", symbol ")" ]
        , succeed (\p -> Loop (p :: revProps))
            |= propertyParser
        ]


nodeParser : Parser Node
nodeParser =
    succeed identity
        |= loop [] nodeHelp


parserHelp revNodes =
    oneOf
        [ succeed (\n -> Loop (n :: revNodes)) |= nodeParser
        , succeed () |> map (\_ -> Done (List.reverse revNodes))
        ]


parser : Parser (List Node)
parser =
    succeed identity
        |. symbol "("
        |. symbol ";"
        |= loop [] parserHelp
        |. end


parse : String -> Maybe Record
parse input =
    Nothing
