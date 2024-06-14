module Things exposing (..)

import Browser
import Codec
import Html
import Json.Encode as JE
import LocalState exposing (LocalState)
import Replay exposing (GameView)
import Replay.Tree as Tree exposing (..)


t : a -> Forest a -> Tree a
t v c =
    Tree { value = Just v, defaultChild = 0, children = c }


trickyTree : Tree String
trickyTree =
    t "root"
        [ t "A1"
            [ t "A2"
                [ t "A3"
                    [ Locked
                    , t "E4" []
                    , t "F4"
                        [ t "F5"
                            [ t "F6" []
                            ]
                        ]
                    , t "G4" []
                    ]
                ]
            ]
        , t "B1"
            [ t "B2"
                [ t "B3" []
                ]
            , t "C2"
                [ t "C3"
                    [ t "C4" []
                    ]
                ]
            ]
        ]


zipper : Zipper String
zipper =
    makeZipper trickyTree


z : Maybe (Zipper String)
z =
    zipper
        |> lookPrev


encode : Zipper String -> Codec.Value
encode =
    Codec.encoder <| LocalState.zipperCodec Codec.string


lstStr strs =
    Html.pre [] (List.map (\txt -> Html.text <| " " ++ txt ++ " ") strs)


view : ( String, List String, List String ) -> Html.Html msg
view ( enc, cur, all ) =
    Html.div []
        [ Html.pre [] [ Html.text enc ]
        , lstStr cur
        , lstStr all
        ]


main =
    let
        enc =
            Maybe.map (encode >> JE.encode 4) z
                |> Maybe.withDefault "couldn't"

        cur =
            Maybe.map Tree.currentValues z
                |> Maybe.withDefault []

        all =
            Maybe.map Tree.allValues z
                |> Maybe.withDefault []
    in
    Browser.sandbox { init = ( enc, cur, all ), update = identity, view = view }
