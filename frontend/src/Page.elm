module Page exposing (..)

import Html as H


type alias Page msg =
    { title : String, body : List (H.Html msg) }


viewPage : (pageMsg -> globalMsg) -> Page pageMsg -> Page globalMsg
viewPage toMsg pageView =
    let
        { title, body } =
            pageView
    in
    { title = title
    , body = List.map (H.map toMsg) body
    }
