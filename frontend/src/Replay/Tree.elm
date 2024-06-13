module Replay.Tree exposing (..)

import List.Extra



-- Tree


type alias Node a =
    { value : Maybe a
    , defaultChild : Int
    , children : Forest a
    }


type Tree a
    = Tree (Node a)
    | Locked


type alias Forest a =
    List (Tree a)


listToLockedTree : List a -> Tree a
listToLockedTree children =
    let
        helper chlds =
            case chlds of
                head :: tail ->
                    [ Tree
                        { value = Just head
                        , defaultChild = 0
                        , children = helper tail
                        }
                    ]

                [] ->
                    [ Locked ]
    in
    Tree
        { value = Nothing
        , defaultChild = 0
        , children = helper children
        }


getTreeNodeData : Tree a -> Maybe (Node a)
getTreeNodeData tree =
    case tree of
        Tree tnd ->
            Just tnd

        Locked ->
            Nothing


getValue : Tree a -> Maybe a
getValue =
    getTreeNodeData >> Maybe.andThen .value


getChildren : Tree a -> Maybe (Forest a)
getChildren =
    getTreeNodeData >> Maybe.map .children



-- Zipper


type alias Crumb a =
    { value : Maybe a
    , before : List (Tree a)
    , after : List (Tree a)
    }


type alias Zipper a =
    { focus : Tree a
    , before : List (Tree a)
    , after : List (Tree a)
    , crumbs : List (Crumb a)
    }



-- Zipper from trees and vice versa


makeZipper : Tree a -> Zipper a
makeZipper tree =
    { focus = tree, before = [], after = [], crumbs = [] }


makeTree : Zipper a -> Tree a
makeTree =
    lookStart >> .focus



-- Looking around zipper


lookPrev : Zipper a -> Maybe (Zipper a)
lookPrev zipper =
    case zipper.crumbs of
        [] ->
            Nothing

        crumb :: rest ->
            Just
                { focus = reconstruct crumb.value zipper.before zipper.focus zipper.after
                , before = crumb.before
                , after = crumb.after
                , crumbs = rest
                }


lookNext : Zipper a -> Maybe (Zipper a)
lookNext zipper =
    case zipper.focus of
        Locked ->
            Nothing

        Tree tnd ->
            lookChild tnd.defaultChild tnd zipper


lookNextByIndex : Int -> Zipper a -> Maybe (Zipper a)
lookNextByIndex childIndex zipper =
    case zipper.focus of
        Locked ->
            Nothing

        Tree tnd ->
            lookChild childIndex tnd zipper


lookStart : Zipper a -> Zipper a
lookStart zipper =
    case lookPrev zipper of
        Nothing ->
            zipper

        Just z ->
            lookStart z


lookEnd : Zipper a -> Zipper a
lookEnd zipper =
    case lookNext zipper of
        Nothing ->
            zipper

        Just z ->
            lookEnd z


lookPrevVar : Zipper a -> Zipper a
lookPrevVar zipper =
    -- TODO
    zipper


lookNextVar : Zipper a -> Zipper a
lookNextVar zipper =
    -- TODO
    zipper


currentValues : Zipper a -> List a
currentValues zipper =
    -- values from root to current view
    zipper.crumbs
        |> List.map .value
        |> (::) (getValue zipper.focus)
        |> List.filterMap identity
        |> List.reverse


allValues : Zipper a -> List a
allValues zipper =
    -- values from root to end of current variation
    currentValues zipper ++ defaultChildList zipper



-- Children


findChildIndex : a -> Zipper a -> Maybe Int
findChildIndex child zipper =
    case zipper.focus of
        Tree { children } ->
            List.Extra.findIndex (\c -> getValue c == Just child) children

        Locked ->
            Nothing


addChild : a -> Zipper a -> ( Int, Zipper a )
addChild child zipper =
    case zipper.focus of
        Tree { value, children } ->
            let
                childTree : Tree a
                childTree =
                    Tree
                        { value = Just child
                        , defaultChild = 0
                        , children = []
                        }

                newChildren =
                    children ++ [ childTree ]
            in
            ( List.length children
            , { zipper
                | focus =
                    Tree
                        { value = value
                        , defaultChild = List.length children
                        , children = newChildren
                        }
              }
            )

        Locked ->
            ( 0, zipper )



-- Helpers


reconstruct : Maybe a -> List (Tree a) -> Tree a -> List (Tree a) -> Tree a
reconstruct value before focus after =
    Tree
        { value = value
        , defaultChild = List.length before
        , children = List.reverse before ++ [ focus ] ++ after
        }


splitAround : Int -> List a -> Maybe ( List a, a, List a )
splitAround n xs =
    case ( List.take n xs, List.drop n xs ) of
        ( before, el :: after ) ->
            Just ( before, el, after )

        _ ->
            Nothing


lookChild : Int -> Node a -> Zipper a -> Maybe (Zipper a)
lookChild chosenChild { value, children } zipper =
    let
        helper : ( List (Tree a), Tree a, List (Tree a) ) -> Maybe (Zipper a)
        helper ( before, child, after ) =
            case child of
                Locked ->
                    Nothing

                Tree _ ->
                    Just
                        { focus = child
                        , before = before
                        , after = after
                        , crumbs =
                            { value = value
                            , before = zipper.before
                            , after = zipper.after
                            }
                                :: zipper.crumbs
                        }
    in
    splitAround chosenChild children
        |> Maybe.andThen helper


defaultChildList : Zipper a -> List a
defaultChildList =
    let
        helper : List a -> Zipper a -> List a
        helper acc zipper =
            case lookNext zipper of
                Nothing ->
                    acc

                Just z ->
                    case z.focus of
                        Tree { value } ->
                            case value of
                                Just v ->
                                    helper (v :: acc) z

                                Nothing ->
                                    helper acc z

                        Locked ->
                            acc
    in
    helper [] >> List.reverse
