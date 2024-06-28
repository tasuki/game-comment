module Replay.Tree exposing (..)

import List.Extra
import Maybe.Extra



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


createLockedTree : List a -> Tree a
createLockedTree children =
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


getChildren : Tree a -> Forest a
getChildren =
    getTreeNodeData
        >> Maybe.map .children
        >> Maybe.withDefault []


defaultChild : Tree a -> Maybe Int
defaultChild =
    getTreeNodeData >> Maybe.map .defaultChild



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



-- Zipper from tree and vice versa


makeZipper : Tree a -> Zipper a
makeZipper tree =
    { focus = tree, before = [], after = [], crumbs = [] }


makeTree : Zipper a -> Tree a
makeTree =
    ascendStart >> .focus



-- Looking around zipper


ascend : Zipper a -> Maybe (Zipper a)
ascend zipper =
    case zipper.crumbs of
        [] ->
            Nothing

        crumb :: rest ->
            Just
                { focus = reconstructTree crumb.value zipper.before zipper.focus zipper.after
                , before = crumb.before
                , after = crumb.after
                , crumbs = rest
                }


descend : Zipper a -> Maybe (Zipper a)
descend zipper =
    case zipper.focus of
        Locked ->
            Nothing

        Tree tnd ->
            descendToChild tnd.defaultChild tnd zipper


descendToIndex : Int -> Zipper a -> Maybe (Zipper a)
descendToIndex childIndex zipper =
    case zipper.focus of
        Locked ->
            Nothing

        Tree tnd ->
            descendToChild childIndex tnd zipper


ascendStart : Zipper a -> Zipper a
ascendStart zipper =
    case ascend zipper of
        Nothing ->
            zipper

        Just z ->
            ascendStart z


descendEnd : Zipper a -> Zipper a
descendEnd zipper =
    case descend zipper of
        Nothing ->
            zipper

        Just z ->
            descendEnd z


prevVariation : Zipper a -> Zipper a
prevVariation zipper =
    case zipper.before of
        [] ->
            zipper

        previous :: rest ->
            { focus = previous
            , before = rest
            , after = zipper.focus :: zipper.after
            , crumbs = zipper.crumbs
            }


nextVariation : Zipper a -> Zipper a
nextVariation zipper =
    case zipper.after of
        [] ->
            zipper

        next :: rest ->
            { focus = next
            , before = zipper.focus :: zipper.before
            , after = rest
            , crumbs = zipper.crumbs
            }


cutVariation : Zipper a -> Zipper a
cutVariation zipper =
    case zipper.crumbs of
        [] ->
            zipper

        crumb :: rest ->
            { focus =
                Tree
                    { value = crumb.value
                    , defaultChild = 0
                    , children = List.reverse zipper.before ++ zipper.after
                    }
            , before = crumb.before
            , after = crumb.after
            , crumbs = rest
            }



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



-- The Others


findAll : (a -> Bool) -> Zipper a -> List (Zipper a)
findAll isGood zipper =
    let
        add : Zipper a -> List (Zipper a)
        add z =
            z.focus
                |> getValue
                |> Maybe.Extra.filter isGood
                |> Maybe.map (\_ -> z)
                |> Maybe.Extra.toList

        childCount : Zipper a -> Int
        childCount z =
            List.length <| getChildren z.focus

        childZippers : Zipper a -> List (Zipper a)
        childZippers z =
            -- for child count 0, we want []
            -- for child count 1, we want [0]
            -- for child count 2, we want [0, 1]
            List.range 0 (childCount z - 1)
                |> List.filterMap (\i -> descendToIndex i z)

        helper : Zipper a -> List (Zipper a)
        helper z =
            add z ++ List.Extra.andThen helper (childZippers z)
    in
    helper <| ascendStart zipper


replaceFirstVar : List (Maybe a) -> Tree a -> Tree a
replaceFirstVar new old =
    case new of
        [] ->
            Locked

        head :: tail ->
            let
                newChildren =
                    case getChildren old of
                        [] ->
                            [ replaceFirstVar tail Locked ]

                        firstChild :: otherChildren ->
                            replaceFirstVar tail firstChild :: otherChildren
            in
            Tree
                { value = head
                , defaultChild = defaultChild old |> Maybe.withDefault 0
                , children = newChildren
                }



-- Zipper to list


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



-- Helpers


reconstructTree : Maybe a -> List (Tree a) -> Tree a -> List (Tree a) -> Tree a
reconstructTree value before focus after =
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


descendToChild : Int -> Node a -> Zipper a -> Maybe (Zipper a)
descendToChild chosenChild { value, children } zipper =
    let
        helper : ( List (Tree a), Tree a, List (Tree a) ) -> Maybe (Zipper a)
        helper ( before, child, after ) =
            case child of
                Locked ->
                    Nothing

                Tree _ ->
                    Just
                        { focus = child
                        , before = List.reverse before
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
            case descend zipper of
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
