module Replay.Tree exposing (..)


type alias TreeNodeData a =
    { value : a
    , defaultChild : Int
    , children : Trees a
    }


type Tree a
    = Locked
    | Tree (TreeNodeData a)


type alias Trees a =
    List (Tree a)



-- Zippers


type alias Crumb a =
    { value : a
    , before : List (Tree a)
    , after : List (Tree a)
    }


type alias Zipper a =
    { focus : Tree a
    , before : List (Tree a)
    , after : List (Tree a)
    , crumbs : List (Crumb a)
    }


makeZipper : Trees a -> Zipper a
makeZipper trees =
    case trees of
        t :: ts ->
            { focus = t, before = [], after = ts, crumbs = [] }

        _ ->
            { focus = Locked, before = [], after = [], crumbs = [] }


makeTrees : Zipper a -> Trees a
makeTrees zipper =
    let
        root : Zipper a
        root =
            lookStart zipper
    in
    List.reverse root.before ++ [ root.focus ] ++ root.after


reconstruct : a -> List (Tree a) -> Tree a -> List (Tree a) -> Tree a
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


pickChild : TreeNodeData a -> Zipper a -> Maybe (Zipper a)
pickChild { value, defaultChild, children } zipper =
    let
        helper : ( List (Tree a), Tree a, List (Tree a) ) -> Zipper a
        helper ( before, child, after ) =
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
    splitAround defaultChild children
        |> Maybe.map helper


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

        Tree treeNodeData ->
            pickChild treeNodeData zipper


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
                            helper (value :: acc) z

                        Locked ->
                            acc
    in
    helper []


currentValues : Zipper a -> List a
currentValues zipper =
    zipper.crumbs |> List.map .value |> List.reverse


allValues : Zipper a -> List a
allValues zipper =
    currentValues zipper ++ defaultChildList zipper
