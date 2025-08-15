module Replay.TreeLayout exposing (..)

import Dict exposing (Dict)
import List.Extra
import Replay.Tree as T



-- 1. build branch queue


type alias BranchWithParents a =
    { parents : List a
    , nodes : List a
    }


buildBranchQueue : T.Tree a -> List (BranchWithParents a)
buildBranchQueue tree =
    let
        ( firstPath, subBranches ) =
            extractFirstPathAndSubBranches tree []
    in
    firstPath :: subBranches


extractFirstPathAndSubBranches : T.Tree a -> List a -> ( BranchWithParents a, List (BranchWithParents a) )
extractFirstPathAndSubBranches currentTree currentAncestors =
    case currentTree of
        T.Locked ->
            ( { parents = [], nodes = [] }, [] )

        T.Tree node ->
            let
                childAncestors =
                    Maybe.map (\val -> currentAncestors ++ [ val ]) node.value
                        |> Maybe.withDefault currentAncestors

                currentNodeValueList : List a
                currentNodeValueList =
                    Maybe.withDefault [] (Maybe.map List.singleton node.value)

                createBranch : List a -> BranchWithParents a
                createBranch nodeValues =
                    { parents = currentAncestors, nodes = nodeValues }
            in
            case node.children of
                [] ->
                    ( createBranch currentNodeValueList
                    , []
                    )

                firstChild :: otherChildren ->
                    let
                        ( firstPath, subBranches ) =
                            extractFirstPathAndSubBranches firstChild childAncestors

                        branchesFromSiblings =
                            processForestForBranches otherChildren childAncestors
                    in
                    ( createBranch (currentNodeValueList ++ firstPath.nodes)
                    , subBranches ++ branchesFromSiblings
                    )


processForestForBranches : T.Forest a -> List a -> List (BranchWithParents a)
processForestForBranches forest ancestors =
    case forest of
        [] ->
            []

        firstChild :: otherChildren ->
            let
                ( firstPath, subBranches ) =
                    extractFirstPathAndSubBranches firstChild ancestors

                branchesFromSiblings =
                    processForestForBranches otherChildren ancestors
            in
            (firstPath :: subBranches) ++ branchesFromSiblings



-- 2. `List (BranchWithParents a) -> List (Branch a)`


type alias Branch a =
    { parentBranch : Int
    , firstNodeNum : Int
    , nodes : List a
    }


buildBranchList : List (BranchWithParents a) -> List (Branch a)
buildBranchList bwps =
    let
        toBranch : BranchWithParents a -> Branch a
        toBranch b =
            { parentBranch = findParentBranch bwps b
            , firstNodeNum = List.length b.parents
            , nodes = b.nodes
            }
    in
    List.map toBranch bwps


findParentBranch : List (BranchWithParents a) -> BranchWithParents a -> Int
findParentBranch allBranches =
    findParentBranchHelper (List.indexedMap Tuple.pair allBranches)


findParentBranchHelper : List ( Int, BranchWithParents a ) -> BranchWithParents a -> Int
findParentBranchHelper matchingBranches branch =
    case branch.parents of
        [] ->
            matchingBranches
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 0

        firstParent :: restParents ->
            findParentBranchHelper
                (List.filterMap (nextParent firstParent) matchingBranches)
                { branch | parents = restParents }


nextParent : a -> ( Int, BranchWithParents a ) -> Maybe ( Int, BranchWithParents a )
nextParent node ( branchIndex, bwp ) =
    case ( bwp.parents, bwp.nodes ) of
        ( [], hNode :: tNodes ) ->
            if hNode == node then
                Just ( branchIndex, { bwp | nodes = tNodes } )

            else
                Nothing

        ( hParent :: tParents, _ ) ->
            if hParent == node then
                Just ( branchIndex, { bwp | parents = tParents } )

            else
                Nothing

        _ ->
            Nothing



-- 3. process layout: take branches off the queue and position each


type alias PositionedBranch a =
    { parentBranch : Int
    , branchOffset : Int
    , firstNodeNum : Int
    , nodes : List a
    }


buildPositionedBranches : List (Branch a) -> List (PositionedBranch a)
buildPositionedBranches =
    let
        helper acc branches =
            case branches of
                [] ->
                    acc

                branch :: restBranches ->
                    helper (positionBranch 0 branch acc) restBranches
    in
    helper []


positionBranch : Int -> Branch a -> List (PositionedBranch a) -> List (PositionedBranch a)
positionBranch offset branch acc =
    let
        isWhollyBefore b1 b2 =
            b1.firstNodeNum > b2.firstNodeNum + List.length b2.nodes

        conflicts : PositionedBranch a -> Bool
        conflicts b =
            (offset <= b.branchOffset)
                -- && (not <| isWhollyBefore branch b) -- good idea or not?
                && (not <| isWhollyBefore b branch)
    in
    if List.any conflicts acc then
        positionBranch (offset + 1) branch acc

    else
        let
            positioned =
                { parentBranch = branch.parentBranch
                , branchOffset = offset
                , firstNodeNum = branch.firstNodeNum
                , nodes = branch.nodes
                }
        in
        acc ++ [ positioned ]



-- 4. Tree to branches


treeToBranches : T.Tree a -> List (PositionedBranch a)
treeToBranches =
    buildBranchQueue >> buildBranchList >> buildPositionedBranches



-- 5. Branches to dict


type alias Pos =
    -- branchOffset, nodeNum
    ( Int, Int )


branchToNodes : ( PositionedBranch a, PositionedBranch a ) -> List ( Pos, Pos, a )
branchToNodes ( parentBranch, b ) =
    let
        nodePosition : Int -> a -> ( Pos, a )
        nodePosition i node =
            ( ( b.branchOffset, b.firstNodeNum + i ), node )

        branchNodes : List ( Pos, a )
        branchNodes =
            List.indexedMap nodePosition b.nodes

        positionsWithoutLast : List Pos
        positionsWithoutLast =
            branchNodes
                |> List.Extra.init
                |> Maybe.withDefault []
                |> List.map Tuple.first

        parentPositions : List Pos
        parentPositions =
            let
                newNodeNum =
                    if b.firstNodeNum > 0 then
                        b.firstNodeNum - 1

                    else
                        0
            in
            ( parentBranch.branchOffset, newNodeNum ) :: positionsWithoutLast
    in
    List.map2 (\pp ( np, n ) -> ( pp, np, n )) parentPositions branchNodes


branchesToDict : List (PositionedBranch a) -> Dict Pos ( Pos, a )
branchesToDict branches =
    let
        withParent : PositionedBranch a -> Maybe ( PositionedBranch a, PositionedBranch a )
        withParent branch =
            List.Extra.getAt branch.parentBranch branches
                |> Maybe.map (\mpb -> ( mpb, branch ))
    in
    branches
        |> List.filterMap withParent
        |> List.concatMap branchToNodes
        |> List.foldl
            (\( parentPos, currentPos, node ) dict ->
                Dict.insert currentPos ( parentPos, node ) dict
            )
            Dict.empty



-- 6. find position


findPositionHelper : List (PositionedBranch a) -> List a -> Pos -> Pos
findPositionHelper positionedBranches path ( branchId, nodeNum ) =
    let
        nextNodeNum =
            nodeNum + 1

        getNextInBranch : a -> PositionedBranch a -> Maybe Pos
        getNextInBranch elem branch =
            List.Extra.getAt (nextNodeNum - branch.firstNodeNum) branch.nodes
                |> Maybe.andThen
                    (\node ->
                        if node == elem then
                            Just ( branchId, nextNodeNum )

                        else
                            Nothing
                    )

        findNextBranch : a -> Pos
        findNextBranch elem =
            List.indexedMap Tuple.pair positionedBranches
                |> List.filter
                    (\( _, pb ) ->
                        (pb.parentBranch == branchId)
                            && (pb.firstNodeNum == nextNodeNum)
                            && (List.head pb.nodes == Just elem)
                    )
                |> List.head
                |> Maybe.map (\( i, _ ) -> ( i, nextNodeNum ))
                -- TODO bad bad bad
                |> Maybe.withDefault ( -1, -1 )

        newPositionFromElem : a -> Pos
        newPositionFromElem elem =
            positionedBranches
                |> List.Extra.getAt branchId
                |> Maybe.andThen (getNextInBranch elem)
                |> Maybe.withDefault (findNextBranch elem)
    in
    case path of
        [] ->
            positionedBranches
                |> List.Extra.getAt branchId
                |> Maybe.map (\b -> ( b.branchOffset, nodeNum ))
                -- TODO bad bad bad
                |> Maybe.withDefault ( -1, -1 )

        head :: tail ->
            findPositionHelper positionedBranches tail (newPositionFromElem head)


findPosition : List (PositionedBranch a) -> List a -> Pos
findPosition positionedBranches path =
    findPositionHelper positionedBranches (List.tail path |> Maybe.withDefault []) ( 0, 0 )



-- 7. View


type alias LayoutItem a =
    { parent : Pos
    , node : a
    , path : List a
    , focus : Bool
    }


getPath : Int -> Int -> List (PositionedBranch a) -> List a
getPath row col positionedBranches =
    let
        contains : ( Int, PositionedBranch a ) -> Bool
        contains ( _, pb ) =
            (pb.branchOffset == row)
                && (col >= pb.firstNodeNum)
                && (col < pb.firstNodeNum + List.length pb.nodes)

        doGetPath : Int -> ( Int, PositionedBranch a ) -> List a
        doGetPath toColumn ( branchId, pb ) =
            let
                nodes : List a
                nodes =
                    pb.nodes |> List.take (toColumn + 1 - pb.firstNodeNum)
            in
            if branchId == 0 then
                nodes

            else
                case List.Extra.getAt pb.parentBranch positionedBranches of
                    Just branch ->
                        doGetPath (pb.firstNodeNum - 1) ( pb.parentBranch, branch ) ++ nodes

                    Nothing ->
                        []
    in
    positionedBranches
        |> List.indexedMap Tuple.pair
        |> List.filter contains
        |> List.map (doGetPath col)
        |> List.head
        |> Maybe.withDefault []


getTreeLayout : Int -> Int -> T.Zipper a -> List (List (Maybe (LayoutItem a)))
getTreeLayout width height zipper =
    let
        branches : List (PositionedBranch a)
        branches =
            T.makeTree zipper |> treeToBranches

        position : Pos
        position =
            findPosition branches (T.currentValues zipper)

        upper =
            max 0 (Tuple.first position - (height // 2))

        left =
            max 0 (Tuple.second position - (width // 2))

        dict : Dict Pos ( Pos, a )
        dict =
            branchesToDict branches

        createItem : Bool -> ( Pos, a ) -> LayoutItem a
        createItem focus ( p, a ) =
            { parent = p
            , node = a
            , path = []
            , focus = focus
            }

        createCell : Int -> Int -> Maybe (LayoutItem a)
        createCell row col =
            Dict.get ( row, col ) dict
                |> Maybe.map (createItem (( row, col ) == position))

        createCols : Int -> List (Maybe (LayoutItem a))
        createCols row =
            List.map (createCell row) (List.range left (left + width - 1))
    in
    List.map createCols (List.range upper (upper + height - 1))
