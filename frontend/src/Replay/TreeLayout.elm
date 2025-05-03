module Replay.TreeLayout exposing (..)

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
    { branchOffset : Int
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
            (offset == b.branchOffset)
                && (not <| isWhollyBefore branch b)
                && (not <| isWhollyBefore b branch)
    in
    if List.any conflicts acc then
        positionBranch (offset + 1) branch acc

    else
        let
            positioned =
                { branchOffset = offset
                , firstNodeNum = branch.firstNodeNum
                , nodes = branch.nodes
                }
        in
        acc ++ [ positioned ]
