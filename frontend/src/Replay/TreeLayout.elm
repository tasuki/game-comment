module Replay.TreeLayout exposing (..)

import Replay.Tree as T


type alias Branch a =
    { parentBranch : Int
    , firstNodeNum : Int
    , nodes : List a
    }



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



-- TODO
-- 2. `List (BranchWithParents a) -> List (Branch a)`
-- 3. process layout: take branches off the queue and add horizontal line for each
-- 4. layout horizontal line
