module Replay.TreeLayout exposing (..)

import Replay.Tree as T



-- 1. build branch queue
-- 2. process layout: take branches off the queue and add horizontal line for each
-- 3. layout horizontal line


type alias Branch a =
    { parentBranch : Int
    , firstNodeNum : Int
    , nodes : List a
    }


buildBranchQueue : T.Tree a -> List (Branch a)
buildBranchQueue rootTree =
    let
        branchFromNodes : List a -> Branch a
        branchFromNodes nodeList =
            { parentBranch = 0 -- Placeholder
            , firstNodeNum = 0 -- Placeholder
            , nodes = nodeList
            }
    in
    List.map branchFromNodes <| collectNodeLists rootTree


collectNodeLists : T.Tree a -> List (List a)
collectNodeLists tree =
    let
        ( firstPath, subBranches ) =
            extractFirstPathAndSubBranches tree
    in
    firstPath :: subBranches


extractFirstPathAndSubBranches : T.Tree a -> ( List a, List (List a) )
extractFirstPathAndSubBranches currentTree =
    case currentTree of
        T.Locked ->
            ( [], [] )

        T.Tree node ->
            let
                currentNodeValueList : List a
                currentNodeValueList =
                    Maybe.withDefault [] (Maybe.map List.singleton node.value)
            in
            case node.children of
                [] ->
                    ( currentNodeValueList, [] )

                firstChild :: otherChildren ->
                    let
                        ( firstPath, subBranches ) =
                            extractFirstPathAndSubBranches firstChild

                        branchesFromSiblings =
                            processForestForBranches otherChildren
                    in
                    ( currentNodeValueList ++ firstPath
                    , subBranches ++ branchesFromSiblings
                    )


processForestForBranches : T.Forest a -> List (List a)
processForestForBranches forest =
    case forest of
        [] ->
            []

        firstChild :: otherChildren ->
            let
                ( firstPath, subBranches ) =
                    extractFirstPathAndSubBranches firstChild

                branchesFromSiblings =
                    processForestForBranches otherChildren
            in
            (firstPath :: subBranches) ++ branchesFromSiblings
