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


{-| Converts a Tree into a list of Branches according to specific traversal rules:

1.  Follow the first child path down to its end, collecting node values. This forms the first branch.
2.  Backtrack: At each node going up, process the branches starting from its _next_ sibling.

-}
buildBranchQueue : T.Tree a -> List (Branch a)
buildBranchQueue rootTree =
    let
        -- First, get the lists of node values for each branch
        nodeLists =
            collectNodeLists rootTree

        -- Convert each list of node values into a Branch record
        -- Using placeholder values for parentBranch and firstNodeNum for now
        branchFromNodes nodeList =
            { parentBranch = 0 -- Placeholder
            , firstNodeNum = 0 -- Placeholder
            , nodes = nodeList
            }
    in
    List.map branchFromNodes nodeLists


{-| Helper function: Collects the lists of node values for each branch.
This is where the core traversal logic resides.
-}
collectNodeLists : T.Tree a -> List (List a)
collectNodeLists tree =
    let
        -- This function extracts the *first* path and any branches
        -- starting from siblings encountered along that first path.
        ( firstPathNodes, siblingBranches ) =
            extractFirstPathAndSubBranches tree
    in
    -- The overall result is the first path (if it contains any nodes)
    -- followed by all the branches found from siblings.
    if List.isEmpty firstPathNodes then
        siblingBranches

    else
        firstPathNodes :: siblingBranches


{-| Extracts the node values along the "first child" path starting from the given tree.
It also recursively collects all branches that start from the siblings of the nodes
along this primary path.

Returns: A tuple containing:

1.  `List a`: The nodes collected along the primary (first child) path.
2.  `List (List a)`: A list of all branches originating from siblings encountered
    during the descent down the first path.

-}
extractFirstPathAndSubBranches : T.Tree a -> ( List a, List (List a) )
extractFirstPathAndSubBranches currentTree =
    case currentTree of
        T.Locked ->
            -- A locked node terminates the path and generates no branches.
            ( [], [] )

        T.Tree node ->
            -- Get the value of the current node, if it exists.
            let
                currentNodeValueList : List a
                currentNodeValueList =
                    Maybe.withDefault [] (Maybe.map List.singleton node.value)
            in
            case node.children of
                [] ->
                    -- Leaf node: The path ends here. Only contains the current node's value.
                    -- No children means no siblings to generate branches from at this level.
                    ( currentNodeValueList, [] )

                firstChild :: otherSiblings ->
                    -- This node has children. Descend down the first child.
                    let
                        -- Recursively find the path and sub-branches from the first child.
                        ( pathNodesFromChild, branchesFromChildSubtree ) =
                            extractFirstPathAndSubBranches firstChild

                        -- Process the siblings of the first child to find branches starting from them.
                        branchesFromSiblings =
                            processForestForBranches otherSiblings
                    in
                    -- Combine the results:
                    -- 1. The primary path consists of the current node's value followed by the
                    --    primary path from its first child.
                    -- 2. The collected sub-branches are those found deeper within the first child's
                    --    subtree *plus* those starting from the siblings of the first child.
                    ( currentNodeValueList ++ pathNodesFromChild
                    , branchesFromChildSubtree ++ branchesFromSiblings
                    )


{-| Processes a list of sibling trees (a Forest) and collects all branches
originating from them, following the same "first path first, then siblings" logic
for each tree in the list.
-}
processForestForBranches : T.Forest a -> List (List a)
processForestForBranches forest =
    case forest of
        [] ->
            -- Empty forest, no branches.
            []

        firstSiblingTree :: otherSiblingTrees ->
            -- Process the first tree in the forest.
            let
                ( firstPath, subBranches ) =
                    extractFirstPathAndSubBranches firstSiblingTree

                -- Combine its primary path (if not empty) and its sub-branches.
                branchesFromFirst =
                    if List.isEmpty firstPath then
                        subBranches

                    else
                        firstPath :: subBranches

                -- Recursively process the rest of the siblings.
                branchesFromOthers =
                    processForestForBranches otherSiblingTrees
            in
            -- The total branches from this forest are those from the first tree
            -- followed by those from the remaining trees.
            branchesFromFirst ++ branchesFromOthers
