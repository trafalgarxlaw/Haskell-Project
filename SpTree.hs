{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : SpTree
Description : Representation of SP-graphs with a binary tree
Copyright   : (c) Alexandre Blondin Massé
License     : GPL-3
Maintainer  : blondin_masse.alexandre@uqam.ca
Stability   : experimental

This module provides a types and functions for handling series-parallel trees,
i.e. binary trees that represents series-parallel graphs.
See the [Wikipedia page](https://en.wikipedia.org/wiki/Series-parallel_graph)
for more details.

First, we define series-parallel graphs. A series-parallel graph is a triple
@(G,s,t)@, where @G@ is a graph and @s@, @t@ are two special vertices of @G@
called respectively the source and the sink of @G@, built recursively as
follows:

* Basic case: Any triple @(G,s,t)@ such that @G@ is a graph of two vertices
  @s@ and @t@ and one arc @(s,t)@
* Series composition: If @G1@ and @G2@ are two SP-graphs with sources and
  sinks @s1@, @t1@, @s2@, @t2@, then the graph obtained by merging @t1@ with
  @s2@ is an SP-graph
* Parallel composition: If @G1@ and @G2@ are two SP-graphs with sources and
  sinks @s1@, @t1@, @s2@, @t2@, then the graph obtained by merging @s1@ with
  @s2@, and @t1@ with @t2@ is an SP-graph

An SP-tree is a binary tree representing the recursive scheme described
above for building an SP-graph. More precisely:

* Each internal node corresponds with either a @series@ or @parallel@
  composition
* Each leaf corresponds with an arc of the SP-graph

For instance, the diamond-shaped SP-graph of 4 vertices can be represented
by the following SP-tree:

>>> diamond
Par "P" (Ser "S" (Leaf "L") (Leaf "L")) (Ser "S" (Leaf "L") (Leaf "L"))

The red and blue SP-graphs (having respectively 8 and 7 vertices) of the
Wikipedia page are available for testing purposes:

>>> numVertices wikipediaRedExample
8
>>> numVertices wikipediaBlueExample
7
 -}

module SpTree (
    -- * Series-parallel trees
    SpTree (LeafNode, SeriesNode, ParallelNode),
    -- * Basic queries
    rootContent, numNodes, height, width,
    -- * Indexing
    -- | By default, SP-trees are not indexed, i.e. there is no key associated
    -- with each node. To produced an indexed version, the following functions
    -- are useful. In particular, it makes it easier to produce a Graphviz
    -- instance of an SP-tree.
    NodeIndexedSpTree, indexNodes,
    -- * Converting to a directed graph
    -- | To convert an SP-tree to a directed graph, it is convenient first to
    -- store into it the indices of the source and the sink in each node.
    numVertices, VerticesIndexedSpTree, indexVertices, toGraph,
    -- * Examples
    leaf, series, parallel, diamond, wikipediaRedExample, wikipediaBlueExample
) where

import Graphviz
import Graph hiding (numVertices)

-------------------------
-- Types and instances --
-------------------------

-- | A binary tree representing a series-parallel graph
data SpTree a
    -- | Leaf node
    = LeafNode a
    -- | Series composition
    | SeriesNode a (SpTree a) (SpTree a)
    -- | Parallel composition
    | ParallelNode a (SpTree a) (SpTree a)

-- | An SP-tree can be shown
--
-- >>> show $ LeafNode 1
-- "Leaf 1"
-- >>> show $ SeriesNode 1 (LeafNode 2) (LeafNode 3)
-- "Ser 1 (Leaf 2) (Leaf 3)"
-- >>> show $ ParallelNode 1 (LeafNode 2) (LeafNode 3)
-- "Par 1 (Leaf 2) (Leaf 3)"
instance Show a => Show (SpTree a) where
    show (LeafNode x) = "Leaf " ++ show x
    show (SeriesNode x y z) =
      "Ser " ++ show x ++ " (" ++ show y ++ ") (" ++ show z ++ ")"
    show (ParallelNode x y z) =
      "Par " ++ show x ++ " (" ++ show y ++ ") (" ++ show z ++ ")" 

-- | An SP-tree can be mapped over
--
-- >>> fmap head diamond
-- Par 'P' (Ser 'S' (Leaf 'L') (Leaf 'L')) (Ser 'S' (Leaf 'L') (Leaf 'L'))
instance Functor SpTree where
    fmap  f (LeafNode x) = (LeafNode (f x))
    fmap  f (SeriesNode x y z) = (SeriesNode (f x) (fmap f y) (fmap f z) ) 
    fmap  f (ParallelNode x y z) = (ParallelNode (f x) (fmap f y) (fmap f z) ) 

-------------------
-- Basic queries --
-------------------

-- | Returns the content of the root node
--
-- >>> rootContent (LeafNode 1)
-- 1
-- >>> rootContent $ SeriesNode [1,2,3] (LeafNode [1,2]) (LeafNode [3])
-- [1,2,3]
-- >>> rootContent diamond
-- "P"
rootContent :: SpTree a -> a
rootContent (LeafNode x) = x
rootContent (SeriesNode x y z) = x
rootContent (ParallelNode x y z) = x

-- | Returns the number of nodes of an SP-tree
--
-- >>> numNodes (LeafNode 1)
-- 1
-- >>> numNodes diamond
-- 7
-- >>> numNodes wikipediaRedExample
-- 19
-- >>> numNodes wikipediaBlueExample
-- 13
numNodes :: SpTree a -> Int
numNodes (LeafNode x) = 1 
numNodes (SeriesNode x y z) = 1 + numNodes y + numNodes z
numNodes (ParallelNode x y z) = 1 + numNodes y + numNodes z



-- | Returns the height of the SP-graph associated with an SP-tree
--
-- The height is the length of a longest path from the source to the sink.
--
-- >>> height (LeafNode 1)
-- 1
-- >>> height diamond
-- 2
-- >>> height wikipediaRedExample
-- 4
-- >>> height wikipediaBlueExample
-- 4
height :: SpTree a -> Int
height (LeafNode x) = 1 
height (SeriesNode x y z) = 1 + (max (height y) (height z))
height (ParallelNode x y z)  =  (max (height y) (height z))

-- | Returns the width of the SP-graph associated with an SP-tree
--
-- The width is defined recursively as follows:
--
-- * The width of a basic SP-graph is 1
-- * The width of an SP-graph obtained by a series composition of two graphs
--   `G1` and `G2` is the maximum of the widths of `G1` and `G2`
-- * The width of an SP-graph obtained by a parallel composition of two graphs
--   `G1` and `G2` is the sum of the widths of `G1` and `G2`
--
-- >>> width (LeafNode 1)
-- 1
-- >>> width diamond
-- 2
-- >>> width wikipediaRedExample
-- 4
-- >>> width wikipediaBlueExample
-- 2
width :: SpTree a -> Int
width (LeafNode x) = 1
width (SeriesNode x y z) = max (width y )  (width z)
width (ParallelNode x y z) =   (width y ) + (width z)

---------------------
-- Indexed SP-tree --
---------------------

-- | An SP-tree whose nodes are indexed with unique integers
type NodeIndexedSpTree a = SpTree (Int, a)

-- | Indexes an SP-tree with unique integers
--
-- To make the indices unique, we use the numbers `0` up to `n - 1`, where `n`
-- is the number of nodes and we index the nodes so that an in-order (infix)
-- traversal yields the indices in ascending order.
--
-- >>> fmap fst $ indexNodes diamond
-- Par 3 (Ser 1 (Leaf 0) (Leaf 2)) (Ser 5 (Leaf 4) (Leaf 6))

indexNodes' :: SpTree a ->Int -> Int-> NodeIndexedSpTree a
indexNodes' (LeafNode x) m n = (LeafNode (m,x))
indexNodes' (SeriesNode x y z) m n = SeriesNode (m + (numNodes y),x) (indexNodes' y (m) (m + (numNodes y - 1))) (indexNodes' z (m+(numNodes y)+1) n ) 
indexNodes' (ParallelNode x y z) m n = ParallelNode (m + (numNodes y),x) (indexNodes' y (m) (m + (numNodes y - 1))) (indexNodes' z (m+(numNodes y)+1) n ) 



indexNodes :: SpTree a -> NodeIndexedSpTree a
indexNodes sptree =  indexNodes' (sptree) (0) (numNodes(sptree) - 1)

----------------------
-- Underlying graph --
----------------------

-- | Returns the number of vertices in the underlying SP-graph
--
-- >>> numVertices (LeafNode 1)
-- 2
-- >>> numVertices diamond
-- 4
numVertices :: SpTree a -> Int
numVertices (LeafNode a)= 2
numVertices (SeriesNode x y z)= (numVertices y )+(numVertices z ) -1
numVertices (ParallelNode x y z)= (numVertices y )+(numVertices z ) -2

-- | An SP-tree with source and sinks vertices
type VerticesIndexedSpTree a = SpTree (Int, Int, a)

-- | Indexes an SP-tree with pairs `(s,t)` of the source and sink of each node
--
-- For instance, the source and sink of the diamond SP-graph are `0` and `3`.
-- Also, the last operation is a parallel composition. Therefore, we have:
--
-- >>> rootContent (indexVertices diamond)
-- (0,3,"P")
--
-- The red and blue Wikipedia examples:
--
-- >>> indexVertices diamond
-- Par (0,3,"P") (Ser (0,3,"S") (Leaf (0,1,"L")) (Leaf (1,3,"L"))) (Ser (0,3,"S") (Leaf (0,2,"L")) (Leaf (2,3,"L")))
-- >>> rootContent (indexVertices wikipediaRedExample)
-- (0,7,"P")
-- >>> rootContent (indexVertices wikipediaBlueExample)
-- (0,6,"S")

indexVertices' :: SpTree a->Int ->Int ->Int -> Int ->VerticesIndexedSpTree a
indexVertices' (LeafNode value) s t m n = LeafNode (s,t,value)
indexVertices' (SeriesNode value  left right) s t m n = SeriesNode (s,t,value) (indexVertices' left s (m + (numVertices left) - 2) m ( m + (numVertices left) - 3)  )  (indexVertices' right (m + (numVertices left) - 2) t (m + (numVertices left) - 1) n )
indexVertices' (ParallelNode value left right) s t m n = ParallelNode (s,t,value) (indexVertices' left s t m ( m + (numVertices left) - 3))  (indexVertices' right s t ( m + (numVertices left) - 2) n)

indexVertices :: SpTree a -> VerticesIndexedSpTree a
indexVertices spTree = indexVertices' spTree 0 ((numVertices spTree )- 1)  1 ((numVertices spTree) - 2)

-- | Builds the SP-graph associated with the given SP-tree
--
-- >>> g = toGraph $ indexVertices wikipediaRedExample
-- >>> import Data.Map (fromList)
-- >>> labels = fromList $ zip (vertices g) (repeat "")
-- >>> putStrLn $ graphvizString $ labelGraph labels g
-- digraph {
--     0 [label=""];
--     1 [label=""];
--     2 [label=""];
--     3 [label=""];
--     4 [label=""];
--     5 [label=""];
--     6 [label=""];
--     7 [label=""];
--     0 -> 1;
--     0 -> 6;
--     1 -> 2;
--     1 -> 3;
--     1 -> 4;
--     2 -> 5;
--     3 -> 5;
--     4 -> 5;
--     5 -> 7;
--     6 -> 7;
-- }
-- ...

convertList :: VerticesIndexedSpTree a-> [(Int,Int)]
convertList (LeafNode (s,t,value))  = [(s,t)]
convertList (SeriesNode (s,t,value) left right) =  (convertList left) ++ (convertList right)
convertList (ParallelNode (s,t,value) left right )=  (convertList left) ++ (convertList right)



toGraph :: SpTree a -> Graph Int
toGraph s =  addArcs emptyGraph (convertList (indexVertices s))


--------------
-- Graphviz --
--------------

-- | An SP-tree can be drawn in Graphviz
--
-- For the diamond (the colors of the nodes are arbitrary):
--
-- >>> putStrLn . graphvizString . indexNodes $ diamond
-- digraph {
--     3 [label="P", fillcolor=peachpuff, style=filled];
--     1 [label="S", fillcolor=seagreen1, style=filled];
--     0 [label="L"];
--     2 [label="L"];
--     5 [label="S", fillcolor=seagreen1, style=filled];
--     4 [label="L"];
--     6 [label="L"];
--     3 -> 1;
--     3 -> 5;
--     1 -> 0;
--     1 -> 2;
--     5 -> 4;
--     5 -> 6;
-- }
-- ...

instance Show a => Graphviz (NodeIndexedSpTree a) where
    graphvizNodesList (LeafNode (id, a)) = [(show id,show a,"")]
    graphvizNodesList (SeriesNode (id, a) left right) = [(show id,show a,"")] ++ (graphvizNodesList left) ++ (graphvizNodesList right)
    graphvizNodesList (ParallelNode (id, a) left right) = [(show id,show a,"")] ++ (graphvizNodesList left) ++ (graphvizNodesList right)

    graphvizArcsList (LeafNode (id, a)) = [(show id,"","")]
    graphvizArcsList (SeriesNode (id, a) left right) = (zipWith (\(id1,id2,z) (id1',id2',z2) -> (id1,id1',z2)) (cycle [(show id, "","")]) ((graphvizArcsList left) ++ (graphvizArcsList right)))
    graphvizArcsList (ParallelNode (id, a) left right) = (zipWith (\(id1,id2,z) (id1',id2',z2) -> (id1,id1',z2)) (cycle [(show id, "","")]) ( (take 1 (graphvizArcsList left)) ++ (take 1(graphvizArcsList right)))) ++((graphvizArcsList left) ++ (graphvizArcsList right))


--------------
-- Examples --
--------------

-- | A leaf node with "L"
leaf = LeafNode "L"
-- | A leaf node with "S"
series = SeriesNode "S"
-- | A leaf node with "P"
parallel = ParallelNode "P"

-- | The diamond-shaped SP-graph of 4 vertices
diamond = parallel (series leaf leaf)
                   (series leaf leaf)

-- | The red example found on the Wikipedia page
wikipediaRedExample
    = parallel
        (series
            leaf
            (series
                (parallel
                    (series leaf leaf)
                    (parallel
                        (series leaf leaf)
                        (series leaf leaf)
                    )
                )
                leaf
            )
        )
        (series leaf leaf)

-- | The blue example found on the Wikipedia page
wikipediaBlueExample
    = series
        leaf
        (parallel
            (series
                leaf
                (series leaf leaf)
            )
            (series
                leaf
                (series leaf leaf)
            )
        )
