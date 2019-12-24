{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : ColorSpGraph
Description : A module for building 3-coloring of SP-graphs
Copyright   : (c) Alexandre Blondin Massé
License     : GPL-3
Maintainer  : blondin_masse.alexandre@uqam.ca
Stability   : experimental

This module provides provides a linear algorithm that creates a 3-coloring of
an SP-graph.
 -}

module ColorSpGraph (
    -- * Coloring a series-parallel tree
    -- | The idea is to store in each node of the tree the color of the source
    -- and the sink. Since it can easily be computed recursively, it is then
    -- easy to convert it to a graph.
    colorSpTree,
    -- * Coloring a series parallel graph
    -- | Derives a colored series-parallel graph from the series-parallel tree
    -- representing it.
    colorSpGraph
) where

import Data.Map (Map,fromList,empty,insert,union)

import SpTree
import Graph

type Color = Int
type Coloring = Map Color Color
type ColorSpTree = SpTree (Color,Color)

-- | Computes a 3-coloring of the underlying SP-graph
--
-- The colors are stored in the source and sink of each node.
--
-- >>> colorSpTree leaf
-- Leaf (1,2)
-- >>> colorSpTree $ series leaf leaf
-- Ser (1,2) (Leaf (1,3)) (Leaf (3,2))
-- >>> colorSpTree $ diamond
-- Par (1,2) (Ser (1,2) (Leaf (1,3)) (Leaf (3,2))) (Ser (1,2) (Leaf (1,3)) (Leaf (3,2)))
colorSpTree' :: SpTree a -> Color-> Color->Color-> ColorSpTree
colorSpTree' (LeafNode value )cs ct c = LeafNode (cs,ct)
colorSpTree' (SeriesNode value left right )cs ct c = SeriesNode (cs,ct) (colorSpTree' left cs c ct) (colorSpTree' right c ct cs)
colorSpTree' (ParallelNode value left right )cs ct c = ParallelNode (cs,ct) (colorSpTree' left cs ct c) (colorSpTree' right cs ct c)

colorSpTree :: SpTree a -> ColorSpTree
colorSpTree sptree= colorSpTree' sptree 1 2 3


----------------------
-- Convert to graph --
----------------------

-- | Converts an SP-tree to an SP-graph with a 3-coloring
--
-- The colormap used is @1 -> red@, @2 -> green@ and @3 -> blue@.

-- >>> import Graphviz
-- >>> putStrLn . graphvizString . colorSpGraph $ wikipediaRedExample
-- digraph {
--     0 [label="0", style=filled, fillcolor="red"];
--     1 [label="1", style=filled, fillcolor="blue"];
--     2 [label="2", style=filled, fillcolor="green"];
--     3 [label="3", style=filled, fillcolor="green"];
--     4 [label="4", style=filled, fillcolor="green"];
--     5 [label="5", style=filled, fillcolor="red"];
--     6 [label="6", style=filled, fillcolor="blue"];
--     7 [label="7", style=filled, fillcolor="green"];
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

convertoIndexedTree :: ColorSpTree -> VerticesIndexedSpTree (Color,Color)
convertoIndexedTree sptree = indexVertices sptree


colorMap :: VerticesIndexedSpTree (Color,Color) -> Map Int String
colorMap  (LeafNode (indexSource,indexTail,(colorSource,colorTail)) )   =   union (insert indexSource (show colorSource) empty) (insert indexTail (show colorTail) empty)
colorMap  (SeriesNode (indexSource,indexTail,(colorSource,colorTail))  left right )   = union (union (insert indexSource (show colorSource) empty) (insert indexTail (show colorTail) empty)) (union (colorMap left ) (colorMap right ))
colorMap  (ParallelNode (indexSource,indexTail,(colorSource,colorTail)) left right )  = union (union (insert indexSource (show colorSource) empty) (insert indexTail (show colorTail) empty)) (union (colorMap left ) (colorMap right ))


colorSpGraph :: SpTree a -> Graph Int
colorSpGraph sptree = styleGraph (colorMap (convertoIndexedTree(colorSpTree sptree)) ) (toGraph sptree)
