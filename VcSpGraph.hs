{-|
Module      : VcSpGraph
Description : Computing minimum vertex covers of series-parallel graphs
Copyright   : (c) Alexandre Blondin Massé
License     : GPL-3
Maintainer  : blondin_masse.alexandre@uqam.ca
Stability   : experimental

This module provides functions and types for computing minimum vertex covers of
series-parallel graphs. See the [Wikipedia
page](https://en.wikipedia.org/wiki/Vertex_cover) for more details about the
/minimum vertex cover problem/.
 -}

module VcSpGraph (
    -- * Representation
    VcType (None, Src, Sink, Both), VertexCover, VertexCovers,
    -- * Basic operations
    compareCover, showVertexCovers, mergeCovers,
    -- * Computing the minimum vertex covers for a series-parallel graph
    computeVcSpGraph, minimumVc, minimumVcGraph
) where

import Data.Array (Array)
import Data.Ix (Ix)

import InfInt
import SpTree
import Graph

-- | Composition type
data CType
    -- | Series composition type
    = Ser
    -- | Parallel composition type
    | Par

-- | There are four types of vertex cover
data VcType
    -- | The vertex cover includes neither the source nor the sink
    = None
    -- | The vertex cover includes the source, but not the sink
    | Src
    -- | The vertex cover includes the sink, but not the source
    | Sink
    -- | The vertex cover includes both the source and the sink
    | Both
    deriving (Ix, Eq, Ord, Show)

-- | A vertex cover is a triple @(t,s,c)@ where
--
-- * @t@ is the type of the cover (either @None@, @Src@, @Sink@ or @Both@)
-- * @s@ is the size of the cover
-- * @c@ is the list of the vertices in the cover
type VertexCover = (VcType, InfInt, [Int])

-- | A map associating vertex cover types with a vertex cover
type VertexCovers = Array VcType VertexCover

-- | Compares two vertex covers
--
-- It simply compares the size of the covers.
--
-- >>> compareCover (Sink, 1, [1]) (Both, 1, [3])
-- EQ
-- >>> compareCover (None, 1, [2]) (Src, 4, [1,3,4,6])
-- LT
-- >>> compareCover (None, PInf, []) (Src, 1, [3])
-- GT
compareCover :: VertexCover -> VertexCover -> Ordering
compareCover = error "À compléter"

-- | Shows vertex covers in a more readable way
--
-- >>> covers = [(None,PInf,[]), (Src,1,[0]), (Sink,1,[1]), (Both,2,[0,1])]
-- >>> putStrLn . showVertexCovers . listArray (None,Both) $ covers
-- None: []
--  Src: [0]
-- Sink: [1]
-- Both: [0,1]
showVertexCovers :: VertexCovers -> String
showVertexCovers = error "À compléter"

-- | Merge two vertex covers
--
-- >>> mergeCovers Ser (Src,1,[0]) (Sink,1,[2])
-- (Both,2,[0,2])
-- >>> mergeCovers Ser (Sink,1,[1]) (Src,1,[1])
-- (None,1,[1])
-- >>> mergeCovers Par (Src,1,[0]) (Sink,1,[2])
-- (Both,2,[0,2])
-- >>> mergeCovers Par (Both,2,[0,4]) (Both,3,[0,2,4])
-- (Both,3,[0,2,4])
mergeCovers :: CType -> VertexCover -> VertexCover -> VertexCover
mergeCovers = error "À compléter"

-- | Computes vertex covers for each node in an SP-tree
--
-- >>> import Graphviz
-- >>> t = computeVcSpGraph $ indexVertices wikipediaRedExample
-- >>> t' = fmap showVertexCovers t
-- >>> putStrLn . graphvizString . indexNodes $ t'
-- digraph {
--     15 [label="None: [1,5,6]\n Src: [0,1,5,6]\nSink: [1,5,6,7]\nBoth: [0,1,5,7]", fillcolor=peachpuff, style=filled];
--     1 [label="None: [1,5]\n Src: [0,1,5]\nSink: [1,5,7]\nBoth: [0,1,5,7]", fillcolor=seagreen1, style=filled];
--     0 [label="None: []\n Src: [0]\nSink: [1]\nBoth: [0,1]"];
--     13 [label="None: [2,3,4,5]\n Src: [1,5]\nSink: [2,3,4,7]\nBoth: [1,5,7]", fillcolor=seagreen1, style=filled];
--     5 [label="None: [2,3,4]\n Src: [1,2,3,4]\nSink: [2,3,4,5]\nBoth: [1,5]", fillcolor=peachpuff, style=filled];
-- ...
computeVcSpGraph :: SpTree (Int,Int,a) -> SpTree VertexCovers
computeVcSpGraph = error "À compléter"

-- | Computes a minimum vertex cover for an SP-tree
--
-- >>> t = indexVertices $ wikipediaRedExample
-- >>> rootContent t
-- (0,7,"P")
-- >>> minimumVc t
-- [1,5,6]
minimumVc :: SpTree (Int,Int,a) -> [Int]
minimumVc = error "À compléter"

-- | Computes a graph and one of its minimum vertex cover from an SP-tree
--
-- >>> t = indexVertices $ wikipediaRedExample
-- >>> import Graphviz
-- >>> putStrLn . graphvizString . minimumVcGraph $ t
-- digraph {
--     0 [label="0"];
--     1 [label="1", style=filled, fillcolor="red"];
--     2 [label="2"];
--     3 [label="3"];
--     4 [label="4"];
--     5 [label="5", style=filled, fillcolor="red"];
--     6 [label="6", style=filled, fillcolor="red"];
--     7 [label="7"];
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
minimumVcGraph :: SpTree (Int,Int,a) -> Graph Int
minimumVcGraph = error "À compléter"
