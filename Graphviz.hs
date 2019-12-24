{-|
Module      : Graphviz
Description : A class for types that can be drawn as graphs
Copyright   : (c) Alexandre Blondin Massé
License     : GPL-3
Maintainer  : blondin_masse.alexandre@uqam.ca
Stability   : experimental

This module provides a class @Graphviz@ that allows a type (whenever this makes
sense) to be drawn as a graph by using the [Graphviz](http://graphviz.org/)
software.
 -}

module Graphviz (
    -- * Types for representing Graphviz nodes and arcs
    GraphvizNode, GraphvizArc,
    -- * Graphviz class
    Graphviz,
    -- * Functions provided by the Graphviz class
    graphvizNodesList,
    graphvizArcsList,
    graphvizString,
    graphvizHeader,
    graphvizFooter,
    graphvizNodes,
    graphvizArcs,
    graphvizFile
) where

import Data.List (intercalate)
import Text.Printf (printf)

-- | A triple @(id,label,style)@ where
--
-- * @id@ is the key identifying the node (must be unique)
-- * @label@ is any string compatible with Graphviz
-- * @style@ is any string describing the Graphviz style of the node
type GraphvizNode = (String, String, String)

-- | A triple @(id1,id2,label)@ where
--
-- * @id1@ is the key of the source vertex
-- * @id2@ is the key of the target vertex
-- * @label@ is the label of the arc
type GraphvizArc = (String, String, String)

-- | Class of types that can be converted to Graphviz strings
class Graphviz g where

    -- | The list of Graphviz nodes
    --
    -- It returns a list of all nodes of the graph, indicating the label and
    -- the style of each node.
    graphvizNodesList :: g -> [GraphvizNode]

    -- | The list of Graphviz arcs
    --
    -- It returns a list of all arcs of the graph, indicating the label of the
    -- arc.
    graphvizArcsList :: g -> [GraphvizArc]

    -- | The header of the Graphviz string
    --
    -- This is the string printed before writing the nodes and the arcs.
    graphvizHeader :: g -> String
    graphvizHeader _ = "digraph {\n"

    -- | The footer of the Graphviz string
    --
    -- This is the string printed after having written the nodes and the arcs.
    graphvizFooter :: g -> String
    graphvizFooter _ = "}\n"

    -- | The string of all Graphviz nodes, with their labels and styles
    graphvizNodes :: g -> String
    graphvizNodes g = (intercalate "\n" $ map printNode (graphvizNodesList g)) ++ "\n"
        where printNode (id, label, style) = printf "    %s [label=%s%s];"
                                             id
                                             (if label == "" then "\"\""
                                              else label)
                                             (if style == "" then ""
                                              else printf ", %s" style)

    -- | The string of all Graphviz arcs, with their labels
    graphvizArcs :: g -> String
    graphvizArcs g = (intercalate "\n" $ map printArc (graphvizArcsList g)) ++ "\n"
        where printArc (node1, node2, label)
                  = printf "    %s -> %s%s;" node1 node2
                          (if label == "" then ""
                           else printf " [label=%s]" label)

    -- | A valid string that can be processed by the Graphviz software
    graphvizString :: g -> String
    graphvizString g = graphvizHeader g ++
                       graphvizNodes g ++
                       graphvizArcs g ++
                       graphvizFooter g

    -- | Writes a valid Graphviz string to the given file
    graphvizFile :: g -> FilePath -> IO ()
    graphvizFile g path = writeFile path $ graphvizString g
