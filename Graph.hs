{-|
Module      : Graph
Description : A module for handling directed graphs
Copyright   : (c) Alexandre Blondin Massé
License     : GPL-3
Maintainer  : blondin_masse.alexandre@uqam.ca
Stability   : experimental

This module provides functionalities for handling directed graphs. A directed
graph (or simply digraph) is a pair `G = (V,A)`, where `V` is a finite set
whose elements are called *vertices*, and `A` is a subset of `V x V`, whose
elements are called *arcs*.

The implementation is not very efficient. It uses associative tables under the
hood (from the module @Data.Map@). It also uses additional information on
vertices to simplify the use of Graphviz.
 -}

module Graph (
    -- * Data type
    Graph,
    -- * Constructors and modifiers
    -- | Vertices and arcs can be added to a graph. Currently, there is no
    -- function that allows the removal of vertices or arcs.
    emptyGraph, addVertex, addVertices, addArc, addArcs,
    -- * Basic queries
    -- | Basic information about vertices, arcs and neighbors are provided.
    hasVertex, vertices, numVertices, successors, predecessors,
    hasArc, arcs, numArcs,
    -- * Labels and styles
    -- | It is possible to draw a graph using Graphviz (see @Graphviz@). To
    -- facilitate this, the label and style of each vertex can be indicated by
    -- using the @labelGraph@ and @styleGraph@ functions.
    labelGraph, unlabelGraph, styleGraph
) where
import Data.Map (Map,empty,member,insert,keys,findWithDefault,assocs ,fromList,(!),union,mapWithKey,toList,elems)
import Graphviz 


-- | A directed graph
data Graph v = Graph
    { arcsMap :: Map v [v]     -- A map associating a vertex with its successors
    , labelMap :: Map v String -- The Graphviz label of each node
    , styleMap :: Map v String -- The Graphviz style of each node
    }deriving (Show,Eq, Ord)

-- | Returns an empty graph
--
-- >>> numVertices emptyGraph
-- 0
emptyGraph :: Graph v
emptyGraph = Graph empty empty empty

-- | Indicates if a vertex belongs to a graph
--
-- >>> hasVertex 1 emptyGraph
-- False
-- >>> hasVertex 1 $ addVertex 1 emptyGraph
-- True
-- >>> hasVertex 1 $ addArc (2,1) emptyGraph
-- True
-- >>> hasVertex 3 $ addArc (2,1) emptyGraph
-- False
hasVertex :: Ord v => v -> Graph v -> Bool
hasVertex v (Graph arcs labels styles) = member v arcs

-- | Adds a vertex to a graph
--
-- Note: if the vertex already belongs to the graph, the graph remains
-- unchanged.
--
-- >>> vertices $ addVertex 1 emptyGraph
-- [1]
-- >>> vertices $ addVertices emptyGraph [1,2,1]
-- [1,2]
addVertex :: Ord v => v -> Graph v -> Graph v
addVertex v (Graph arcs labels styles) = Graph (insert v [] arcs) labels styles

-- | Adds vertices to a graph
--
-- >>> numVertices $ addVertices emptyGraph "abc"
-- 3
addVertices :: Ord v => Graph v -> [v] -> Graph v
addVertices g v =  foldr addVertex g v

-- | Returns the list of vertices of a graph in ascending order
--
-- >>> vertices emptyGraph
-- []
-- >>> vertices $ addVertices emptyGraph [1,4,5,2,1]
-- [1,2,4,5]
vertices :: Graph v -> [v]
vertices = keys.arcsMap 

-- | Returns the number of vertices of a graph
--
-- >>> numVertices emptyGraph
-- 0
-- >>> numVertices $ addVertex 1 emptyGraph
-- 1
numVertices :: Graph v -> Int
numVertices g = length (vertices g)

-- | Returns the successors of a vertex in a graph in ascending order
--
-- We say that `v` is a successor of `u` in a graph `G` if the arc `(u,v)`
-- belongs to `G`.
--
-- Note: Returns the empty list if the vertex does not belong to the graph.
--
-- >>> successors 1 emptyGraph
-- []
-- >>> successors 1 $ addArc (1,2) emptyGraph
-- [2]
-- >>> successors 1 $ addArcs emptyGraph [(1,2),(2,3),(1,3)]
-- [2,3]
successors :: Ord v => v -> Graph v -> [v]
successors v (Graph arcs _ _) = findWithDefault [] v arcs

-- | Returns the predecessors of a vertex in a graph in ascending order
--
-- We say that `u` is a predecessor of `v` in a graph `G` if the arc `(u,v)`
-- belongs to `G`.
--
-- Note: Returns the empty list if the vertex does not belong to the graph.
--
-- >>> predecessors 1 emptyGraph
-- []
-- >>> predecessors 2 $ addArc (1,2) emptyGraph
-- [1]
-- >>> predecessors 3 $ addArcs emptyGraph [(1,2),(2,3),(1,3)]
-- [1,2]
predecessors :: Ord v => v -> Graph v -> [v]
predecessors v  (Graph arcs _ _) = map (fst)  ( (filter (\(x,y) -> elem v y) (assocs arcs) ) )

-- | Indicates if an arc belongs to a graph
--
-- Note: If one of the given vertices does not belong to the graph, then it
-- returns False.
--
-- >>> hasArc (1,2) emptyGraph
-- False
-- >>> hasArc (1,2) $ addArc (1,2) emptyGraph
-- True
-- >>> hasArc (1,2) $ addArc (2,1) emptyGraph
-- False
hasArc :: Ord v => (v,v) -> Graph v -> Bool
hasArc (x,y) (Graph arcs labels styles) = (member x arcs) && ( y `elem` (arcs ! x))

-- | Adds a (labelled) arc to a graph
--
-- Note: If the vertices of the arc do not belong to the graph, they are first
-- added. Also, if the arc already exists, the graph remains unchanged, even if
-- the arc label is different.
--
-- >>> numArcs $ addArc (1,2) emptyGraph
-- 1
-- >>> arcs $ addArc (1,2) $ addArc (1,2) emptyGraph
-- [(1,2)]
addArc :: Ord v => (v,v) -> Graph v -> Graph v
addArc (x,y) (Graph arcs labels styles) 

    |(hasArc (x,y) (Graph arcs labels styles)) == True=(Graph arcs labels styles) --if the arc already exists, we do nothing

    |(hasVertex x (Graph arcs labels styles) == False && hasVertex y (Graph arcs labels styles) ==False )  = addArc (x,y)  (addVertices (Graph arcs labels styles) [x,y] ) --if the vertex x and y dosnt exist
    
    |(hasVertex x (Graph arcs labels styles) == True && hasVertex y (Graph arcs labels styles) ==False )  = addArc (x,y)  (addVertices (Graph arcs labels styles) [y] ) --if the vertex x and y dosnt exist
    
    |(hasVertex x (Graph arcs labels styles) == False && hasVertex y (Graph arcs labels styles) ==True )  = addArc (x,y)  (addVertices (Graph arcs labels styles) [x] ) --if the vertex x and y dosnt exist

    |otherwise = (Graph (insert x (successors x (Graph arcs labels styles)++[y]) arcs) labels styles )--if not, we add y to the list of succesors of x

-- | Adds multiple arcs to a graph
--
-- This is the same as calling @addArc@ multiple times.
--
-- >>> arcs $ addArcs emptyGraph [(2,3),(1,3),(1,2)]
-- [(1,2),(1,3),(2,3)]
addArcs :: Ord v => Graph v -> [(v,v)] -> Graph v
addArcs g x = foldr addArc g x

-- | Returns the arcs of a given graph in ascending order
--
-- >>> arcs $ addArcs emptyGraph [(2,3),(1,3),(1,2)]
-- [(1,2),(1,3),(2,3)]
arcs :: Ord v => Graph v -> [(v,v)]
arcs g= [(x,y) | x <- vertices g, y <-successors x g ]

-- | Returns the number of arcs in a graph
--
-- >>> numArcs emptyGraph
-- 0
-- >>> numArcs $ addArcs emptyGraph [(2,3),(1,3),(1,2)]
-- 3
numArcs :: Ord v => Graph v -> Int
numArcs g = length (arcs g)

-- | Labels the vertices of a graph
--
-- >>> g = addArcs emptyGraph [(1,2),(2,3),(1,3)]
-- >>> labels = fromList [(1,"alpha"),(2,"beta"),(3,"gamma")]
-- >>> putStrLn $ graphvizString $ labelGraph labels g
-- digraph {
--     1 [label="alpha"];
--     2 [label="beta"];
--     3 [label="gamma"];
--     1 -> 2;
--     1 -> 3;
--     2 -> 3;
-- }
-- ...
--
labelGraph :: Ord v => Map v String -> Graph v -> Graph v
labelGraph  x (Graph arcs labels styles) = Graph arcs (union x labels) styles

-- | Replaces the labels of each vertex by an empty string
--
-- >>> g = addArcs emptyGraph [(1,2),(2,3),(1,3)]
-- >>> putStrLn $ graphvizString $ unlabelGraph g
-- digraph {
--     1 [label=""];
--     2 [label=""];
--     3 [label=""];
--     1 -> 2;
--     1 -> 3;
--     2 -> 3;
-- }
-- ...
unlabelGraph :: Ord v => Graph v -> Graph v
unlabelGraph  (Graph arcs labels styles) =
    (Graph arcs (mapWithKey f labels) styles)
    where f key value = ""                               

-- | Stylizes the vertices of a graph
--
-- >>> g = addArcs emptyGraph [(1,2),(2,3),(1,3)]
-- >>> style1 = "style=\"filled\", fillcolor=\"blue\""
-- >>> style2 = "style=\"filled\", fillcolor=\"red\""
-- >>> style3 = "style=\"filled\", fillcolor=\"green\""
-- >>> styles = fromList $ zip [1,2,3] [style1,style2,style3]
-- >>> putStrLn $ graphvizString $ styleGraph styles g
-- digraph {
--     1 [label="1", style="filled", fillcolor="blue"];
--     2 [label="2", style="filled", fillcolor="red"];
--     3 [label="3", style="filled", fillcolor="green"];
--     1 -> 2;
--     1 -> 3;
--     2 -> 3;
-- }
-- ...
styleGraph :: Map v String -> Graph v -> Graph v
styleGraph s (Graph arcs labels styles)= (Graph arcs labels s)

-- | Converts a graph to Graphviz
--
-- >>> putStrLn $ graphvizString $ addArcs emptyGraph [(1,2),(2,3),(1,3)]
-- digraph {
--     1 [label="1"];
--     2 [label="2"];
--     3 [label="3"];
--     1 -> 2;
--     1 -> 3;
--     2 -> 3;
-- }
-- ...
instance (Show v, Ord v) => Graphviz (Graph v) where
    graphvizNodesList (Graph arcs labels styles) 
                    | null labels && null styles = [(show id,(show id),("")) |  id<-(keys arcs)]
                    | (null labels) && (null styles==False )= [(show id,(show id),(styles ! id)) |  id<-(keys arcs)]
                    | (null labels==False) && (null styles )= [(show id,(labels ! id),("")) |  id<-(keys arcs)]





    graphvizArcsList (Graph arcs labels styles) 
                      | null labels && null styles = [(show id, show id2,("")) |  id<-(keys arcs), id2<-(arcs!id)  ]
                      | (null labels==False) && (null styles) =  [(show id, show id2,"") |  id<-(keys arcs), id2<-(arcs!id) ]
                      | (null labels) && (null styles== False) =  [(show id, show id2,("")) |  id<-(keys arcs), id2<-(arcs!id)  ]
