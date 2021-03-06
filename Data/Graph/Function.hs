{-|
Module      : Data.Graph.Function
Description : Graph algorithms without a graph data structure.
Copyright   : (c) Michael Lamparski, 2016
License     : MIT
Maintainer  : diagonaldevice@gmail.com
Stability   : experimental
Portability : lolidunno

There are wide variety of problems which can be easily reduced to
some sort of algorithm on a graph---and oftentimes, the easiest part
of this graph to obtain is its adjacency function.

But if you want to use a big graph library like fgl, you need to know
quite a bit more about your graph; such as a complete set of nodes.
This of course also means that you cannot have an infinitely large graph;
you must take a finite section of it, which may require plenty of
reasoning first to decide where the line can be drawn.
This library provides lazy graph algoriths so that you don't have
to do that.

Note that with great abstraction comes poor efficiency;
those counting their clock cycles may want to look elsewhere.

The current selection is limited to just BFS and DFS traversals
on unweighted, directed graphs with unlabeled edges.
However, the intent is to expand it with more useful algorithms.

-}
-- FIXME these rules are kind of nonsensical and immediately stop making sense
--       if you consider e.g. undirected graphs;  dge-centric algorithms
--       on undirected graphs will want to treat (s,t) and (t,s) as the same
--       edge, but if multiple edges are distinct and unlabeled, how do we
--       identify the right '(s,t)'s to the right '(t,s)'s?

module Data.Graph.Function(
    -- * Assumptions:

	-- | Unless otherwise stated:
	--
	--      * graphs are directed
	--      * edges are unlabeled and unweighted
	--      * the edges @(u,v)@ and @(v,u)@ are considered distinct.
	--      * multiple edges are distinct
	--      * self-loops are permitted

	-- * Traversal
	-- | Most traversal functions accept:
	--
	--    * @v -> [v]@: An /out-edge function/ listing the neighbors of a node.
	--    * @v@: A /root node/ from which to begin the traversal.

	-- ** Depth-first search (DFS)
	dfsPreOrder,
	dfsPostOrder,
	dfsEdgesTraveled,
	dfsEdgesSeen,
	-- ** Breadth-first search (BFS)
	bfsOrder,
	bfsEdgesTraveled,
	bfsEdgesSeen,
	groupsByDistance,
	) where

import qualified Data.List as List
import Data.Hashable(Hashable)
import Data.HashSet(HashSet)
import qualified Data.HashSet as HashSet
import Data.Dequeue(BankersDequeue)
import qualified Data.Dequeue as Dequeue

{---- DFS/BFS IMPLEMENTATION DETAIL ----}

-- To reduce code duplication, BFS and DFS functions are implemented via
-- "central" methods that produce streams of events, which are then filtered
-- using the functions below.

-- CAUTION: There is a bit of a refactoring hazard in that multiple functions
--           produce values of this type even though none use all of the variants.
--          This could be alleviated by having separate Event types for each
--           event-producing function, but then I'd have to write twice as
--           many filtering functions. >_>
data TraversalEvent v = Enter v
                      | Exit v
                      | Peek v v Bool
                      | Group (HashSet v)
                      deriving (Show,Eq)

-- Event-filtering functions
eventPreOrders :: TraversalEvent v -> [v]
eventPreOrders (Enter v) = [v]
eventPreOrders _ = []
eventPostOrders :: TraversalEvent v -> [v]
eventPostOrders (Exit v) = [v]
eventPostOrders _ = []
eventEdgesSeen :: TraversalEvent v -> [(v,v)]
eventEdgesSeen (Peek s t _) = [(s,t)]
eventEdgesSeen _ = []
eventEdgesTraveled :: TraversalEvent v -> [(v,v)]
eventEdgesTraveled (Peek s t True) = [(s,t)]
eventEdgesTraveled _ = []
eventGroups :: TraversalEvent v -> [HashSet v]
eventGroups (Group g) = [g]
eventGroups _ = []

{---- DEPTH-FIRST SEARCH ----}

-- | Lazily perform a rooted depth-first-search and obtain the visited nodes
--   in a pre-ordering.
dfsPreOrder :: (Eq v, Hashable v) => (v -> [v]) -> v -> [v]
dfsPreOrder adj root = dfsEvents adj root >>= eventPreOrders

-- | Lazily perform a rooted depth-first-search and obtain the visited nodes
--   in a post-ordering.
dfsPostOrder :: (Eq v, Hashable v) => (v -> [v]) -> v -> [v]
dfsPostOrder adj root = dfsEvents adj root >>= eventPostOrders

-- NOTE: an edge post-ordering might also be useful.
-- | Lazily perform a rooted depth-first-search and obtain the (directed)
--   spanning tree edges in a pre-ordering.
dfsEdgesTraveled :: (Eq v, Hashable v) => (v -> [v]) -> v -> [(v,v)]
dfsEdgesTraveled adj root = dfsEvents adj root >>= eventEdgesTraveled

-- | Lazily perform a rooted depth-first-search and obtain the (directed) edges
--   seen (regardless of whether they were traveled), in order of inspection.
dfsEdgesSeen :: (Eq v, Hashable v) => (v -> [v]) -> v -> [(v,v)]
dfsEdgesSeen adj root = dfsEvents adj root >>= eventEdgesSeen

dfsEvents :: (Eq v,Hashable v)=> (v -> [v]) -> v -> [TraversalEvent v]
dfsEvents adj root = peek root [] HashSet.empty True where
	-- NOTE: the stack is a LIFO stack of (node, neighborIter) pairs tracking
	--       the unfinished work remaining for each node in the current path.

	-- Nothing left, period
	rec [] seen = []

	-- Backtrack from a leaf
	rec ((s,[]):stack) seen = Exit s:rec stack seen

	-- Look at a neighbor (in the context of another node)
	rec ((s,(t:ts)):stack) seen = Peek s t isNew:peek t ((s,ts):stack) seen isNew
		where isNew = not $ t `HashSet.member` seen

	-- Look at a potentially new node (in isolation of others)
	peek t stack seen True  = Enter t:rec ((t,adj t):stack) (HashSet.insert t seen)
	peek t stack seen False =         rec (          stack) seen

{---- BREADTH-FIRST SEARCH ----}

-- | Lazily perform a rooted breadth-first-search and obtain the nodes
--   visited, in order.
bfsOrder :: (Eq v, Hashable v) => (v -> [v]) -> v -> [v]
bfsOrder adj root = bfsNodeEvents adj [root] >>= eventPreOrders

-- FIXME:
--   Multiple times while writing tests for this I got memory errors
--     thanks to forgetting about the non-terminating output.
--   Perhaps the mathematical beauty is not worth this potential footgun?
--   Or maybe these issues won't be common in typical usage...
--     (typical test code differs vastly from typical "real" code)

-- | Yields groups of increasing distance from a set of nodes.
--   The first set yielded are the input nodes, the second set are
--   the nodes whose minimum distance from an input node is exactly 1,
--   and so on.
--   Continues to yield empty sets when there are no more nodes.
--
--   The nodes in the graph must be of finite degree.
groupsByDistance :: (Eq v, Hashable v)
     => (v -> [v]) -- ^ Out-edge function
     -> [v]        -- ^ Initial set of nodes
     -> [HashSet v]
groupsByDistance adj roots =
	(bfsNodeEvents adj roots >>= eventGroups) ++ cycle [HashSet.empty]

-- | Lazily perform a rooted breadth-first-search and obtain the (directed)
--   spanning tree edges.
--
--   There is a known bug that this doesn't handle multiple edges properly.
--   (it is debatable whether the bug ought to be fixed, or if multiple
--    edges ought to be forbidden in the first place)
bfsEdgesTraveled :: (Eq v, Hashable v) => (v -> [v]) -> v -> [(v,v)]
bfsEdgesTraveled adj root = bfsEdgeEvents adj [root] >>= eventEdgesTraveled

-- | Lazily perform a rooted breadth-first-search and obtain the (directed)
--   edges seen (regardless of whether they were traveled).
--
--   There is a known bug that this doesn't handle multiple edges properly.
bfsEdgesSeen :: (Eq v, Hashable v) => (v -> [v]) -> v -> [(v,v)]
bfsEdgesSeen adj root = bfsEdgeEvents adj [root] >>= eventEdgesSeen

-- These two BFS implementations operate at different levels of granularity
-- (equidistant groups versus individual nodes).
bfsNodeEvents :: (Eq v,Hashable v)=> (v -> [v]) -> [v] -> [TraversalEvent v]
bfsNodeEvents adj roots = start where
	start = rec (HashSet.fromList roots) (HashSet.fromList roots)
	rec group seen
		| HashSet.null group = []
		| otherwise          = events group ++ rec nextGroup seen' where
			seen' = seen `hashUnion` nextGroup
			nextGroup =
				(`HashSet.difference` seen) $
				(HashSet.fromList . concatMap adj . HashSet.toList) group
	events group = Group group:(map Enter $ HashSet.toList group)

-- FIXME: The current API technically accepts multigraphs, but the way this is
--        written assumes edges from a node are unique.
--        I wasn't sure how to cleanly account for multiple edges without adding
--        a lot of complexity to the code, so I knowingly left this bug in.
bfsEdgeEvents :: (Eq v,Hashable v)=> (v -> [v]) -> [v] -> [TraversalEvent v]
bfsEdgeEvents adj roots = start where
	start = iter (makeDequeue $ hashNub roots) (HashSet.fromList roots)
	iter queueIn seen = case Dequeue.popFront queueIn of
		Nothing         -> []
		Just (s,queue) -> events ++ iter queue' seen' where
			edges = [(s,t,not $ HashSet.member t seen) | t <- adj s]
			newTs = [t | (_,t,isNew) <- edges, isNew]

			events = [Peek s t isNew | (s,t,isNew) <- edges]
			seen' = seen `hashUnion` HashSet.fromList newTs
			queue' = foldl Dequeue.pushBack queue newTs

{---- UTILITY ----}

-- HashSet docs warn that for performance considerations, the smaller set
--  should be provided first.
-- Color me perhaps confused, then, over the module's implementation of
--  unions = List.foldl' union empty
hashUnion :: (Eq a,Hashable a)=> HashSet a -> HashSet a -> HashSet a
hashUnion = flip HashSet.union
hashUnions :: (Eq a,Hashable a)=> [HashSet a] -> HashSet a
hashUnions = List.foldl' hashUnion HashSet.empty

hashNub :: (Eq a,Hashable a)=> [a] -> [a]
hashNub = HashSet.toList . HashSet.fromList

-- Dequeue.fromList is, perhaps unhelpfully, polymorphic
makeDequeue :: [a] -> BankersDequeue a
makeDequeue = Dequeue.fromList


{---- IDEA BIN ----}

-- These are things that I think are reasonable to implement,
-- but do not yet feel up to implementing/testing.

-- | For an undirected graph, read out the components (which must be
-- | of finite size) that each contain at least one of the given nodes.
-- |
-- | It is a logical error to call this with a directed graph.
-- |
-- | @roots@ may be infinite.
undirectedComponents :: (Eq v, Hashable v)
                     => (v -> [v]) -- ^ Neighbor function (undirected graph)
                     -> [v]        -- ^ Root nodes to search from
                     -> [HashSet v]
undirectedComponents adj roots = undefined -- FIXME

-- | For a directed graph, read out the strongly connected components
-- | which are in the same component as any of the given nodes.
-- |
-- | The order of the returned SCCs is unspecified. (in particular, if the
-- | (weakly-connected) components in the graph are infinitely large, then
-- | there is no guarantee that all members of @roots@ will eventually
-- | be visited).
-- |
-- | @roots@ may be infinite.
scComponents :: (Eq v, Hashable v)
             => (v -> [v]) -- ^ Out-edge function
             -> (v -> [v]) -- ^ In-edge function
             -> [v]        -- ^ Root nodes to search from
             -> [HashSet v]
scComponents outEdges inEdges roots = undefined -- FIXME

-- | For an undirected graph, read out all biconnected components
-- | reachable from at least one of the given nodes.
-- | These are maximally-sized subgraphs in which the removal of any single
-- | node does not disconnect the graph.
-- |
-- | This follows the convention that dyads are biconnected components.
-- | (hence every (undirected) edge in the graph will appear in exactly one BCC)
-- |
-- | It is a logical error to call this with a directed graph.
-- |
-- | @roots@ may be infinite.
biconnectedComponents :: (Eq v, Hashable v)
                      => (v -> [v]) -- ^ Neighbor function (undirected graph)
                      -> [v]        -- ^ Root nodes to search from
                      -> [HashSet v]
biconnectedComponents adj roots = undefined -- FIXME

{-
-- FIXME does this have *any* advantage over Data.Function.Memoize?
-- FIXME precise signature needs a lot more thought.
-- | Create a memoized function from one defined via open recursion
-- |  on a directed acyclic graph.
memoFixDagU :: (Eq v,Hashable v)
            => AdjacencyU v
            -- The type of this argument uses parametricity to prevent the function from
            --  "inventing" new nodes, the intent being to guarantee that the graph
            --  accurately reflects the depedency tree of the function.
            --  (e.g. sinks are base cases, and et cetera.)
            -- FIXME: I don't know if it actually accomplishes this or not.
            -> (forall u. (u -> [u]) -> (u -> b) -> (u -> b))
            -> (v -> b)
memoFixDagU adj openRecProp = undefined -- FIXME use postorder to organize computations
-}

