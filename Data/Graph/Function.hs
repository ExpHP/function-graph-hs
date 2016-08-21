-- | Graph algorithms without a graph data structure.
module Data.Graph.Function(
	{- TYPEDEFS AND ADAPTERS -}
	AdjacencyU,
	AdjacencyW,
	adjacencyToW,
	adjacencyToU,
	{- DEPTH-FIRST TRAVERSAL -}
	dfsPreOrder,
	dfsPostOrder,
	dfsEdgesTraveled,
	dfsEdgesSeen,
	{- BREADTH-FIRST TRAVERSAL -}
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

{---- TYPEDEFS AND ADAPTERS ----}

-- | Type of an unweighted adjacency function.
type AdjacencyU   v = v -> [v];
-- | Type of a weighted adjacency function.
type AdjacencyW w v = v -> [(v,w)];

-- | Augment an unweighted adjacency function with weights of 1.
adjacencyToW :: (Num w)=> AdjacencyU v -> AdjacencyW w v
adjacencyToW = (map (flip (,) 1) .)

-- | Project a weighted adjacency function to just the weights.
adjacencyToU :: AdjacencyW w v -> AdjacencyU v
adjacencyToU = (map fst .)

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
-- | in a pre-ordering.
dfsPreOrder :: (Eq v,Hashable v)=> AdjacencyU v -> v -> [v]
dfsPreOrder adj root = dfsEvents adj root >>= eventPreOrders

-- | Lazily perform a rooted depth-first-search and obtain the visited nodes
-- | in a post-ordering.
dfsPostOrder :: (Eq v,Hashable v)=> AdjacencyU v -> v -> [v]
dfsPostOrder adj root = dfsEvents adj root >>= eventPostOrders

-- NOTE: an edge post-ordering might also be useful.
-- | Lazily perform a rooted depth-first-search and obtain the (directed)
-- | spanning tree edges in a pre-ordering.
dfsEdgesTraveled :: (Eq v,Hashable v)=> AdjacencyU v -> v -> [(v,v)]
dfsEdgesTraveled adj root = dfsEvents adj root >>= eventEdgesTraveled

-- | Lazily perform a rooted depth-first-search and obtain the (directed) edges
-- | seen (regardless of whether they were traveled), in order of inspection.
dfsEdgesSeen :: (Eq v,Hashable v)=> AdjacencyU v -> v -> [(v,v)]
dfsEdgesSeen adj root = dfsEvents adj root >>= eventEdgesSeen

dfsEvents :: (Eq v,Hashable v)=> AdjacencyU v -> v -> [TraversalEvent v]
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
-- | visited, in order.
bfsOrder :: (Eq v,Hashable v)=> AdjacencyU v -> v -> [v]
bfsOrder adj root = bfsNodeEvents adj [root] >>= eventPreOrders

-- | Yields groups of increasing distance from a set of nodes.
-- | The first set yielded are the input nodes, the second set are
-- | the nodes whose minimum distance from an input node is exactly 1,
-- | and so on.
-- | Continues to yield empty sets when there are no more nodes.
-- |
-- | The nodes in the graph must be of finite degree.
groupsByDistance :: (Eq v,Hashable v)=> AdjacencyU v -> [v] -> [HashSet v]
groupsByDistance adj roots =
	(bfsNodeEvents adj roots >>= eventGroups) ++ cycle [HashSet.empty]

-- | Lazily perform a rooted breadth-first-search and obtain the (directed)
-- | spanning tree edges.
bfsEdgesTraveled :: (Eq v,Hashable v)=> AdjacencyU v -> v -> [(v,v)]
bfsEdgesTraveled adj root = bfsEdgeEvents adj [root] >>= eventEdgesTraveled

-- | Lazily perform a rooted breadth-first-search and obtain the (directed)
-- | edges seen (regardless of whether they were traveled).
bfsEdgesSeen :: (Eq v,Hashable v)=> AdjacencyU v -> v -> [(v,v)]
bfsEdgesSeen adj root = bfsEdgeEvents adj [root] >>= eventEdgesSeen

-- These two BFS implementations operate at different levels of granularity
-- (equidistant groups versus individual nodes).
bfsNodeEvents :: (Eq v,Hashable v)=> AdjacencyU v -> [v] -> [TraversalEvent v]
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
bfsEdgeEvents :: (Eq v,Hashable v)=> AdjacencyU v -> [v] -> [TraversalEvent v]
bfsEdgeEvents adj roots = start where
	start = iter (makeDequeue $ hashNub roots) (HashSet.fromList roots)
	iter queue seen = case Dequeue.popFront queue of
		Nothing         -> []
		Just (s,queue') -> events ++ iter queue' seen' where
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
undirectedComponents :: (Eq v,Hashable v)=> AdjacencyU v -> [v] -> [HashSet v]
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
scComponents :: (Eq v,Hashable v)=> AdjacencyU v -> AdjacencyU v -> [v] -> [HashSet v]
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
biconnectedComponents :: (Eq v,Hashable v)=> AdjacencyU v -> [v] -> [HashSet v]
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

-- FIXME need tests
-- * Make sure BFS related tests include edges between nodes of equal breadth
