module Bfs(testSuite) where
import ExampleGraphs
import TestUtils

import qualified Data.HashSet as HashSet
import qualified Data.List as List

import Data.Graph.Function

infix 1 `assertEqMultiset`, @?=*
-- | Compare lists like multisets (i.e. unordered, but duplicates matter)
assertEqMultiset :: (Show a,Ord a)=> [a] -> [a] -> Assertion
assertEqMultiset a b = List.sort a @?= List.sort b

-- | Check if the input is an element of the RHS.
(@?=*) :: (Eq a,Show a)=> a -> [a] -> Assertion
actual @?=* expecteds = actual `elem` expecteds @? "unexpected value: " ++ show actual

{---- Tests that all traversal algorithms need ----}
-- These test basic functionality and edge cases that all BFS-style
--  traversal functions need to worry about.

-- They are organized by graph simply because it is easier to write them
--  that way.

-- FIXME refactor;
--       this is getting to the point where I'm worried that I
--       might have a test call the wrong function and not see it

testBasic :: TestTree
testBasic = "basic tests (sinkRay)" ~::~
	[ "dfsPreOrder" ~:      dfsPreOrder g root      @?= [3,2,1,0]
	, "dfsPostOrder" ~:     dfsPostOrder g root     @?= [0,1,2,3]
	, "dfsEdgesTraveled" ~: dfsEdgesTraveled g root @?= zip [3,2,1] [2,1,0]
	, "dfsEdgesSeen" ~:     dfsEdgesSeen g root     @?= zip [3,2,1] [2,1,0]

	, "bfsOrder" ~:         bfsOrder g root         @?= [3,2,1,0]
	, "bfsEdgesTraveled" ~: bfsEdgesTraveled g root @?= zip [3,2,1] [2,1,0]
	, "bfsEdgesSeen" ~:     bfsEdgesSeen g root     @?= zip [3,2,1] [2,1,0]
	, "groupsByDistance" ~:
		take 4 (groupsByDistance g [root])
		@?= map HashSet.singleton [3,2,1,0]
	] where g = sinkRay; root = 3

testIsolated :: TestTree
testIsolated = "isolated vertex" ~::~
	[ "dfsPreOrder" ~:      dfsPreOrder g root      @?= [0]
	, "dfsPostOrder" ~:     dfsPostOrder g root     @?= [0]
	, "dfsEdgesTraveled" ~: dfsEdgesTraveled g root @?= []
	, "dfsEdgesSeen" ~:     dfsEdgesSeen g root     @?= []

	, "bfsOrder" ~:         bfsOrder g root         @?= [0]
	, "bfsEdgesTraveled" ~: bfsEdgesTraveled g root @?= []
	, "bfsEdgesSeen" ~:     bfsEdgesSeen g root     @?= []
	, "groupsByDistance" ~:
		take 4 (groupsByDistance g [root])
		@?= map HashSet.fromList [[0],[],[],[]]
	] where g = sinkRay; root = 0

testDoubleEdge :: TestTree
testDoubleEdge = "double-edges" ~::~
	-- Graph:  2 => 1 => 0
	[ "dfsPreOrder" ~:      dfsPreOrder g root      @?= [2,1,0]
	, "dfsPostOrder" ~:     dfsPostOrder g root     @?= [0,1,2]
	, "dfsEdgesTraveled" ~: dfsEdgesTraveled g root @?= [(2,1),(1,0)]
	, "dfsEdgesSeen" ~:     dfsEdgesSeen g root     @?= [(2,1),(1,0),(1,0),(2,1)]

	, "bfsOrder" ~:         bfsOrder g root         @?= [2,1,0]
	, "bfsEdgesTraveled" ~: bfsEdgesTraveled g root @?= [(2,1),(1,0)]
	, "bfsEdgesSeen" ~:     bfsEdgesSeen g root     @?= [(2,1),(2,1),(1,0),(1,0)]
	, "groupsByDistance" ~:
		take 4 (groupsByDistance g [root])
		@?= map HashSet.fromList [[2],[1],[0],[]]

	-- these are done on a graph with at least two steps just in case the
	-- implementation manually initializes the first step
	] where g = withDoubleEdges sinkRay; root = 2

testSelfLoop :: TestTree
testSelfLoop = "self-loops" ~::~
	-- Graph:  2 -> 1 -> 0  (with self-loops at each node)
	[ "dfsPreOrder" ~:      dfsPreOrder g root      @?= [2,1,0]
	, "dfsPostOrder" ~:     dfsPostOrder g root     @?= [0,1,2]
	, "dfsEdgesTraveled" ~: dfsEdgesTraveled g root @?= zip [2,1] [1,0]

	-- start dfsEdgesSeen closer to the sink as otherwise there are four
    --   different possible traversal orderings with no easy general description
	, "dfsEdgesSeen" ~:     dfsEdgesSeen g 1        @?=* [ [(1,1),(1,0),(0,0)]
	                                                     , [(1,0),(0,0),(1,1)] ]

	, "bfsOrder" ~:         bfsOrder g root         @?= [2,1,0]
	, "bfsEdgesTraveled" ~: bfsEdgesTraveled g root @?= zip [2,1] [1,0]

	-- FIXME should factor out tests like these too
	--  into some kind of [v] -> [[v]] -> Assertion
	, "bfsEdgesSeen" ~: do
		let actual = bfsEdgesSeen g root
		take 2 (drop 0 actual) `assertEqMultiset` [(2,1), (2,2)]
		take 2 (drop 2 actual) `assertEqMultiset` [(1,0), (1,1)]
		take 1 (drop 4 actual) `assertEqMultiset` [(0,0)]
		length actual @?= 5

	, "groupsByDistance" ~:
		take 4 (groupsByDistance g [root])
		@?= map HashSet.fromList [[2],[1],[0],[]]

	-- these are done on a graph with at least two steps just in case the
	-- implementation manually initializes the first step
	] where g = withSelfLoops sinkRay; root = 2

{---- Tests specific to BFS ----}

-- BFS has no fear of the infinite.
testInfiniteBfs :: TestTree
testInfiniteBfs = "flirt with infinity (sourceRay)" ~::~
	[ "bfsOrder" ~:         take 4 (bfsOrder g root)         @?= [3,4,5,6]
	, "bfsEdgesTraveled" ~: take 3 (bfsEdgesTraveled g root) @?= zip [3,4,5] [4,5,6]
	, "bfsEdgesSeen" ~:     take 3 (bfsEdgesSeen g root)     @?= zip [3,4,5] [4,5,6]
	, "groupsByDistance" ~:
		take 4 (groupsByDistance g [root])
		@?= map HashSet.singleton [3,4,5,6]
	] where g = sourceRay; root = 3

-- In this traversal there are two ways to reach (0,True),
-- but only one should be in the spanning tree.
testForwardEdgeBfs :: TestTree
testForwardEdgeBfs = "forward edge for BFS" ~::~
	[ "bfsOrder" ~: bfsOrder g root
		@?=* [ [0]++mid++[3] | mid <- List.permutations [1,2] ]

	, "bfsEdgesTraveled" ~: bfsEdgesTraveled g root
		@?=* [ pre++suf | pre <- List.permutations [(0,1),(0,2)]
		                , suf <- [[(1,3)], [(2,3)]]              ]

	, "bfsEdgesSeen" ~: bfsEdgesSeen g root
		@?=* [ pre++suf | pre <- List.permutations [(0,1),(0,2)]
		                , suf <- List.permutations [(1,3),(2,3)] ]

	, "groupsByDistance" ~:
		takeWhile (not.null) (groupsByDistance g [root])
		@?= map HashSet.fromList [[0],[1,2],[3]]
	]
	where
		-- a directed diamond shape
		g 0 = [1,2]
		g 1 = [3]  -- one of these will
		g 2 = [3]  -- be the forward edge
		g 3 = []
		g _ = error "bad node"
		root = 0 :: Int

-- This has a cross edge between two nodes in the same distance group.
testCrossEdgeBfs :: TestTree
testCrossEdgeBfs = "cross edge" ~::~
	[ "bfsOrder" ~: bfsOrder g root @?=* [[0,1,2], [0,2,1]]

	, "bfsEdgesTraveled" ~:
		bfsEdgesTraveled g root `assertEqMultiset` treeEdges

	, "bfsEdgesSeen" ~: do
		let actual = bfsEdgesSeen g root
		take 2 actual `assertEqMultiset` treeEdges
		drop 2 actual `assertEqMultiset` backEdges ++ crossEdges

	, "groupsByDistance" ~:
		takeWhile (not.null) (groupsByDistance g [root])
		@?= map HashSet.fromList nodeGroups
	]
	where
		g = manyCompletes 3
		nodeGroups@([root]:_) = [[0], [1,2]]
		treeEdges  = [(0,1), (0,2)]
		backEdges  = [(1,0), (2,0)]
		crossEdges = [(1,2), (2,1)]

{---- Tests specific to DFS ----}

-- !!! FRAGILE !!!
-- AFAICT it is not possible to write a graph that has at least one forward edge
--  in every single possible DFS traversal.
--
-- This test currently relies on the fact that dfsEvents iterates through
-- neighbors in the order returned by 'adj', and is DELIBERATELY written
-- break if that changes. (to signal that the test itself needs updating)
testForwardEdgeDfs :: TestTree
testForwardEdgeDfs = "forward edge for DFS (FRAGILE)" ~::~
	[ "dfsPreOrder" ~:      dfsPreOrder g root      @?= [0,1,2]
	, "dfsPostOrder" ~:     dfsPostOrder g root     @?= [2,1,0]
	, "dfsEdgesTraveled" ~: dfsEdgesTraveled g root @?= [(0,1), (1,2)]
	, "dfsEdgesSeen" ~:     dfsEdgesSeen g root     @?= [(0,1), (1,2), (0,2)]
	] where
        -- a directed triangle shape
		g 0 = [1,2] -- (0,2) is the forward edge
		g 1 = [2]
		g 2 = []
		g _ = error "bad node"
		root = 0 :: Int

testCrossEdgeDfs :: TestTree
testCrossEdgeDfs = "cross edge for DFS" ~::~
	[ "dfsPreOrder" ~:      dfsPreOrder g root      @?=* [ [0,1,3,2]
	                                                     , [0,2,3,1] ]
	, "dfsPostOrder" ~:     dfsPostOrder g root     @?=* [ [3,1,2,0]
	                                                     , [3,2,1,0] ]
	, "dfsEdgesTraveled" ~: dfsEdgesTraveled g root @?=* [ [(0,1),(1,3),(0,2)]
	                                                     , [(0,2),(2,3),(0,1)] ]
	, "dfsEdgesSeen" ~:     dfsEdgesSeen g root     @?=* [ [(0,1),(1,3),(0,2),(2,3)]
	                                                     , [(0,2),(2,3),(0,1),(1,3)] ]
	] where
		-- a directed diamond shape
		g 0 = [1,2]
		g 1 = [3]  -- one of these will
		g 2 = [3]  -- be the cross edge
		g 3 = []
		g _ = error "bad node"
		root = 0 :: Int

{---- Function-specific edge cases ----}

testsByFunction :: [TestTree]
testsByFunction =
	[ testGroupsByDistance
	]

testGroupsByDistance :: TestTree
testGroupsByDistance = "groupsByDistance" ~::~
	[ "no roots" ~:
			take 3 (f sinkRay [])
			@?= replicate 3 HashSet.empty

	, "ensure empty groups at end in general" ~:
			take 7 (f sinkRay [3])
			@?= map HashSet.singleton [3,2,1,0] ++ replicate 3 HashSet.empty

	, "multiple roots" ~::~
		[ "overlap after first iter" ~:
			take 5 (f sinkRay [5,3])
			@?= map HashSet.fromList [[5,3], [4,2], [1], [0], []]

		, "cross edge within root set" ~:
			take 5 (f sinkRay [5,4])
			@?= map HashSet.fromList [[5,4], [3], [2], [1], [0]]
		]
	]
	where f a b = groupsByDistance a b

testSuite :: TestTree
testSuite =
	"Traversal" ~::~
	[ "Common to DFS and BFS" ~::~
		[ testBasic
		, testIsolated
		, testDoubleEdge
		, testSelfLoop
		]
	, "DFS Specific" ~::~
		[ testInfiniteBfs
		, testForwardEdgeBfs
		, testCrossEdgeBfs
		]
	, "BFS Specific" ~::~
		[ testForwardEdgeDfs
		, testCrossEdgeDfs
		]
	, "Individual function specific" ~::~
		testsByFunction
	]
