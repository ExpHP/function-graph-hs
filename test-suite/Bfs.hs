module Bfs(testSuite) where
import ExampleGraphs
import TestUtils

import qualified Data.HashSet as HashSet
import qualified Data.List as List

import Data.Graph.Function

infix 1 `assertEqMultiset`
-- | Compare lists like multisets (i.e. unordered, but duplicates matter)
assertEqMultiset :: (Show a,Ord a)=> [a] -> [a] -> Assertion
assertEqMultiset a b = List.sort a @?= List.sort b

{---- Tests that all traversal algorithms need ----}
-- These test basic functionality and edge cases that all BFS-style
--  traversal functions need to worry about.

-- They are organized by graph simply because it is easier to write them
--  that way.

testsByGraph :: [TestTree]
testsByGraph =
	[ testBasic
	, testIsolated
	, testInfinite
	, testForwardEdge
	, testCrossEdge
	, testDoubleEdge
	, testSelfLoop
	]

testBasic :: TestTree
testBasic = "basic tests (sinkRay)" ~::~
	[ "bfsOrder" ~:         bfsOrder g root         @?= [3,2,1,0]
	, "bfsEdgesTraveled" ~: bfsEdgesTraveled g root @?= zip [3,2,1] [2,1,0]
	, "bfsEdgesSeen" ~:     bfsEdgesSeen g root     @?= zip [3,2,1] [2,1,0]
	, "groupsByDistance" ~:
		take 4 (groupsByDistance g [root])
		@?= map HashSet.singleton [3,2,1,0]
	] where g = sinkRay; root = 3

testIsolated :: TestTree
testIsolated = "isolated vertex" ~::~
	[ "bfsOrder" ~:         bfsOrder g root         @?= [0]
	, "bfsEdgesTraveled" ~: bfsEdgesTraveled g root @?= []
	, "bfsEdgesSeen" ~:     bfsEdgesSeen g root     @?= []
	, "groupsByDistance" ~:
		take 4 (groupsByDistance g [root])
		@?= map HashSet.fromList [[0],[],[],[]]
	] where g = sinkRay; root = 0

-- BFS has no fear of the infinite.
testInfinite :: TestTree
testInfinite = "flirt with infinity (sourceRay)" ~::~
	[ "bfsOrder" ~:         take 4 (bfsOrder g root)         @?= [3,4,5,6]
	, "bfsEdgesTraveled" ~: take 3 (bfsEdgesTraveled g root) @?= zip [3,4,5] [4,5,6]
	, "bfsEdgesSeen" ~:     take 3 (bfsEdgesSeen g root)     @?= zip [3,4,5] [4,5,6]
	, "groupsByDistance" ~:
		take 4 (groupsByDistance g [root])
		@?= map HashSet.singleton [3,4,5,6]
	] where g = sourceRay; root = 3

-- In this traversal there are two ways to reach (0,True),
-- but only one should be in the spanning tree.
testForwardEdge :: TestTree
testForwardEdge = "forward edge (sinkLadder)" ~::~
	[ "bfsOrder" ~: assert $
		bfsOrder g root `elem` (map concat . sequence . map List.permutations) nodeGroups

	, "bfsEdgesTraveled" ~: do
		let actual = bfsEdgesTraveled g root
		length actual @?= 3
		let [a,b,c] = actual
		[a,b] `assertEqMultiset` edgeGroups !! 0
		assert $ c `elem` edgeGroups !! 1

	, "bfsEdgesSeen" ~: do
		let actual = bfsEdgesSeen g root
		take 2 actual `assertEqMultiset` edgeGroups !! 0
		drop 2 actual `assertEqMultiset` (edgeGroups !! 1) ++ backEdges

	, "groupsByDistance" ~:
		groupsByDistance g [root] @?= map HashSet.fromList nodeGroups
	]
	where
		g = sinkLadder
		nodeGroups = [[(1,False)], [(0,False),(1,True)], [(0,True)]]
		edgeGroups = [map ((,) root) mids, map (flip (,) final) mids]
		backEdges  = map (\(a,b) -> (b,a)) (edgeGroups !! 0)
		[[root], mids, [final]] = nodeGroups

-- This has a cross edge between two nodes in the same distance group.
testCrossEdge :: TestTree
testCrossEdge = "cross edge" ~::~
	[ "bfsOrder" ~:
		assert $ bfsOrder g root `elem` [[0,1,2], [0,2,1]]

	, "bfsEdgesTraveled" ~:
		bfsEdgesTraveled g root `assertEqMultiset` treeEdges

	, "bfsEdgesSeen" ~: do
		let actual = bfsEdgesSeen g root
		take 2 actual `assertEqMultiset` treeEdges
		drop 2 actual `assertEqMultiset` backEdges ++ crossEdges

	, "groupsByDistance" ~:
		groupsByDistance g [root] @?= map HashSet.fromList nodeGroups
	]
	where
		g = manyCompletes 3
		nodeGroups@([root]:_) = [[0], [1,2]]
		treeEdges  = [(0,1), (0,2)]
		backEdges  = [(1,0), (2,0)]
		crossEdges = [(1,2), (2,1)]

testDoubleEdge :: TestTree
testDoubleEdge = "double-edges" ~::~
	[ "bfsOrder" ~:         bfsOrder g root         @?= [2,1,0]
	, "bfsEdgesTraveled" ~: bfsEdgesTraveled g root @?= zip [2,1]     [1,0]
	, "bfsEdgesSeen" ~:     bfsEdgesSeen g root     @?= zip [2,2,1,1] [1,1,0,0]
	, "groupsByDistance" ~:
		take 4 (groupsByDistance g [root])
		@?= map HashSet.fromList [[2],[1],[0],[]]

	-- these are done on a graph with at least two steps just in case the
	-- implementation manually initializes the first step
	] where g = withDoubleEdges sinkRay; root = 2

testSelfLoop :: TestTree
testSelfLoop = "self-loops" ~::~
	[ "bfsOrder" ~:         bfsOrder g root         @?= [2,1,0]
	, "bfsEdgesTraveled" ~: bfsEdgesTraveled g root @?= zip [2,1] [1,0]

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
	"Bfs" ~::~
	[ "common" ~::~ testsByGraph
	, "extra"  ~::~ testsByFunction
	]

-- vim: set nolist
