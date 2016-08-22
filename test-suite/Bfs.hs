module Bfs(testSuite) where
import ExampleGraphs
import TestUtils

import qualified Data.HashSet as HashSet
import qualified Data.List as List

import Data.Graph.Function

infixl 0 .<
(.<) :: (Enum a)=> a -> a -> [a]
a .< b = [a..pred b]

--testBfsOrder :: Test
--testBfsOrder = do
--	bfsOrder sinkRay 6 @?= [6,5..0]
--	take 7 (groupsByDistance sinkRay [3]) @?= map HashSet.singleton [3,2,1,0]
--	                                       ++ replicate 3 HashSet.empty

testGroupsByDistance :: TestTree
testGroupsByDistance = "groupsByDistance" ~::~
	[ "it doesn't explode" ~:
			take 4 (g sinkRay [3])
			@?= map HashSet.singleton [3,2,1,0]

	, "empty groups" ~::~
		[ "nth iter" ~:
			take 7 (g sinkRay [3])
			@?= map HashSet.singleton [3,2,1,0] ++ replicate 3 HashSet.empty

		, "1st iter" ~:
			take 3 (g sinkRay [0])
			@?= [HashSet.singleton 0, HashSet.empty, HashSet.empty]

		, "0th iter" ~:
			take 3 (g sinkRay [])
			@?= replicate 3 HashSet.empty
		]

	, "multiple groups" ~::~
		[ "overlap after first iter" ~:
			take 5 (g sinkRay [5,3])
			@?= map HashSet.fromList [[5,3], [4,2], [1], [0], []]

		, "overlap at first iter" ~:
			take 5 (g sinkRay [5,4])
			@?= map HashSet.fromList [[5,4], [3], [2], [1], [0]]
		]

	, "flirt with infinity" ~::~
		[ "sourceRay" ~:
			take 20 (g sourceRay [7])
			@?= map HashSet.singleton (7 .< 7+20)
		]

	, "degree > 1" ~::~
		[ "basic (sinkLadder)" ~:
			take 3 (g sinkLadder [(7,False)])
			@?= map HashSet.fromList
				[[(7,False)]
				,[(6,False),(7,True)]
				,[(5,False),(6,True)]
				]

		, "cross edges within a group (complete)" ~:
			take 3 (g (manyCompletes 7) [10])
			@?= map HashSet.fromList [[10], List.delete 10 (7 .< 14), []]
		]
	]
	where g a b = groupsByDistance a b

testSuite :: TestTree
testSuite =
	"Bfs" ~::~
	[ testGroupsByDistance
	]

-- vim: set nolist
