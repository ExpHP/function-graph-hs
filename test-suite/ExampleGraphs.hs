module ExampleGraphs(
	sinkRay,
	sourceRay,
	undirectedRay,
	sinkLadder,
	sourceLadder,
	manyCompletes,
	fibonacciGraph,
	nimGraph,
	withDoubleEdges,
	withSelfLoops,
	Sided, _L, _R,
	) where

import qualified Data.List as List
import Numeric.Natural -- base 4.8

(.<) :: (Enum a)=> a -> a -> [a]
a .< b = [a..pred b]

type Sided a = (a, Bool)
_L :: a -> Sided a
_L x = (x, False)
_R :: a -> Sided a
_R x = (x, True)

floorMultiple :: (Integral a)=> a -> a -> a
floorMultiple n = (* n).(`div` n)

{---- example graphs to use in tests ----}

-- ... -> o -> o -> o -> o
sinkRay :: Natural -> [Natural]
sinkRay 0 = []
sinkRay k = [k-1]

--        o -> o -> o -> o -> ...
sourceRay :: Natural -> [Natural]
sourceRay k = [k+1]

-- ... -- o -- o -- o -- o
undirectedRay :: Natural -> [Natural]
undirectedRay 0 = [1]
undirectedRay n = [n-1,n+1]

-- ... -> o -> o -> o -> o
--        |    |    |    |
-- ... -> o -> o -> o -> o
sinkLadder :: (Natural,Bool) -> [(Natural,Bool)]
sinkLadder (0,side) = [(0,not side)]
sinkLadder (k,side) = [(k,not side), (k-1,side)]

--        o -> o -> o -> o -> ...
--        |    |    |    |
--        o -> o -> o -> o -> ...
sourceLadder :: (Natural,Bool) -> [(Natural,Bool)]
sourceLadder (k,side) = [(k,not side), (k+1,side)]

-- Composed of an infinite number of disconnected complete graphs
manyCompletes :: Natural -> (Natural -> [Natural])
manyCompletes n k = List.delete k (floorMultiple n k .< floorMultiple n (k+n))

-- Dependency graph of the fibonacci function
fibonacciGraph :: Natural -> [Natural]
fibonacciGraph 0 = []
fibonacciGraph 1 = []
fibonacciGraph n = [n-1, n-2]

-- A game of nim, with canonicalized states.
-- The canonicalization actually makes this a multigraph.
nimGraph :: [Natural] -> [[Natural]]
nimGraph piles = foo $ canonicalize piles where
	foo []     = []
	foo [k]    = map canonicalize $ these k []
	foo (k:ks) = map canonicalize $ these k ks ++ those k ks

	these k ks = map (:ks) (0 .< k)  -- take from this pile
	those k ks = map (k:) (foo ks)    -- take from a future pile
	canonicalize = List.sort . filter (>0)

-- Examples of particularly EVIL graph features which are technically
--  permitted due to the simplistic (v -> [v]) input form.
-- Some graph properties do not have sensible definitions in these cases,
-- and even when they do, correct handling of them can be painful.

-- add self loops to every node in another graph
withSelfLoops :: (a -> [a]) -> (a -> [a])
withSelfLoops adj v = v:adj v

-- make double edges out of every edge in a graph
withDoubleEdges :: (a -> [a]) -> (a -> [a])
withDoubleEdges adj v = adj v ++ adj v

