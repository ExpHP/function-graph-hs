module TestUtils(
	(~::~),
	(~:),

	{- RE-EXPORTS: tasty -}
	TestTree,

	{- RE-EXPORTS: tasty-hunit -}
	Assertion,
	(@=?),
	(@?=),
	) where

import Test.Tasty
import Test.Tasty.HUnit

-- HUnit's (~:), but for Tasty.
infixr 0 ~:
(~:) :: String -> Assertion -> TestTree
(~:) = testCase

-- A section header.
-- (note: HUnit's (~:) could do this as well, but I'm too lazy to write
--  the necessary typeclass for that to work)
-- (besides, ~::~ kinda looks nice)
infixr 0 ~::~
(~::~) :: String -> [TestTree] -> TestTree
(~::~) = testGroup
