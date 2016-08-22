import TestUtils
import qualified Dfs
import qualified Bfs
import Test.Tasty(defaultMain)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
	"All" ~::~
		[ "Searching" ~::~
			[ Dfs.testSuite
			, Bfs.testSuite
			]
		]

