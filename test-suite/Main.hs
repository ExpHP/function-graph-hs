import TestUtils
import qualified Bfs
import Test.Tasty(defaultMain)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
	"All" ~::~
		[ Bfs.testSuite
		]

