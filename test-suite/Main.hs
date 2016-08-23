import TestUtils
import qualified Traversal
import Test.Tasty(defaultMain)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
	"All" ~::~
		[ Traversal.testSuite
		]

