import EulerHS.Prelude
import qualified FRFS.UnitTests as UnitTests
import Test.Tasty

main :: IO ()
main = defaultMain UnitTests.allTests
