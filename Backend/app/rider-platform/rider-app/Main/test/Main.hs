import EulerHS.Prelude
import qualified LocationFallbackTests
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "rider-app" [LocationFallbackTests.tests]
