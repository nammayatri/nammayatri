module Main (main) where

import qualified SharedCabProxyTests
import qualified SharedRunTests
import Test.Tasty (defaultMain, testGroup)
import Prelude

main :: IO ()
main = defaultMain $ testGroup "driver-app" [SharedCabProxyTests.tests, SharedRunTests.tests]
