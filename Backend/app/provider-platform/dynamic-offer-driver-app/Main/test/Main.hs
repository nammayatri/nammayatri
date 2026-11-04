module Main (main) where

import qualified SharedCabProxyTests
import Test.Tasty (defaultMain, testGroup)
import Prelude

main :: IO ()
main = defaultMain $ testGroup "driver-app" [SharedCabProxyTests.tests]
