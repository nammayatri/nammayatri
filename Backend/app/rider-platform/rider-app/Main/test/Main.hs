module Main (main) where

import qualified PartnerAuth.BhimAES as BhimAES
import Test.Tasty
import Prelude

main :: IO ()
main = defaultMain $ testGroup "rider-app-tests" [BhimAES.tests]
