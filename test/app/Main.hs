module Main where

import EulerHS.Prelude
import qualified Mobility.Spec as Mobility
import qualified MockAppBackend.Spec as MockAppBackend
import Test.Tasty

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs = do
  mobilityTests <- Mobility.mkTestTree
  mockAppBackendTests <- MockAppBackend.mkTestTree
  return $ testGroup "all" [mobilityTests, mockAppBackendTests]
