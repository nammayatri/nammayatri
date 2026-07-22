module Main (main) where

import qualified DriverInfoByPhoneSpec
import EulerHS.Prelude
import Test.Hspec (hspec)

main :: IO ()
main = hspec DriverInfoByPhoneSpec.spec
