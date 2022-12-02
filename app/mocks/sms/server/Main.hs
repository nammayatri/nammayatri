module Main where

import App (runMockSms)
import EulerHS.Prelude

main :: IO ()
main = runMockSms id
