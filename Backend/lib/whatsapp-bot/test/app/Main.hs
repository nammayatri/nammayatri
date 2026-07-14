-- | Tasty entrypoint for the whatsapp-bot golden characterization suite.
-- One test per embedded golden fixture (each covering its steps + trackerSteps),
-- plus the English copy spot-checks. See "GoldenReplay" for the harness.
module Main (main) where

import GoldenReplay (copyChecks, goldenTests)
import Kernel.Prelude
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "whatsapp-bot golden" (goldenTests ++ [copyChecks]))
