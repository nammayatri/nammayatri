-- | Tasty entrypoint for the whatsapp-bot golden characterization suite.
-- One test per embedded golden fixture (each covering its steps + trackerSteps),
-- plus the English copy spot-checks and the persisted-codec round-trips. See
-- "GoldenReplay" for the harness and "CodecSpec" for the Redis-schema guards.
module Main (main) where

import CodecSpec (codecSpec)
import GoldenReplay (copyChecks, fixtureGuards, goldenTests, rideTypeButtonLayoutChecks)
import Kernel.Prelude
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "whatsapp-bot golden" (goldenTests ++ [copyChecks, fixtureGuards, codecSpec, rideTypeButtonLayoutChecks]))
