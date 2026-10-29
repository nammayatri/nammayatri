{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabDegradedSweepTests (tests) where

import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import "beckn-spec" Domain.Types.FRFSTicketStatus (FRFSTicketStatus (..))
import qualified "rider-app" Domain.Types.MerchantOperatingCity as DMOC
import "mobility-core" Kernel.Types.Id (Id (..))
import "rider-app" SharedLogic.SharedCab.Degraded (shouldExpireDegraded)
import "rider-app" SharedLogic.SharedCab.Keys (refundRetryRunKey, sweepJobGuardKey, sweepLeaseKey, sweepRunKey)
import "rider-app" SharedLogic.SharedCab.Schedule (scanWindowStart)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Prelude

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 9 25) 0

-- | The company's current default degraded marker TTL (SharedLogic.SharedCab.Config.defaultTunables).
horizon :: Int
horizon = 60 * 60

seconds :: Int -> UTCTime
seconds n = addUTCTime (fromIntegral n) t0

-- | Sweep runs: every `cadence` seconds except a down window [downStart, downStart + downLen].
sweeps :: Int -> Int -> Int -> [UTCTime]
sweeps cadence downStart downLen =
  [seconds s | s <- [0, cadence .. 12 * 3600], s < downStart || s > downStart + downLen]

-- | Falsification model for the scan-window rule: a ride degraded at `createdAt` (the booking updatedAt the window keys on),
-- marker dead one horizon later, is lost iff no sweep time satisfies both "marker dead" and "window front still covers it".
caught :: UTCTime -> [UTCTime] -> Bool
caught createdAt =
  any (\t -> t >= addUTCTime (fromIntegral horizon) createdAt && createdAt >= scanWindowStart horizon t)

tests :: TestTree
tests =
  testGroup
    "SharedCab degraded sweep"
    [ testCase "the scan window is 3 horizons, not one" $
        scanWindowStart horizon (seconds (3 * horizon)) @?= t0,
      testCase "a sweep-down gap shorter than 2 horizons loses nothing" $ do
        let at = sweeps 120 (6 * 3600) (2 * horizon - 300)
            createdAts = [seconds c | c <- [0, 300 .. 8 * 3600]]
        assertBool "some candidate fell through the window" (all (`caught` at) createdAts),
      testCase "a gap of 2 horizons + page cadence DOES lose a degraded ride (rule is tight)" $ do
        let at = sweeps 120 (6 * 3600) (2 * horizon + 600)
            -- created exactly one horizon before the outage: killable in [downStart, downStart + 2h).
            lost = seconds (6 * 3600 - horizon)
        assertBool "expected the rule to genuinely lose this candidate" (not (caught lost at)),
      testCase "rider markDropped mid-sweep: the locked fresh re-read no-ops the double-fire" $ do
        -- A sweep reads a stale candidate and takes the booking lock, but the rider's "I got down" already flipped every ticket
        -- USED: re-decided on the fresh read, the rule owner refuses (no second USED flip, no second Dropped event).
        shouldExpireDegraded Nothing [USED] False @?= False,
      testCase "two cities, four keys each: one lease per city, no cross-talk" $ do
        let a = Id "city-a" :: Id DMOC.MerchantOperatingCity
            b = Id "city-b" :: Id DMOC.MerchantOperatingCity
            keysA = [sweepJobGuardKey a, sweepRunKey a, sweepLeaseKey a, refundRetryRunKey a]
            keysB = [sweepJobGuardKey b, sweepRunKey b, sweepLeaseKey b, refundRetryRunKey b]
        assertBool "per-city keys must differ across cities" (all (`notElem` keysB) keysA)
    ]
