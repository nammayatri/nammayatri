{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabExpiryTests (tests) where

import "beckn-spec" BecknV2.FRFS.Enums (ServiceTierType (AC))
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import qualified "rider-app" Domain.Types.VehicleTrip as DVT
import "mobility-core" Kernel.Types.Id (Id (..))
import "rider-app" SharedLogic.SharedCab.SessionState
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 9 25) 0

minutes :: Int -> UTCTime
minutes n = addUTCTime (fromIntegral (n * 60)) t0

-- 15 min pause, 60 min end, last ping at t0.
decide :: Int -> SessionStatus -> Maybe ExpiryAction
decide nowMin = expiryAction (15 * 60) (60 * 60) (minutes nowMin) t0

liveTrip :: DVT.VehicleTrip
liveTrip =
  DVT.VehicleTrip
    { id = Id "trip7",
      serviceTierType = AC,
      driverId = "d1",
      vehicleNumber = "ML05A1234",
      capacity = 4,
      merchantId = Id "m",
      merchantOperatingCityId = Id "moc",
      integratedBppConfigId = Id "ibc",
      routeCode = "SC-R2",
      status = DVT.ACTIVE,
      startedAt = t0,
      movingAt = Nothing,
      reachedEndAt = Nothing,
      endedAt = Nothing,
      endReason = Nothing,
      offlineBoardings = 3,
      createdAt = t0,
      updatedAt = t0
    }

tests :: TestTree
tests =
  testGroup
    "SharedCab expiry + flush recovery"
    [ testCase "a fresh ping keeps the session" $
        decide 14 ACTIVE @?= Nothing,
      testCase "stale ping past pauseAfter pauses an ACTIVE session" $
        decide 15 ACTIVE @?= Just PauseSilent,
      testCase "an already PAUSED session isn't paused again" $
        decide 30 PAUSED @?= Nothing,
      testCase "stale ping past endAfter ends a PAUSED session" $
        decide 60 PAUSED @?= Just EndSilent,
      testCase "stale ping past endAfter ends an ACTIVE one too (missed ticks)" $
        decide 90 ACTIVE @?= Just EndSilent,
      testCase "an ENDED session is left alone" $
        decide 90 ENDED @?= Nothing,
      testCase "recovery restores route, driver, capacity and trip from the row" $
        case sessionFromTrip liveTrip (minutes 5) of
          Session {routeCode = r, driverId = d, capacity = c, vehicleTripId = t, status = st} ->
            (r, d, c, t, st) @?= ("SC-R2", "d1", 4, Id "trip7", ACTIVE),
      testCase "recovery keeps the walk-up mirror (offlineBoardings is durable)" $
        walkupCount (sessionFromTrip liveTrip (minutes 5)) @?= 3,
      testCase "a PAUSED trip comes back paused (pause is non-terminal)" $
        status (sessionFromTrip liveTrip {DVT.status = DVT.PAUSED} (minutes 5)) @?= PAUSED,
      testCase "recovery's version outranks any pre-flush counter" $
        (version (sessionFromTrip liveTrip (minutes 5)) > 1000000) @?= True
    ]
