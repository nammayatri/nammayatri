{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabExpiryTests (tests) where

import "beckn-spec" BecknV2.FRFS.Enums (ServiceTierType (SHARED_CAB))
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import "mobility-core" Kernel.Types.Id (Id (..))
import "rider-app" SharedLogic.SharedCab.SessionState
import qualified "rider-app" SharedLogic.SharedCab.TripClient as TC
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

liveTrip :: TC.DriverTrip
liveTrip =
  TC.DriverTrip
    { TC.id = "trip7",
      TC.driverId = "d1",
      TC.vehicleNumber = "ML05A1234",
      TC.agencyId = "feed:SHARED_CAB",
      TC.routeCode = "SC-R2",
      TC.capacity = Just 4,
      TC.walkupCount = 3,
      TC.startedAt = t0
    }

recovered :: TC.DriverTrip -> Session
recovered trip = sessionFromTrip (Id "m") (Id "moc") (Id "ibc") trip (minutes 5)

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
      testCase "recovery restores route, driver, capacity, trip and start from driver-app's run, under rider-app's ids" $
        case recovered liveTrip of
          Session {routeCode = r, driverId = d, capacity = c, vehicleTripId = t, status = st, startedAt = at, merchantOperatingCityId = city, serviceTierType = tier} ->
            (r, d, c, t, st, at, city, tier) @?= ("SC-R2", "d1", 4, "trip7", ACTIVE, t0, Id "moc", SHARED_CAB),
      testCase "recovery seeds walk-ups from the run's last driver count, capped at capacity" $
        (walkupCount (recovered liveTrip), walkupCount (recovered liveTrip {TC.walkupCount = 9})) @?= (3, 4),
      testCase "a run with no capacity on the vehicle gets the default" $
        case recovered liveTrip {TC.capacity = Nothing} of
          Session {capacity = c} -> c @?= defaultCapacity,
      testCase "a ping more than 5 min ahead (ms epoch, skewed clock) or missing is unreadable" $
        map (readPing (minutes 10)) [Just (minutes 9), Just (minutes 15), Just (minutes 16), Nothing]
          @?= [SeenAt (minutes 9), SeenAt (minutes 15), Unreadable, Unreadable],
      testCase "an unreadable ping is skipped only until the trip is endAfter old" $
        map (\(nowMin, ping) -> lastSeenFor (60 * 60) (minutes nowMin) t0 ping) [(30, Just Unreadable), (60, Just Unreadable), (30, Nothing), (30, Just (SeenAt (minutes 20)))]
          @?= [Nothing, Just t0, Just t0, Just (minutes 20)],
      testCase "recovery's version outranks any pre-flush counter" $
        (version (recovered liveTrip) > 1000000) @?= True
    ]
