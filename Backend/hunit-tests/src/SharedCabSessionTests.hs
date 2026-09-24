{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabSessionTests (tests) where

import "beckn-spec" BecknV2.FRFS.Enums (ServiceTierType (AC))
import Data.Time (UTCTime (..), fromGregorian)
import qualified "rider-app" Domain.Types.VehicleTrip as DVT
import "mobility-core" Kernel.Types.Id (Id (..))
import "rider-app" SharedLogic.SharedCab.SessionState
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import "rider-app" Tools.Error (SharedCabSessionError (..))
import Prelude

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 9 25) 0

active :: Session
active =
  Session
    { driverId = "d1",
      vehicleNumber = "ML05A1234",
      merchantId = Id "m",
      merchantOperatingCityId = Id "moc",
      integratedBppConfigId = Id "ibc",
      serviceTierType = AC,
      routeCode = "R1",
      queuedRouteCode = Nothing,
      capacity = 4,
      walkupCount = 0,
      status = ACTIVE,
      pauseReason = Nothing,
      consecutiveMisses = 0,
      version = 3,
      startedAt = t0,
      vehicleTripId = Id "trip1"
    }

tests :: TestTree
tests =
  testGroup
    "SharedCab session state"
    [ testCase "second driver on the same plate is rejected" $
        planSelect "d2" "R1" (Just active) @?= Left SessionHeldByAnotherDriver,
      testCase "second driver can't drive another's session either" $
        ownedSession "d2" (Just active) @?= Left SessionHeldByAnotherDriver,
      testCase "an ENDED session frees the plate for another driver" $
        planSelect "d2" "R1" (Just (endSession active)) @?= Right OpenSession,
      testCase "same driver, same route is a no-op" $
        planSelect "d1" "R1" (Just active) @?= Right (KeepRoute active),
      testCase "same driver, other route is a route change" $
        planSelect "d1" "R2" (Just active) @?= Right (ChangeRoute active),
      testCase "route change points the session at the new trip" $
        switchRoute "R2" (Id "trip2") active
          @?= active {routeCode = "R2", vehicleTripId = Id "trip2", version = 4},
      testCase "route change leaves the old route set and joins the new one" $
        routeSetMoves (Just active) (switchRoute "R2" (Id "trip2") active) @?= RouteSetMoves ["R1"] ["R2"],
      testCase "route change closes the old trip as COMPLETED" $
        closedTripStatus DVT.ROUTE_CHANGED @?= DVT.COMPLETED,
      testCase "the opened trip is ACTIVE on the new route" $
        let trip = tripFor (switchRoute "R2" (Id "trip2") active) t0
         in (DVT.routeCode trip, DVT.status trip, DVT.endedAt trip) @?= ("R2", DVT.ACTIVE, Nothing),
      testCase "walk-up with a stale version is rejected" $
        setWalkup 2 1 active @?= Left SessionVersionMismatch,
      testCase "walk-up with the current version bumps it" $
        setWalkup 3 1 active @?= Right active {walkupCount = 1, version = 4},
      testCase "walk-ups above capacity are rejected" $
        setWalkup 3 5 active @?= Left InvalidWalkupCount,
      testCase "END_FOR_NOW leaves the route set" $
        routeSetMoves (Just active) (endSession active) @?= RouteSetMoves ["R1"] [],
      testCase "END_FOR_NOW closes the trip with its own reason" $
        endActionReason EndForNow @?= DVT.END_FOR_NOW,
      testCase "pause leaves the route set, resume rejoins" $
        let paused = either (error . show) id (pauseSession NO_LOCATION active)
         in (routeSetMoves (Just active) paused, routeSetMoves (Just paused) <$> resumeSession paused)
              @?= (RouteSetMoves ["R1"] [], Right (RouteSetMoves [] ["R1"])),
      testCase "a new session on a reused plate outranks the old version" $
        version (newSession (OpenSessionReq "d2" "ml 05-a 1234" (Id "m") (Id "moc") (Id "ibc") AC 4 "R1") (Id "trip3") t0 (Just active))
          @?= 4
    ]
