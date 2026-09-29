{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabSessionTests (tests) where

import "beckn-spec" BecknV2.FRFS.Enums (ServiceTierType (AC))
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import qualified "rider-app" Domain.Types.VehicleTrip as DVT
import "mobility-core" Kernel.Types.Id (Id (..))
import qualified "rider-app" SharedLogic.SharedCab.Events as Events
import "rider-app" SharedLogic.SharedCab.Session (endDropBy)
import "rider-app" SharedLogic.SharedCab.SessionState
import qualified "rider-app" SharedLogic.SharedCab.SessionView as View
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
      testCase "afterLastDrop queues the change and stays on the current route" $
        queueRoute "R2" active @?= active {queuedRouteCode = Just "R2", version = 4},
      testCase "applying the queued route clears it" $
        queuedRouteCode (switchRoute "R2" (Id "trip2") (queueRoute "R2" active)) @?= Nothing,
      testCase "END_ROUTE closes the trip as COMPLETED with its own reason" $
        (endActionReason EndRoute, closedTripStatus (endActionReason EndRoute)) @?= (DVT.END_ROUTE, DVT.COMPLETED),
      testCase "walk-up with a stale version is rejected" $
        setWalkup 2 1 active @?= Left SessionVersionMismatch,
      testCase "walk-up with the current version bumps it" $
        setWalkup 3 1 active @?= Right active {walkupCount = 1, version = 4},
      testCase "walk-ups above capacity are rejected" $
        setWalkup 3 5 active @?= Left InvalidWalkupCount,
      testCase "R19 cab full: walk-ups take every seat the boarded riders don't" $
        fillCab 1 active {walkupCount = 1} @?= active {walkupCount = 3, version = 4},
      testCase "R19 cab full never lowers the walk-ups" $
        fillCab 3 active {walkupCount = 2} @?= active {walkupCount = 2, version = 4},
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
          @?= 4,
      testCase "forward route returns on its reverse" $
        returnRouteOf "SC-MAWLAI-F" @?= Right "SC-MAWLAI-R",
      testCase "reverse route returns on its forward" $
        returnRouteOf "SC-MAWLAI-R" @?= Right "SC-MAWLAI-F",
      testCase "a route without a direction suffix has no return" $
        returnRouteOf "SC-MAWLAI" @?= Left NoReturnRoute,
      testCase "ridersByStop: waiting riders board at their stop, boarded riders alight at theirs, in route order" $
        View.ridersByStop
          routeStops
          [ row "b1" "Asha" 2 "A" "C" False,
            row "b2" "Ravi" 1 "A" "B" True,
            row "b3" "Mei" 1 "B" "C" False
          ]
          @?= [ View.RidersAtStop "Alpha" [boarding "b1" "Asha" 2 "Charlie"] [],
                View.RidersAtStop "Bravo" [boarding "b3" "Mei" 1 "Charlie"] [View.AlightingRider "b2" "Ravi" 1]
              ],
      testCase "ridersByStop: a booking holding no seat, an unknown stop and an empty cab produce nothing" $ do
        View.ridersByStop routeStops [row "b1" "Asha" 0 "A" "C" False, row "b2" "Ravi" 1 "Z" "Y" False] @?= []
        View.ridersByStop routeStops [] @?= [],
      testCase "expiry and ops ends drop the riders as the tick" $
        map endDropBy [DVT.SESSION_TIMEOUT, DVT.OPS_FORCED] @?= [Events.DroppedByTick, Events.DroppedByTick],
      testCase "a driver's end drops the riders as the driver" $
        map endDropBy [DVT.END_ROUTE, DVT.END_FOR_NOW, DVT.RETURN] @?= replicate 3 Events.DroppedByDriver
    ]

routeStops :: [(Text, Text)]
routeStops = [("A", "Alpha"), ("B", "Bravo"), ("C", "Charlie")]

row :: Text -> Text -> Int -> Text -> Text -> Bool -> View.RiderRow
row bookingId firstName seats boardStopCode dropStopCode boarded =
  View.RiderRow {bookingId, firstName, seats, boardStopCode, dropStopCode, boarded, fare = 10}

boarding :: Text -> Text -> Int -> Text -> View.BoardingRider
boarding bookingId firstName seats dropStop =
  View.BoardingRider {bookingId, firstName, seats, dropStop, fare = 10, riderStatus = View.MINUTES_AWAY, minutesAway = Nothing, expiresAt = Nothing}
