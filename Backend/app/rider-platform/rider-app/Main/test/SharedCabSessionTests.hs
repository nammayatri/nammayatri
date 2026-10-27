{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabSessionTests (tests) where

import "beckn-spec" BecknV2.FRFS.Enums (ServiceTierType (AC))
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Text (Text)
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import qualified "beckn-spec" Domain.Types.FRFSTicketStatus as TS
import qualified "rider-app" Domain.Types.VehicleTrip as DVT
import "mobility-core" Kernel.External.Maps.Types (LatLong (..))
import "mobility-core" Kernel.Types.Id (Id (..))
import qualified "rider-app" SharedLogic.SharedCab.Events as Events
import "rider-app" SharedLogic.SharedCab.Session (endDropBy, stranded)
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
    [ testCase "a cab-full fill counts no offline boardings, a driver count does" $ do
        let full = fillCab 3 active
        walkupsToCount CabFullFill active full @?= 0
        walkupsToCount DriverCounted active full @?= 3,
      testCase "the driver's count never goes negative" $
        walkupsToCount DriverCounted active {walkupCount = 3} active @?= 0,
      testCase "INPROGRESS on an ENDED session is stranded" $
        shouldDropOnDeadSession (Just ENDED) False [TS.INPROGRESS] @?= True,
      testCase "INPROGRESS with no session and no live trip is stranded; a live trip means a flush, not an end" $ do
        shouldDropOnDeadSession Nothing False [TS.INPROGRESS] @?= True
        shouldDropOnDeadSession Nothing True [TS.INPROGRESS] @?= False,
      testCase "a live session keeps its riders" $ do
        shouldDropOnDeadSession (Just ACTIVE) False [TS.INPROGRESS] @?= False
        shouldDropOnDeadSession (Just PAUSED) False [TS.INPROGRESS] @?= False,
      testCase "an unboarded or already dropped ticket is left alone" $ do
        shouldDropOnDeadSession (Just ENDED) False [TS.ACTIVE] @?= False
        shouldDropOnDeadSession (Just ENDED) False [TS.USED] @?= False,
      testCase "second driver on the same plate is rejected" $
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
      testCase "cab full: the walk-ups are the seats they now hold" $
        fillCab 3 active {walkupCount = 1} @?= active {walkupCount = 3, version = 4},
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
        View.groupRidersByStop
          routeStops
          [ row "b1" "Asha" 2 "A" "C" False,
            row "b2" "Ravi" 1 "A" "B" True,
            row "b3" "Mei" 1 "B" "C" False
          ]
          @?= [ View.RidersAtStop "Alpha" [boarding "b1" "Asha" 2 "Charlie"] [],
                View.RidersAtStop "Bravo" [boarding "b3" "Mei" 1 "Charlie"] [View.AlightingRider "b2" "Ravi" 1 Nothing False]
              ],
      testCase "ridersByStop: a booking holding no seat and an empty cab produce nothing" $ do
        View.groupRidersByStop routeStops [row "b1" "Asha" 0 "A" "C" False, row "b2" "Ravi" 0 "Z" "Y" True] @?= []
        View.groupRidersByStop routeStops [] @?= [],
      testCase "MED-4: a rider whose stop is off the route shows in a trailing Other stops group" $
        View.groupRidersByStop
          routeStops
          [ row "b1" "Asha" 1 "A" "C" False,
            row "b2" "Ravi" 1 "A" "Y" True,
            row "b3" "Mei" 1 "Z" "C" False,
            row "b4" "Lu" 1 "B" "C" True
          ]
          @?= [ View.RidersAtStop "Alpha" [boarding "b1" "Asha" 1 "Charlie"] [],
                View.RidersAtStop "Charlie" [] [View.AlightingRider "b4" "Lu" 1 Nothing False],
                View.RidersAtStop "Other stops" [boarding "b3" "Mei" 1 "Charlie"] [View.AlightingRider "b2" "Ravi" 1 Nothing False]
              ],
      testCase "real-state status: a flipped ticket beats an allocation, an allocation beats walking" $
        map (uncurry View.riderStatusOf) [(True, True), (True, False), (False, True), (False, False)]
          @?= [View.BOARDED, View.BOARDED, View.ARRIVING, View.MINUTES_AWAY],
      testCase "~5 km/h on foot, rounded up (833 m ~ 10 min, 5 km ~ 60)" $
        map View.walkMinutesAway [0, 60, 833, 5000] @?= [0, 1, 10, 60],
      testCase "the group relays the row's ARRIVING status, walking ETA and deadline" $
        let deadline = Just (addUTCTime 90 t0)
            arriving =
              View.RiderRow
                { bookingId = "b1",
                  firstName = "Asha",
                  seats = 1,
                  boardStopCode = "A",
                  dropStopCode = "C",
                  boarded = False,
                  fare = 10,
                  riderStatus = View.ARRIVING,
                  minutesAway = Just 3,
                  expiresAt = deadline,
                  chatChannelId = Just "b1",
                  callable = True,
                  offerExpiresAt = Nothing,
                  riderRating = Just 4.5,
                  boardStopAddress = Just "1 Alpha Rd",
                  dropStopAddress = Just "2 Charlie Rd",
                  distanceMeters = Just 2500
                }
         in View.groupRidersByStop routeStops [arriving]
              @?= [View.RidersAtStop "Alpha" [View.BoardingRider "b1" "Asha" 1 "Charlie" 10 View.ARRIVING (Just 3) deadline (Just "b1") True (Just 4.5) (Just "1 Alpha Rd") (Just "2 Charlie Rd") (Just 2500)] []],
      testCase "an offered rider is not in the boarding list; the driver's pending offers list it, soonest to lapse first" $
        let offered b secs = ((row b "Asha" 1 "A" "C" False) :: View.RiderRow) {View.offerExpiresAt = Just (addUTCTime secs t0)}
            rows = [offered "late" 30, row "boarding" "Ravi" 1 "A" "C" False, offered "soon" 15]
         in ( map (.bookingId) . concatMap (.boarding) $ View.groupRidersByStop routeStops rows,
              map (\o -> (o.bookingId, o.boardStop, o.dropStop)) (View.pendingOffersOf routeStops rows)
            )
              @?= (["boarding"], [("soon", "Alpha", "Charlie"), ("late", "Alpha", "Charlie")]),
      testCase "a session and a route list entry from before the offer, demand and map fields still decode, with empty defaults" $
        let session = "{\"route\":{\"code\":\"R1\",\"name\":\"Route\",\"direction\":\"Far\",\"nextStops\":[]},\"queuedRoute\":null,\"status\":\"ACTIVE\",\"pauseReason\":null,\"movement\":{\"tag\":\"MOVING\"},\"capacity\":4,\"walkupCount\":0,\"available\":4,\"version\":1,\"ridersByStop\":[],\"demandAhead\":[],\"lowDemandCard\":null,\"offRoute\":null}"
            routeEntry = "{\"code\":\"R1\",\"name\":\"Route\",\"direction\":\"Far\",\"fromStop\":\"Near\",\"toStop\":\"Far\",\"distanceKm\":null,\"isStandRoute\":false}"
         in ( (\s -> (s.pendingOffers, s.stops, s.shapePolyline, s.vehicle)) <$> (decode session :: Maybe View.SharedCabSession),
              (\r -> (r.waitingCount, r.searchingCount, r.stops, r.shapePolyline)) <$> (decode routeEntry :: Maybe View.SharedCabRoute)
            )
              @?= (Just ([], [], Nothing, Nothing), Just (0, 0, [], Nothing)),
      testCase "a stop ships as code, name, lat and lon, and nothing else" $
        BLC.unpack (encode (View.sessionStopOf "SC_POLO" "Polo" (LatLong 25.5 91.8)))
          @?= "{\"code\":\"SC_POLO\",\"lat\":25.5,\"lon\":91.8,\"name\":\"Polo\"}",
      testCase "the ride distance is the legs between the board and drop stops added up; none off the route or backwards" $
        let line = [("A", LatLong 25.0000 91.0000), ("B", LatLong 25.0100 91.0000), ("C", LatLong 25.0200 91.0000)]
            near expected = fmap (\m -> abs (m - expected) <= 10) -- 0.01 degree of latitude is about 1112 m
         in ( near 2224 (View.alongRouteMeters line "A" "C"),
              near 1112 (View.alongRouteMeters line "B" "C"),
              near 1112 (View.alongRouteMeters line "A" "B"),
              View.alongRouteMeters line "C" "A",
              View.alongRouteMeters line "A" "A",
              View.alongRouteMeters line "A" "Z",
              View.alongRouteMeters line "Z" "C"
            )
              @?= (Just True, Just True, Just True, Nothing, Nothing, Nothing, Nothing),
      testCase "the ride distance follows the route polyline when there is one, and falls back to the straight legs" $
        let stops = [("A", LatLong 25.0 91.0), ("C", LatLong 25.01 91.01)]
            bend = [LatLong 25.0 91.0, LatLong 25.0 91.01, LatLong 25.01 91.01] -- east 0.01 degree, then north 0.01 degree: about 1008 + 1112 m
            backwards = reverse bend
            near expected = fmap (\m -> abs (m - expected) <= 25)
         in ( near 2120 (View.rideMeters bend stops "A" "C"),
              near 1503 (View.rideMeters [] stops "A" "C"),
              near 1503 (View.rideMeters [LatLong 25.0 91.0] stops "A" "C"),
              near 1503 (View.rideMeters backwards stops "A" "C"),
              View.rideMeters bend stops "C" "A"
            )
              @?= (Just True, Just True, Just True, Just True, Nothing),
      testCase "a pending offer carries both stops' addresses and the ride distance" $
        let withInfo = ((row "b9" "Mei" 1 "A" "C" False) :: View.RiderRow) {View.offerExpiresAt = Just (addUTCTime 20 t0), View.boardStopAddress = Just "1 Alpha Rd", View.dropStopAddress = Just "2 Charlie Rd", View.distanceMeters = Just 2500}
         in map (\o -> (o.boardStopAddress, o.dropStopAddress, o.distanceMeters)) (View.pendingOffersOf routeStops [withInfo])
              @?= [(Just "1 Alpha Rd", Just "2 Charlie Rd", Just 2500)],
      testCase "the rider's rating is shown to one decimal, and not at all when unrated" $
        map View.roundedRating [Nothing, Just 4.56, Just 4.0, Just 4.44, Just 0.06, Just 5]
          @?= [Nothing, Just 4.6, Just 4.0, Just 4.4, Just 0.1, Just 5.0],
      testCase "an alighting rider keeps its chat channel and call flag" $
        View.groupRidersByStop routeStops [(row "b2" "Ravi" 1 "A" "B" True) {View.chatChannelId = Just "b2", View.callable = True}]
          @?= [View.RidersAtStop "Bravo" [] [View.AlightingRider "b2" "Ravi" 1 (Just "b2") True]],
      testCase "H3: finish reports exactly the riders whose drop failed" $
        stranded [("b1", Right ()), ("b2", Left ("timeout" :: Text)), ("b3", Right ()), ("b4", Left "timeout")] @?= ["b2", "b4"],
      testCase "expiry and ops ends drop the riders as the tick" $
        map endDropBy [DVT.SESSION_TIMEOUT, DVT.OPS_FORCED] @?= [Events.DroppedByTick, Events.DroppedByTick],
      testCase "a driver's end drops the riders as the driver" $
        map endDropBy [DVT.END_ROUTE, DVT.END_FOR_NOW, DVT.RETURN] @?= replicate 3 Events.DroppedByDriver
    ]

routeStops :: [(Text, Text)]
routeStops = [("A", "Alpha"), ("B", "Bravo"), ("C", "Charlie")]

row :: Text -> Text -> Int -> Text -> Text -> Bool -> View.RiderRow
row bookingId firstName seats boardStopCode dropStopCode boarded =
  View.RiderRow {bookingId, firstName, seats, boardStopCode, dropStopCode, boarded, fare = 10, riderStatus = View.MINUTES_AWAY, minutesAway = Nothing, expiresAt = Nothing, chatChannelId = Nothing, callable = False, offerExpiresAt = Nothing, riderRating = Nothing, boardStopAddress = Nothing, dropStopAddress = Nothing, distanceMeters = Nothing}

boarding :: Text -> Text -> Int -> Text -> View.BoardingRider
boarding bookingId firstName seats dropStop =
  View.BoardingRider {bookingId, firstName, seats, dropStop, fare = 10, riderStatus = View.MINUTES_AWAY, minutesAway = Nothing, expiresAt = Nothing, chatChannelId = Nothing, callable = False, riderRating = Nothing, boardStopAddress = Nothing, dropStopAddress = Nothing, distanceMeters = Nothing}
