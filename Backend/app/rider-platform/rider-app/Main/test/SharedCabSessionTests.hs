{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabSessionTests (tests) where

import "beckn-spec" BecknV2.FRFS.Enums (ServiceTierType (AC))
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import "rider-app" Domain.Action.UI.SharedCabInternal (chatMessageBody)
import qualified "beckn-spec" Domain.Types.FRFSTicketStatus as TS
import "mobility-core" Kernel.External.Maps.Types (LatLong (..))
import "mobility-core" Kernel.Types.Id (Id (..))
import "rider-app" SharedLogic.SharedCab.JourneyLeg (sessionOnTrip)
import "rider-app" SharedLogic.SharedCab.Keys (routeKey)
import "rider-app" SharedLogic.SharedCab.Session (stranded)
import "rider-app" SharedLogic.SharedCab.SessionState
import qualified "rider-app" SharedLogic.SharedCab.SessionView as View
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import "rider-app" Tools.Error (SharedCabSessionError (..))
import Prelude

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 9 25) 0

t1 :: UTCTime
t1 = addUTCTime 600 t0

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
      version = 3,
      startedAt = t0,
      vehicleTripId = "trip1"
    }

tests :: TestTree
tests =
  testGroup
    "SharedCab session state"
    [ testCase "a route-set member is dropped only when its session is gone or off the route, never on a failed read" $
        map
          (routeMember "R1")
          [SessionAt active, SessionAt active {routeCode = "R2"}, SessionAt active {status = PAUSED}, SessionAbsent, SessionUnreadable]
          @?= [LiveMember active, StaleMember, StaleMember, StaleMember, UnknownMember],
      testCase "the leg shows a cab's position and driver only on the trip the rider boarded (any trip before boarding)" $
        map
          (fmap (.driverId) . uncurry sessionOnTrip)
          [(Nothing, Just active), (Just "trip1", Just active), (Just "trip0", Just active), (Nothing, Just active {status = ENDED}), (Nothing, Nothing)]
          @?= [Just "d1", Just "d1", Nothing, Nothing, Nothing],
      testCase "a chat message is stripped and must be 1 to 500 characters" $
        map
          (either (const Nothing) (Just . T.length) . chatMessageBody)
          ["  on my way  ", T.replicate 500 "a", T.replicate 501 "a", "   "]
          @?= [Just 9, Just 500, Nothing, Nothing],
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
      testCase "route change points the session at the new trip, started now" $
        switchRoute "R2" "trip2" t1 active
          @?= active {routeCode = "R2", vehicleTripId = "trip2", startedAt = t1, version = 4},
      testCase "two cities whose feeds share a route code keep separate route sets" $
        (routeKey (Id "shillong") "R1" == routeKey (Id "tura") "R1", routeKey (Id "shillong") "R1") @?= (False, "sharedcab:route:shillong:R1"),
      testCase "route change leaves the old route set and joins the new one" $
        routeSetMoves (Just active) (switchRoute "R2" "trip2" t1 active) @?= RouteSetMoves ["R1"] ["R2"],
      testCase "a city's live-plate member stays while its session is live in that city, and goes only on a positive answer" $
        map
          (cityMember (Id "moc"))
          [SessionAt active, SessionAt active {status = PAUSED}, SessionAt (endSession active), SessionAt active {merchantOperatingCityId = Id "tura"}, SessionAbsent, SessionUnreadable]
          @?= [LiveMember active, LiveMember active {status = PAUSED}, StaleMember, StaleMember, StaleMember, UnknownMember],
      testCase "afterLastDrop queues the change and stays on the current route" $
        queueRoute "R2" active @?= active {queuedRouteCode = Just "R2", version = 4},
      testCase "applying the queued route clears it" $
        queuedRouteCode (switchRoute "R2" "trip2" t1 (queueRoute "R2" active)) @?= Nothing,
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
      testCase "pause leaves the route set, resume rejoins" $
        let paused = either (error . show) id (pauseSession NO_LOCATION active)
         in (routeSetMoves (Just active) paused, routeSetMoves (Just paused) <$> resumeSession paused)
              @?= (RouteSetMoves ["R1"] [], Right (RouteSetMoves [] ["R1"])),
      testCase "a new session on a reused plate outranks the old version" $
        version (newSession (OpenSessionReq "d2" "ml 05-a 1234" (Id "m") (Id "moc") (Id "ibc") AC 4 "R1") "trip3" t0 (Just active))
          @?= 4,
      testCase "forward route returns on its reverse" $
        returnRouteOf "SC-MAWLAI-F" @?= Right "SC-MAWLAI-R",
      testCase "reverse route returns on its forward" $
        returnRouteOf "SC-MAWLAI-R" @?= Right "SC-MAWLAI-F",
      testCase "a route without a direction suffix has no return" $
        returnRouteOf "SC-MAWLAI" @?= Left NoReturnRoute,
      testCase "riders: every held rider once, boarded or not, with both stops by name" $
        View.ridersOf
          []
          routeStops
          [ row "b1" "Asha" 2 "A" "C" False,
            row "b2" "Ravi" 1 "A" "B" True,
            row "b3" "Mei" 1 "B" "C" False
          ]
          @?= [ rider "b1" "Asha" 2 "Alpha" "Charlie",
                rider "b2" "Ravi" 1 "Alpha" "Bravo",
                rider "b3" "Mei" 1 "Bravo" "Charlie"
              ],
      testCase "riders: a booking holding no seat and an empty cab produce nothing" $ do
        View.ridersOf [] routeStops [row "b1" "Asha" 0 "A" "C" False, row "b2" "Ravi" 0 "Z" "Y" True] @?= []
        View.ridersOf [] routeStops [] @?= [],
      testCase "MED-4: a rider whose stop is off the route keeps the stop's code" $
        View.ridersOf [] routeStops [row "b2" "Ravi" 1 "A" "Y" True, row "b3" "Mei" 1 "Z" "C" False]
          @?= [rider "b2" "Ravi" 1 "Alpha" "Y", rider "b3" "Mei" 1 "Z" "Charlie"],
      testCase "riders: a rider whose drop stop the cab has passed is left out; one off the route stays" $
        map (.bookingId) (View.ridersOf ["A", "B"] routeStops [row "b1" "Asha" 1 "A" "B" True, row "b2" "Ravi" 1 "A" "C" True, row "b3" "Mei" 1 "B" "C" False, row "b4" "Lin" 1 "A" "Y" True])
          @?= ["b2", "b3", "b4"],
      testCase "real-state status: a flipped ticket beats an allocation, an allocation beats walking" $
        map (uncurry View.riderStatusOf) [(True, True), (True, False), (False, True), (False, False)]
          @?= [View.BOARDED, View.BOARDED, View.ARRIVING, View.MINUTES_AWAY],
      testCase "~5 km/h on foot, rounded up (833 m ~ 10 min, 5 km ~ 60)" $
        map View.walkMinutesAway [0, 60, 833, 5000] @?= [0, 1, 10, 60],
      testCase "a rider relays the row's ARRIVING status, walking ETA and deadline" $
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
                  callNumber = Just "+919876543210",
                  offerExpiresAt = Nothing,
                  riderRating = Just 4.5,
                  boardStopAddress = Just "1 Alpha Rd",
                  dropStopAddress = Just "2 Charlie Rd",
                  distanceMeters = Just 2500
                }
         in View.ridersOf [] routeStops [arriving]
              @?= [View.SharedCabRider "b1" "Asha" 1 "Alpha" "Charlie" 10 View.ARRIVING (Just 3) deadline (Just "b1") True (Just "+919876543210") (Just 4.5) (Just "1 Alpha Rd") (Just "2 Charlie Rd") (Just 2500) "A" "C"],
      testCase "an offered rider is not in the riders list; the driver's pending offers list it, soonest to lapse first" $
        let offered b secs = ((row b "Asha" 1 "A" "C" False) :: View.RiderRow) {View.offerExpiresAt = Just (addUTCTime secs t0)}
            rows = [offered "late" 30, row "boarding" "Ravi" 1 "A" "C" False, offered "soon" 15]
         in ( map (.bookingId) (View.ridersOf [] routeStops rows),
              map (\o -> (o.bookingId, o.boardStop, o.dropStop, o.boardStopCode, o.dropStopCode)) (View.pendingOffersOf routeStops rows)
            )
              @?= (["boarding"], [("soon", "Alpha", "Charlie", "A", "C"), ("late", "Alpha", "Charlie", "A", "C")]),
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
      testCase "a boarded rider keeps its chat channel and call flag" $
        map (\r -> (r.bookingId, r.chatChannelId, r.callable)) (View.ridersOf [] routeStops [(row "b2" "Ravi" 1 "A" "B" True) {View.chatChannelId = Just "b2", View.callable = True}])
          @?= [("b2", Just "b2", True)],
      testCase "H3: finish reports exactly the riders whose drop failed" $
        stranded [("b1", Right ()), ("b2", Left ("timeout" :: Text)), ("b3", Right ()), ("b4", Left "timeout")] @?= ["b2", "b4"]
    ]

routeStops :: [(Text, Text)]
routeStops = [("A", "Alpha"), ("B", "Bravo"), ("C", "Charlie")]

row :: Text -> Text -> Int -> Text -> Text -> Bool -> View.RiderRow
row bookingId firstName seats boardStopCode dropStopCode boarded =
  View.RiderRow {bookingId, firstName, seats, boardStopCode, dropStopCode, boarded, fare = 10, riderStatus = View.MINUTES_AWAY, minutesAway = Nothing, expiresAt = Nothing, chatChannelId = Nothing, callable = False, callNumber = Nothing, offerExpiresAt = Nothing, riderRating = Nothing, boardStopAddress = Nothing, dropStopAddress = Nothing, distanceMeters = Nothing}

rider :: Text -> Text -> Int -> Text -> Text -> View.SharedCabRider
rider bookingId firstName seats boardStop dropStop =
  View.SharedCabRider {bookingId, firstName, seats, boardStop, dropStop, fare = 10, riderStatus = View.MINUTES_AWAY, minutesAway = Nothing, expiresAt = Nothing, chatChannelId = Nothing, callable = False, callNumber = Nothing, riderRating = Nothing, boardStopAddress = Nothing, dropStopAddress = Nothing, distanceMeters = Nothing, boardStopCode = boardStop, dropStopCode = dropStop}
