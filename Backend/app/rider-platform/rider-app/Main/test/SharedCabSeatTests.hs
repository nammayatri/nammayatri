{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | The shared-cab seat count (`SharedLogic.SharedCab.Seats`): seats free over a ride from the live bookings on a cab.
module SharedCabSeatTests (tests) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import "beckn-spec" Domain.Types.FRFSTicketBookingStatus (FRFSTicketBookingStatus (..))
import qualified "beckn-spec" Domain.Types.FRFSTicketStatus as Ticket
import "mobility-core" Kernel.External.Maps.Types (LatLong (..))
import qualified "rider-app" SharedLogic.External.LocationTrackingService.Types as LT
import "rider-app" SharedLogic.SharedCab.Booking (candidateIds, stillOn)
import "rider-app" SharedLogic.SharedCab.Seats
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

-- | S0 .. S4 on one route, nobody on board.
cab :: Int -> CabSeats
cab capacity = CabSeats {capacity, walkups = 0, stopCodes = ["S0", "S1", "S2", "S3", "S4"], riders = []}

rideOf :: CabSeats -> Text -> Text -> (Int, Int)
rideOf c board dropStop = maybe (error "route doesn't serve the ride") id (bookingRange c board dropStop)

aboard :: Text -> Int -> Text -> Text -> CabSeats -> CabSeats
aboard bookingId seats board dropStop c = c {riders = Rider {bookingId, seats, ride = rideOf c board dropStop} : c.riders}

-- | What the tick does for one booking, one cab at a time under its plate lock: the first cab the party fits on takes it.
claimOn :: [(Text, CabSeats)] -> Text -> Text -> Text -> Int -> (Maybe Text, [(Text, CabSeats)])
claimOn [] _ _ _ _ = (Nothing, [])
claimOn ((plate, c) : cs) bookingId board dropStop party
  | fits c bookingId board dropStop party = (Just plate, (plate, aboard bookingId party board dropStop c) : cs)
  | otherwise = let (won, rest) = claimOn cs bookingId board dropStop party in (won, (plate, c) : rest)

-- | A plate's read as `plateBookings` and `seatedOn` make it: the DB's ids and the index's, each re-read from KV, the KV row deciding.
readPlate :: Text -> [Text] -> [Text] -> Map.Map Text Seated -> [Text]
readPlate plate fromDb indexed kv =
  map (.bookingId) . ridersOn (cab 4).stopCodes plate "t1" $
    [r | bookingId <- candidateIds fromDb indexed, Just r <- [Map.lookup bookingId kv], stillOn [plate] [CONFIRMED] r.plate r.status]

seated :: Text -> FRFSTicketBookingStatus -> Maybe Text -> [Ticket.FRFSTicketStatus] -> Seated
seated bookingId status plate tickets =
  Seated {bookingId, status, plate, boardedTrip = Nothing, boardStop = "S1", dropStop = "S3", tickets, party = 1}

tests :: TestTree
tests =
  testGroup
    "SharedCab seat count"
    [ testCase "two bookings race for a cab's last seat in one tick: the first checks it under the plate lock, the second goes to the next cab" $
        let cabs = [("A", (cab 2) {walkups = 1}), ("B", cab 2)]
            (first, afterFirst) = claimOn cabs "b1" "S1" "S3" 1
            (second, _) = claimOn afterFirst "b2" "S1" "S3" 1
         in (first, second) @?= (Just "A", Just "B"),
      testCase "a party needs all its seats in one cab" $
        let c = aboard "b1" 3 "S0" "S4" (cab 4)
         in (fits c "b2" "S1" "S2" 2, fits c "b2" "S1" "S2" 1, freeOver c (rideOf c "S1" "S2")) @?= (False, True, 1),
      testCase "a seat freed by a drop at stop k is sold to a rider boarding at k, not to one boarding before it" $
        let c = aboard "b1" 1 "S0" "S2" (cab 1)
         in (fits c "b2" "S1" "S3" 1, fits c "b2" "S2" "S4" 1) @?= (False, True),
      testCase "riders who never share a stop share a seat: the count is the busiest stop of the ride, not the sum" $
        let c = aboard "b2" 1 "S2" "S4" (aboard "b1" 1 "S0" "S2" (cab 2))
         in (freeOver c (0, 4), freeOver c (1, 3)) @?= (1, 1),
      testCase "every rider covering a stop counts at it" $
        let c = aboard "b3" 1 "S1" "S2" (aboard "b2" 2 "S1" "S3" (aboard "b1" 1 "S0" "S4" (cab 6)))
         in map (freeOver c) [(0, 1), (1, 2), (2, 3), (3, 4), (0, 4)] @?= [5, 2, 3, 5, 2],
      testCase "walk-ups take a seat on every stop" $
        let c = (aboard "b1" 1 "S0" "S2" (cab 4)) {walkups = 2}
         in (freeOver c (0, 2), freeOver c (2, 4)) @?= (1, 2),
      testCase "an overbooked cab reads as full, never below none" $
        freeOver ((aboard "b1" 3 "S0" "S4" (cab 2)) {walkups = 1}) (0, 4) @?= 0,
      testCase "a cab is overbooked only when some stop carries more than its capacity" $
        ( overbooked ((aboard "b1" 2 "S0" "S2" (cab 4)) {walkups = 2}),
          overbooked ((aboard "b1" 2 "S0" "S2" (cab 4)) {walkups = 3}),
          overbooked (aboard "b2" 2 "S2" "S4" (aboard "b1" 2 "S0" "S2" (cab 2)))
        )
          @?= (False, True, False),
      testCase "a booking already counted on the cab is checked beside the others, not beside itself" $
        let c = aboard "b1" 2 "S0" "S4" (cab 2)
         in (fits c "b1" "S0" "S4" 2, fits c "b2" "S0" "S4" 1) @?= (True, False),
      testCase "a ride the route doesn't serve, or serves backwards, never fits" $
        (fits (cab 4) "b1" "S1" "X9" 1, fits (cab 4) "b1" "S3" "S1" 1) @?= (False, False),
      testCase "a booking counts on its plate while allocated or aboard, and stops at a drop, a cancel or a release" $
        map
          (\(status, mbPlate, statuses) -> occupies "P1" status mbPlate statuses)
          [ (CONFIRMED, Just "P1", [Ticket.ACTIVE, Ticket.ACTIVE]),
            (CONFIRMED, Just "P1", [Ticket.INPROGRESS]),
            (CONFIRMED, Just "P1", [Ticket.INPROGRESS, Ticket.ACTIVE]),
            (CONFIRMED, Just "P1", [Ticket.USED]),
            (CONFIRMED, Just "P1", [Ticket.USED, Ticket.ACTIVE]),
            (CONFIRMED, Nothing, [Ticket.ACTIVE]),
            (CONFIRMED, Just "P2", [Ticket.ACTIVE]),
            (CANCELLED, Just "P1", [Ticket.ACTIVE]),
            (CONFIRMED, Just "P1", [Ticket.CANCELLED]),
            (CONFIRMED, Just "P1", [])
          ]
          @?= [True, True, True, False, False, False, False, False, False, False],
      testCase "an assignment the DB hasn't drained yet counts: the plate's index has it, and its KV row is on the plate" $
        readPlate "P1" ["b1"] ["b1", "b2"] (Map.fromList [("b1", seated "b1" CONFIRMED (Just "P1") [Ticket.ACTIVE]), ("b2", seated "b2" CONFIRMED (Just "P1") [Ticket.ACTIVE])])
          @?= ["b1", "b2"],
      testCase "a stale index member doesn't count: dropped, cancelled, released or moved to another plate since" $
        readPlate
          "P1"
          []
          ["dropped", "cancelled", "released", "moved", "live"]
          ( Map.fromList
              [ ("dropped", seated "dropped" CONFIRMED (Just "P1") [Ticket.USED]),
                ("cancelled", seated "cancelled" CANCELLED (Just "P1") [Ticket.CANCELLED]),
                ("released", seated "released" CONFIRMED Nothing [Ticket.ACTIVE]),
                ("moved", seated "moved" CONFIRMED (Just "P2") [Ticket.INPROGRESS]),
                ("live", seated "live" CONFIRMED (Just "P1") [Ticket.INPROGRESS])
              ]
          )
          @?= ["live"],
      testCase "a DB row the KV row has since moved off the plate doesn't count, and a booking on both lists counts once" $
        ( readPlate "P1" ["b1"] [] (Map.fromList [("b1", seated "b1" CONFIRMED Nothing [Ticket.ACTIVE])]),
          candidateIds ["b1", "b2"] ["b2", "b3"]
        )
          @?= ([], ["b1", "b2", "b3"]),
      testCase "every plate read keeps a row only while its KV row is still on one of the plates, in one of the statuses" $
        [ stillOn ["P1", "P2"] [CONFIRMED] plate status
          | (plate, status) <- [(Just "P1", CONFIRMED), (Just "P2", CONFIRMED), (Just "P3", CONFIRMED), (Nothing, CONFIRMED), (Just "P1", CANCELLED)]
        ]
          @?= [True, True, False, False, False],
      testCase "a rider rides [board, drop) on the current trip; one carried over from an earlier trip, or off the route, takes the whole route" $
        let stops = (cab 4).stopCodes
         in ( rideOn stops "t2" Nothing "S1" "S3",
              rideOn stops "t2" (Just "t2") "S1" "S3",
              rideOn stops "t2" (Just "t1") "S1" "S3",
              rideOn stops "t2" Nothing "S1" "X9"
            )
              @?= ((1, 3), (1, 3), (0, 4), (0, 4)),
      testCase "raising walk-ups takes only seats free to the route's end: an app rider's seat never becomes a walk-up" $
        walkupsFitting (aboard "b1" 1 "S2" "S4" (cab 4)) 1 4 @?= 3,
      testCase "walk-ups ride from the cab's next stop: a rider who got down before it leaves their seat to them" $
        walkupsFitting (aboard "b1" 2 "S0" "S2" (cab 4)) 2 4 @?= 4,
      testCase "lowering walk-ups always lands, even on an overbooked cab, and never below none" $
        let over = (aboard "b1" 4 "S0" "S4" (cab 4)) {walkups = 2}
         in (walkupsFitting over 0 1, walkupsFitting over 0 3, walkupsFitting (cab 4) 0 (-1)) @?= (1, 2, 0),
      testCase "a ride is [board, drop) on the route; one the route doesn't serve, or serves backwards, is none" $
        let c = cab 4
         in (bookingRange c "S1" "S3", bookingRange c "S3" "S1", bookingRange c "S1" "S1", bookingRange c "S1" "X9", routeEnd c)
              @?= (Just (1, 3), Nothing, Nothing, Nothing, 4),
      testCase "the cab's next stop is its first upcoming one; without a fix, the route's start" $
        let c = cab 4
            at stops = Just (vehicle [LT.UpcomingStop (LT.Stop code nowhere code 0 0 0) t0 status Nothing | (code, status) <- stops])
         in (nextStopIdx c (at [("S0", LT.Reached), ("S1", LT.Reached), ("S2", LT.Upcoming), ("S3", LT.Upcoming)]), nextStopIdx c Nothing, nextStopIdx c (at []))
              @?= (2, 0, 0),
      testCase "a rider sees the seats free for their [board, drop), else from board to the route's end, and the seats taken at the cab's next stop" $
        let c = aboard "b2" 1 "S3" "S4" (aboard "b1" 2 "S0" "S2" (cab 4))
            atS1 = Just (vehicle [LT.UpcomingStop (LT.Stop "S1" nowhere "S1" 0 0 0) t0 LT.Upcoming Nothing])
            load board dropStop = riderLoad c board dropStop atS1
         in map (uncurry load) [(Just "S0", Just "S2"), (Just "S2", Just "S3"), (Just "S2", Nothing), (Nothing, Nothing), (Just "S3", Just "S1")]
              @?= [(2, 2), (4, 2), (3, 2), (2, 2), (3, 2)]
    ]
  where
    t0 = UTCTime (fromGregorian 2026 10 27) 36000
    nowhere = LatLong 0 0
    vehicle stops =
      LT.VehicleInfo
        { startTime = Nothing,
          scheduleRelationship = Nothing,
          tripId = Nothing,
          latitude = 0,
          longitude = 0,
          speed = Nothing,
          timestamp = Nothing,
          upcomingStops = Just stops
        }
