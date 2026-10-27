{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | The shared-cab seat rules (`SharedLogic.SharedCab.Seats`) run against an in-memory model of the FRFS seat engine: per trip and
-- seat, the stop segments held, with the Lua hold's all-or-nothing check-and-set.
module SharedCabSeatTests (tests) where

import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import qualified "rider-app" Domain.Types.Seat as Seat
import "mobility-core" Kernel.External.Maps.Types (LatLong (..))
import "mobility-core" Kernel.Types.Id (Id (..))
import qualified "rider-app" SharedLogic.External.LocationTrackingService.Types as LT
import "rider-app" SharedLogic.FRFSSeatBooking (SeatHoldMeta (..))
import "rider-app" SharedLogic.SharedCab.Allocation.Reallocate (CloseStep (..), closeSteps)
import "rider-app" SharedLogic.SharedCab.Allocation.Types (Blame (..))
import "rider-app" SharedLogic.SharedCab.Seats
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Prelude

type Held = Map.Map (Text, Text) (Set.Set Int)

segments :: Int -> Int -> Set.Set Int
segments fromIdx toIdx = Set.fromList [fromIdx .. toIdx - 1]

freeIn :: Held -> Text -> Int -> Int -> Id Seat.Seat -> Bool
freeIn held tripId fromIdx toIdx seat = Set.disjoint (Map.findWithDefault Set.empty (tripId, seat.getId) held) (segments fromIdx toIdx)

modelEngine :: IORef Held -> SeatEngine IO
modelEngine ref =
  SeatEngine
    { freeSeatIds = \tripId fromIdx toIdx seats -> (\held -> filter (freeIn held tripId fromIdx toIdx) seats) <$> readIORef ref,
      holdSeats = \tripId seats fromIdx toIdx -> atomicModifyIORef' ref $ \held ->
        if all (freeIn held tripId fromIdx toIdx) seats
          then (foldr (\seat -> Map.insertWith Set.union (tripId, seat.getId) (segments fromIdx toIdx)) held seats, True)
          else (held, False),
      releaseSeats = \tripId seats fromIdx toIdx ->
        modifyIORef' ref $ \held -> foldr (\seat -> Map.adjust (`Set.difference` segments fromIdx toIdx) (tripId, seat.getId)) held seats
    }

-- | The engine as a booking saw it before another claim landed: availability from a stale snapshot, holds against the live store.
staleReads :: Held -> SeatEngine IO -> SeatEngine IO
staleReads snapshot engine = engine {freeSeatIds = \tripId fromIdx toIdx seats -> pure (filter (freeIn snapshot tripId fromIdx toIdx) seats)}

-- | S0 .. S4 on one route.
cab :: Text -> Int -> CabSeats
cab tripId capacity = CabSeats {tripId, capacity, stopCodes = ["S0", "S1", "S2", "S3", "S4"]}

rideOf :: CabSeats -> Text -> Text -> (Int, Int)
rideOf c board dropStop = maybe (error "route doesn't serve the ride") id (bookingRange c board dropStop)

-- | A hold as the tests compare it (SeatHoldMeta has no Eq).
shape :: SeatHoldMeta -> ([Text], Int, Int)
shape h = (h.seatIds, h.fromIdx, h.toIdx)

-- | What the tick does for one booking: the first candidate cab whose seats it can hold.
claimOn :: SeatEngine IO -> [CabSeats] -> Text -> Text -> Int -> IO (Maybe Text)
claimOn _ [] _ _ _ = pure Nothing
claimOn engine (c : cs) board dropStop party =
  holdRange engine c (rideOf c board dropStop) party >>= \case
    Just held -> pure (Just held.tripId)
    Nothing -> claimOn engine cs board dropStop party

withEngine :: (SeatEngine IO -> IORef Held -> IO a) -> IO a
withEngine k = do
  ref <- newIORef Map.empty
  k (modelEngine ref) ref

tests :: TestTree
tests =
  testGroup
    "SharedCab seats on the FRFS seat engine"
    [ testCase "two bookings race for a cab's last seat in one tick: the first holds it, the second goes to the next cab" $
        withEngine $ \engine _ -> do
          let a = cab "tripA" 2
              b = cab "tripB" 2
          _ <- holdRange engine a (0, routeEnd a) 1 -- a walk-up already aboard
          first <- claimOn engine [a, b] "S1" "S3" 1
          second <- claimOn engine [a, b] "S1" "S3" 1
          (first, second) @?= (Just "tripA", Just "tripB"),
      testCase "a claim that read the cab's seats before the other claim landed still can't take the seat it took" $
        withEngine $ \engine ref -> do
          let a = cab "tripA" 2
          _ <- holdRange engine a (0, routeEnd a) 1
          before <- readIORef ref
          won <- holdRange engine a (rideOf a "S1" "S3") 1
          lost <- holdRange (staleReads before engine) a (rideOf a "S1" "S3") 1
          free <- freeOver engine a (rideOf a "S1" "S3")
          (isJust won, shape <$> lost, free) @?= (True, Nothing, 0),
      testCase "a party needs all its seats in one cab, or none are held" $
        withEngine $ \engine _ -> do
          let a = cab "tripA" 4
          _ <- holdRange engine a (rideOf a "S0" "S4") 3
          party <- holdRange engine a (rideOf a "S1" "S2") 2
          free <- freeOver engine a (rideOf a "S1" "S2")
          (shape <$> party, free) @?= (Nothing, 1),
      testCase "a seat freed by a drop at stop k is sold to a rider boarding at k, not to one boarding before it" $
        withEngine $ \engine _ -> do
          let a = cab "tripA" 1
          dropsAtS2 <- holdRange engine a (rideOf a "S0" "S2") 1
          boardsAtS1 <- holdRange engine a (rideOf a "S1" "S3") 1
          boardsAtS2 <- holdRange engine a (rideOf a "S2" "S4") 1
          (isJust dropsAtS2, shape <$> boardsAtS1, (.seatIds) <$> boardsAtS2) @?= (True, Nothing, (.seatIds) <$> dropsAtS2),
      testCase "the rider's last no-show cancels the booking and gives its seats back, so the next rider gets the seat" $
        withEngine $ \engine _ -> do
          let a = cab "tripA" 1
              steps = closeSteps 2 1 BlameRider
          Just noShow <- holdRange engine a (rideOf a "S1" "S3") 1
          when (GiveBackSeats `elem` steps) $ releaseWith engine noShow
          next <- holdRange engine a (rideOf a "S1" "S3") 1
          (steps, (.seatIds) <$> next) @?= ([GiveBackSeats, CancelForNoShows], Just noShow.seatIds),
      testCase "every other close gives the seats back too and finds the booking a cab again" $
        (closeSteps 2 0 BlameRider, closeSteps 2 5 BlameNone, closeSteps 2 5 BlameDriver)
          @?= ([GiveBackSeats, FindAgain], [GiveBackSeats, FindAgain], [GiveBackSeats, FindAgain]),
      testCase "releasing a ride gives every stop of it back" $
        withEngine $ \engine _ -> do
          let a = cab "tripA" 1
          Just held <- holdRange engine a (rideOf a "S0" "S4") 1
          releaseWith engine held
          freeOver engine a (0, routeEnd a) >>= (@?= 1),
      testCase "trips don't share seats" $
        withEngine $ \engine _ -> do
          let a = cab "tripA" 1
          _ <- holdRange engine a (0, routeEnd a) 1
          freeOver engine (cab "tripB" 1) (0, routeEnd a) >>= (@?= 1),
      testCase "raising walk-ups takes only seats free to the route's end: an app rider's seat never becomes a walk-up" $
        withEngine $ \engine _ -> do
          let a = cab "tripA" 4
          _ <- holdRange engine a (rideOf a "S2" "S4") 1 -- an app rider boarding later
          walkups <- adjustWalkups engine a 1 4 []
          (length walkups, all ((== 1) . length . (.seatIds)) walkups, map (\h -> (h.fromIdx, h.toIdx)) walkups) @?= (3, True, replicate 3 (1, 4)),
      testCase "lowering walk-ups gives back the latest ones, and their seats are free again" $
        withEngine $ \engine _ -> do
          let a = cab "tripA" 4
          walkups <- adjustWalkups engine a 0 3 []
          kept <- adjustWalkups engine a 0 1 walkups
          free <- freeOver engine a (0, routeEnd a)
          (map shape kept, free) @?= (map shape (take 1 walkups), 3),
      testCase "nothing to hold is no hold" $
        withEngine $ \engine _ -> holdRange engine (cab "tripA" 4) (0, 2) 0 >>= (@?= Nothing) . fmap shape,
      testCase "a ride is held for [board, drop) on the route; one the route doesn't serve, or serves backwards, is none" $
        let c = cab "t" 4
         in (bookingRange c "S1" "S3", bookingRange c "S3" "S1", bookingRange c "S1" "S1", bookingRange c "S1" "X9", routeEnd c)
              @?= (Just (1, 3), Nothing, Nothing, Nothing, 4),
      testCase "virtual seats are the cab's capacity; the trip keys them in the engine" $
        map (.getId) (virtualSeats (cab "t1" 3)) @?= ["1", "2", "3"],
      testCase "the cab's next stop is its first upcoming one; without a fix, the route's start" $
        let c = cab "t" 4
            at stops = Just (vehicle [LT.UpcomingStop (LT.Stop code nowhere code 0 0 0) t0 status Nothing | (code, status) <- stops])
         in (nextStopIdx c (at [("S0", LT.Reached), ("S1", LT.Reached), ("S2", LT.Upcoming), ("S3", LT.Upcoming)]), nextStopIdx c Nothing, nextStopIdx c (at []))
              @?= (2, 0, 0),
      testCase "the model engine refuses a hold that overlaps by one stop" $
        withEngine $ \engine _ -> do
          let a = cab "tripA" 1
          _ <- holdRange engine a (0, 2) 1
          overlap <- holdRange engine a (1, 2) 1
          assertBool "an overlapping hold must lose" (isNothing overlap)
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
