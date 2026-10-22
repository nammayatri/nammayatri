-- | DRIVER_PERFORMANCE: what one entity contributes to driver_analytics_5m
-- (performance_analytics semantics), and the delta between two versions of
-- that contribution. Pure.
module Processor.DriverPerformance.Contribution
  ( srdRank,
    srdContribution,
    rideContribution,
    diffContributions,
    addMeasures,
    nullMeasures,
  )
where

import qualified Data.Map.Strict as M
import Kernel.Prelude
import Processor.DriverPerformance.Types

bucketSecs :: Integer
bucketSecs = 5 * 60

floorBucket :: Integer -> Integer
floorBucket t = t - t `mod` bucketSecs

countIf :: Bool -> Integer
countIf c = if c then 1 else 0

-- | Zero columns dropped; Nothing when nothing is left.
contribution :: StampedKey -> [(Text, Integer)] -> Contribution
contribution rk cols =
  let ms = Measures {msColumns = M.filter (/= 0) (M.fromListWith (+) cols)}
   in if nullMeasures ms then Nothing else Just (rk, ms)

nullMeasures :: Measures -> Bool
nullMeasures ms = M.null ms.msColumns

addMeasures :: Measures -> Measures -> Measures
addMeasures x y = Measures {msColumns = M.filter (/= 0) (M.unionWith (+) x.msColumns y.msColumns)}

negateMeasures :: Measures -> Measures
negateMeasures ms = Measures {msColumns = negate <$> ms.msColumns}

-- | What turns the old contribution into the new one: one delta when the row
-- key is unchanged, else a retraction at the old key and the new one at its own.
-- Replaying a version emits a zero delta.
diffContributions :: Contribution -> Contribution -> [(StampedKey, Measures)]
diffContributions old new = case (old, new) of
  (Just (oldKey, oldMs), Just (newKey, newMs))
    | oldKey == newKey -> [(newKey, addMeasures newMs (negateMeasures oldMs))]
  _ -> maybe [] (\(oldKey, oldMs) -> [(oldKey, negateMeasures oldMs)]) old <> maybeToList new

-- | 0 unanswered, 1 answered. A response only ever goes from null to a value,
-- so a lower rank is a stale redelivery.
srdRank :: SrdRow -> Int
srdRank r = if isJust r.srdResponse then 1 else 0

-- | Pulled pop-ups count nowhere, like PA (which filters them out).
srdContribution :: Maybe Stamp -> SrdRow -> Contribution
srdContribution stamp r
  | r.srdResponse == Just "Pulled" = Nothing
  | otherwise =
    contribution
      ((r.srdCityId, floorBucket r.srdCreatedAt, r.srdDriverId), stamp)
      [ ("popups_sent", 1),
        ("popups_accepted", countIf accepted),
        ("popups_rejected", countIf (r.srdResponse == Just "Reject")),
        ("popups_neglected", countIf (isNothing r.srdResponse)),
        ("response_secs_sum", fromMaybe 0 responseSecs),
        ("auto_accept_search_requests", countIf (accepted && r.srdAutoAccepted == Just True))
      ]
  where
    accepted = r.srdResponse == Just "Accept"
    responseSecs = case (r.srdRenderedAt, r.srdRespondedAt) of
      (Just rendered, Just responded) | rendered > 0 && responded > rendered -> Just (responded - rendered)
      _ -> Nothing

-- | Bucketed by the ride's created_at (PA uses the search time).
rideContribution :: RideState -> Contribution
rideContribution st =
  contribution
    ((ride.rideCityId, floorBucket ride.rideCreatedAt, ride.rideDriverId), st.rsStamp)
    [ ("total_bookings", 1),
      ("finished_rides", countIf completed),
      ("inprogress_rides", countIf (ride.rideStatus == "INPROGRESS")),
      ("cancelled_rides", countIf cancelled),
      ("rides_cancelled_by_driver", countIf byDriver),
      ("rides_cancelled_by_user", countIf byUser),
      ("auto_accept_bookings", countIf autoAccepted),
      ("auto_accept_finished_rides", countIf (autoAccepted && completed)),
      ("auto_accept_cancelled_rides", countIf (autoAccepted && cancelled)),
      ("auto_accept_rides_cancelled_by_driver", countIf (autoAccepted && byDriver)),
      ("auto_accept_rides_cancelled_by_user", countIf (autoAccepted && byUser)),
      ("rides_with_rating", toInteger (M.size facts.factRatings)),
      ("rating_sum", sum (M.elems facts.factRatings)),
      -- The quote's fare parameters (booking.fare_parameters_id), for any status, like PA.
      ("driver_addition", fromMaybe 0 (quoteFareParams >>= (.fpfDriverSelectedFare))),
      ("customer_tip", fromMaybe 0 (quoteFareParams >>= (.fpfCustomerExtraFee))),
      ("gross_earnings", if completed then fromMaybe 0 ride.rideFare else 0)
    ]
  where
    ride = st.rsRow
    facts = st.rsFacts
    completed = ride.rideStatus == "COMPLETED"
    cancelled = ride.rideStatus == "CANCELLED"
    byDriver = cancelled && facts.factCancelSource == Just "ByDriver"
    byUser = cancelled && facts.factCancelSource == Just "ByUser"
    autoAccepted = maybe False (.dbfAutoAccepted) facts.factDriverBooking
    fareParamsOf fid = M.lookup fid facts.factFareParams
    quoteFareParams = facts.factDriverBooking >>= fareParamsOf . (.dbfFareParamsId)
