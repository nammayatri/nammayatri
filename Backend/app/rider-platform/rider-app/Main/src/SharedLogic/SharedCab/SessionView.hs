-- | Driver-app contract: the session view types are shared with driver-app (Domain.Types.SharedCabSession) and re-exported here;
-- the rest are rider-app's own. Kept out of spec/API/SharedCabInternal.yaml: the generated module imports Servant and
-- EulerHS.Prelude, whose `route` and `force` clash with these field names.
module SharedLogic.SharedCab.SessionView (module SharedLogic.SharedCab.SessionView, module Domain.Types.SharedCabSession) where

import Domain.Types.SharedCabSession
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

riderStatusOf :: Bool -> Bool -> RiderStatus
riderStatusOf boarded allocated
  | boarded = BOARDED
  | allocated = ARRIVING
  | otherwise = MINUTES_AWAY

walkMinutesAway :: Double -> Int
walkMinutesAway meters = ceiling (meters / walkMetersPerMinute)

walkMetersPerMinute :: Double
walkMetersPerMinute = 5000 / 60

-- | END closes the run as END_ROUTE at the last stop, else END_FOR_NOW. RETURN / END are refused while riders are on board unless `force`.
data EndRouteReq = EndRouteReq
  { driverId :: Text,
    vehicleNumber :: Text,
    next :: EndRouteNext,
    atLastStop :: Maybe Bool,
    force :: Maybe Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data RiderRow = RiderRow
  { bookingId :: Text,
    firstName :: Text,
    seats :: Int,
    boardStopCode :: Text,
    dropStopCode :: Text,
    boarded :: Bool,
    fare :: HighPrecMoney,
    riderStatus :: RiderStatus,
    minutesAway :: Maybe Int,
    expiresAt :: Maybe UTCTime
  }
  deriving (Show, Eq)

-- | Per stop, in route order: riders to board and riders getting off. A rider whose stop the route lacks goes in a trailing "Other stops" group.
groupRidersByStop :: [(Text, Text)] -> [RiderRow] -> [RidersAtStop]
groupRidersByStop stops rows =
  [group stopName (== code) | (code, stopName) <- stops, nonEmpty (group stopName (== code))]
    ++ [other | let other = group otherStopsName (`notElem` map fst stops), nonEmpty other]
  where
    held = filter ((> 0) . (.seats)) rows
    group stopName atStop =
      RidersAtStop
        { stopName,
          boarding = [BoardingRider {bookingId = r.bookingId, firstName = r.firstName, seats = r.seats, dropStop = nameOf r.dropStopCode, fare = r.fare, riderStatus = r.riderStatus, minutesAway = r.minutesAway, expiresAt = r.expiresAt} | r <- held, not r.boarded, atStop r.boardStopCode],
          alighting = [AlightingRider {bookingId = r.bookingId, firstName = r.firstName, seats = r.seats} | r <- held, r.boarded, atStop r.dropStopCode]
        }
    nonEmpty g = not (null g.boarding && null g.alighting)
    nameOf code = fromMaybe code (lookup code stops)

otherStopsName :: Text
otherStopsName = "Other stops"
