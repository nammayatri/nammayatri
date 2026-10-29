-- | Driver-app contract: the session view types are shared with driver-app (Domain.Types.SharedCabSession) and re-exported here;
-- the rest are rider-app's own. Kept out of spec/API/SharedCabInternal.yaml: the generated module imports Servant and
-- EulerHS.Prelude, whose `route` and `force` clash with these field names.
module SharedLogic.SharedCab.SessionView (module SharedLogic.SharedCab.SessionView, module Domain.Types.SharedCabSession) where

import Control.Applicative ((<|>))
import Data.List (findIndex, sortOn)
import Data.Ord (comparing)
import Domain.Types.SharedCabSession
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)

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
    expiresAt :: Maybe UTCTime,
    chatChannelId :: Maybe Text,
    callable :: Bool,
    -- | Set while the driver has not accepted this rider: the row is an offer, not a rider to board.
    offerExpiresAt :: Maybe UTCTime,
    riderRating :: Maybe Double,
    boardStopAddress :: Maybe Text,
    dropStopAddress :: Maybe Text,
    distanceMeters :: Maybe Int
  }
  deriving (Show, Eq)

-- | Per stop, in route order: riders to board and riders getting off. A rider whose stop the route lacks goes in a trailing "Other stops" group.
groupRidersByStop :: [(Text, Text)] -> [RiderRow] -> [RidersAtStop]
groupRidersByStop stops rows =
  [group stopName (== code) | (code, stopName) <- stops, nonEmpty (group stopName (== code))]
    ++ [other | let other = group otherStopsName (`notElem` map fst stops), nonEmpty other]
  where
    held = filter (\r -> r.seats > 0 && isNothing r.offerExpiresAt) rows
    group stopName atStop =
      RidersAtStop
        { stopName,
          boarding = [BoardingRider {bookingId = r.bookingId, firstName = r.firstName, seats = r.seats, dropStop = nameOf r.dropStopCode, fare = r.fare, riderStatus = r.riderStatus, minutesAway = r.minutesAway, expiresAt = r.expiresAt, chatChannelId = r.chatChannelId, callable = r.callable, riderRating = r.riderRating, boardStopAddress = r.boardStopAddress, dropStopAddress = r.dropStopAddress, distanceMeters = r.distanceMeters} | r <- held, not r.boarded, atStop r.boardStopCode],
          alighting = [AlightingRider {bookingId = r.bookingId, firstName = r.firstName, seats = r.seats, chatChannelId = r.chatChannelId, callable = r.callable} | r <- held, r.boarded, atStop r.dropStopCode]
        }
    nonEmpty g = not (null g.boarding && null g.alighting)
    nameOf code = fromMaybe code (lookup code stops)

-- | Metres along the route from the board stop to the drop stop: the straight legs between consecutive stops added up. None when either stop is not
-- on the route or the drop does not come after the board.
alongRouteMeters :: [(Text, LatLong)] -> Text -> Text -> Maybe Int
alongRouteMeters stops boardCode dropCode = do
  boardIdx <- findIndex ((== boardCode) . fst) stops
  dropIdx <- findIndex ((== dropCode) . fst) stops
  guard (dropIdx > boardIdx)
  let leg = take (dropIdx - boardIdx + 1) (drop boardIdx stops)
  pure . round . sum $ zipWith (\(_, p) (_, q) -> realToFrac (distanceBetweenInMeters p q) :: Double) leg (drop 1 leg)

-- | Metres along the polyline to the point nearest the stop; none for a polyline with fewer than two points.
arcPosition :: [LatLong] -> LatLong -> Maybe Double
arcPosition poly p
  | length poly < 2 = Nothing
  | otherwise = Just . snd $ minimumBy (comparing fst) [candidate a b start | (a, b, start) <- zip3 poly (drop 1 poly) starts]
  where
    metersBetween a b = realToFrac (distanceBetweenInMeters a b) :: Double
    starts = scanl (+) 0 (zipWith metersBetween poly (drop 1 poly))
    candidate a b start = let (off, t) = project a b in (off, start + t * metersBetween a b)
    earthR = 6371000 :: Double
    toXY q = ((q.lon - p.lon) * cos (p.lat * pi / 180) * earthR * pi / 180, (q.lat - p.lat) * earthR * pi / 180)
    project a b =
      let (ax, ay) = toXY a
          (bx, by) = toXY b
          (dx, dy) = (bx - ax, by - ay)
          len2 = dx * dx + dy * dy
          t = if len2 == 0 then 0 else max 0 (min 1 (negate (ax * dx + ay * dy) / len2))
          (cx, cy) = (ax + t * dx, ay + t * dy)
       in (sqrt (cx * cx + cy * cy), t)

-- | The ride's length: along the route polyline between the points nearest the two stops when there is one and the drop comes after the board,
-- else the straight legs between the stops.
rideMeters :: [LatLong] -> [(Text, LatLong)] -> Text -> Text -> Maybe Int
rideMeters polyline stops boardCode dropCode = alongPolyline <|> alongRouteMeters stops boardCode dropCode
  where
    alongPolyline = do
      boardPoint <- lookup boardCode stops
      dropPoint <- lookup dropCode stops
      boardAt <- arcPosition polyline boardPoint
      dropAt <- arcPosition polyline dropPoint
      guard (dropAt > boardAt)
      pure (round (dropAt - boardAt))

-- | The rider's rating as the driver sees it: one decimal, none when unrated.
roundedRating :: Maybe Double -> Maybe Double
roundedRating = fmap (\r -> fromIntegral (round (r * 10) :: Int) / 10)

-- | A stop as the driver's map reads it.
sessionStopOf :: Text -> Text -> LatLong -> SessionStop
sessionStopOf code name point = SessionStop {code, name, lat = point.lat, lon = point.lon}

-- | The riders awaiting the driver's answer, soonest to lapse first.
pendingOffersOf :: [(Text, Text)] -> [RiderRow] -> [PendingOffer]
pendingOffersOf stops rows =
  sortOn
    (.offerExpiresAt)
    [ PendingOffer {bookingId = r.bookingId, firstName = r.firstName, seats = r.seats, fare = r.fare, boardStop = nameOf r.boardStopCode, dropStop = nameOf r.dropStopCode, minutesAway = r.minutesAway, offerExpiresAt = deadline, riderRating = r.riderRating, boardStopAddress = r.boardStopAddress, dropStopAddress = r.dropStopAddress, distanceMeters = r.distanceMeters}
      | r <- rows,
        r.seats > 0,
        Just deadline <- [r.offerExpiresAt]
    ]
  where
    nameOf code = fromMaybe code (lookup code stops)

otherStopsName :: Text
otherStopsName = "Other stops"
