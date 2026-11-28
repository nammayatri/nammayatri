-- | Pure state transitions for rider-started open meter rides. Domain.Action.UI.OpenMeter
-- runs each one under a Redis lock and writes its result with a single query call.
module SharedLogic.OpenMeter
  ( StartDecision (..),
    isValidRideId,
    decideStart,
    isStale,
    negativeHeartbeatField,
    negativeEndField,
    applyHeartbeat,
    applyEnd,
    applyCancel,
    applyAutoEnd,
    serverLinearDistance,
    isDistanceMismatch,
  )
where

import qualified API.Types.UI.OpenMeter as API
import Control.Applicative ((<|>))
import Data.List (partition)
import qualified Data.Text as T
import Data.Time (diffUTCTime)
import qualified Data.UUID as UUID
import Domain.Types.OpenMeterRide (OpenMeterRide (..), OpenMeterRideStatus (..))
import qualified Domain.Types.Person as DP
import qualified Kernel.External.Maps.Google.PolyLinePoints as PolyLinePoints
import Kernel.Prelude
import Kernel.Types.Common (Meters (..), highPrecMetersToMeters)
import Kernel.Types.Id (Id (..))
import Kernel.Utils.CalculateDistance (getRouteLinearLength)
import Tools.Error (OpenMeterError (..))

data StartDecision
  = ReturnExisting OpenMeterRide
  | -- | Holds the caller's stale RUNNING rides, which must be AUTO_ENDED.
    Create [Id OpenMeterRide]
  | RejectNotFound
  | RejectActive (Id OpenMeterRide)
  deriving (Eq, Show)

-- | The client generates the ride id (a UUID v4) and it becomes the primary key, a
-- varchar(36) column. Start accepts nothing else: KV would take a longer id, but the
-- drainer's insert into Postgres would fail and the ride would be lost when KV expires it.
-- Only the canonical lowercase spelling is taken (both natives write it), so one ride can
-- never be stored or looked up under two spellings of the same UUID.
isValidRideId :: Id OpenMeterRide -> Bool
isValidRideId (Id rideId) = maybe False ((== rideId) . UUID.toText) (UUID.fromText rideId)

-- | Arguments: caller, allowMultiple, now, maxIdleHours, the ride already stored under the
-- requested id, and the caller's RUNNING rides. A known id is idempotent for its owner and
-- not found for anyone else.
decideStart :: Id DP.Person -> Bool -> UTCTime -> Int -> Maybe OpenMeterRide -> [OpenMeterRide] -> StartDecision
decideStart caller allowMultiple now maxIdleHours mbExisting running =
  case mbExisting of
    Just existing
      | existing.personId == caller -> ReturnExisting existing
      | otherwise -> RejectNotFound
    Nothing -> case active of
      (activeRide : _) | not allowMultiple -> RejectActive activeRide.id
      _ -> Create (map (.id) stale)
  where
    (stale, active) = partition (isStale now maxIdleHours) $ filter ((== RUNNING) . (.status)) running

-- | A RUNNING ride the server has not written for over maxIdleHours: no heartbeat applied, or
-- no start if it never got one. updatedAt is the server's clock, while a heartbeat's sampledAt
-- is the phone's, so a phone clock running behind cannot make a live ride look stale. A
-- maxIdleHours below 1 counts as 1, so a misconfigured value cannot end live rides on start.
isStale :: UTCTime -> Int -> OpenMeterRide -> Bool
isStale now maxIdleHours ride =
  ride.status == RUNNING && diffUTCTime now ride.updatedAt > fromIntegral (max 1 maxIdleHours * 3600)

-- | The first total of a heartbeat that is negative, if any. Such a heartbeat is refused.
negativeHeartbeatField :: API.OpenMeterRideHeartbeatReq -> Maybe Text
negativeHeartbeatField req =
  firstNegative
    [ ("distanceMeters", req.distanceMeters.getMeters < 0),
      ("waitingTimeSeconds", req.waitingTimeSeconds.getSeconds < 0),
      ("fare", maybe False (< 0) req.fare)
    ]

-- | The first total of an END that is negative, if any. Such an END is refused.
negativeEndField :: API.OpenMeterRideEndReq -> Maybe Text
negativeEndField req =
  firstNegative
    [ ("distanceMeters", req.distanceMeters.getMeters < 0),
      ("waitingTimeSeconds", req.waitingTimeSeconds.getSeconds < 0),
      ("fare", maybe False (< 0) req.fare),
      ("pointsCount", req.pointsCount < 0),
      ("estimatedGapMeters", req.estimatedGapMeters.getMeters < 0)
    ]

firstNegative :: [(Text, Bool)] -> Maybe Text
firstNegative = fmap fst . find snd

-- | Heartbeats carry absolute totals and only move a RUNNING ride forward. One that is not
-- newer than the stored heartbeat is a no-op. sampledAt comes from the phone's clock, so it is
-- capped at the server's now: a clock running ahead must not block later heartbeats or keep
-- the ride from going stale.
applyHeartbeat :: UTCTime -> API.OpenMeterRideHeartbeatReq -> OpenMeterRide -> Either OpenMeterError (Maybe OpenMeterRide)
applyHeartbeat now req ride
  | ride.status /= RUNNING = Left (invalidState ride)
  | maybe False (sampledAt <=) ride.lastHeartbeatAt = Right Nothing
  | otherwise =
    Right . Just $
      ride
        { distanceMeters = req.distanceMeters,
          waitingTimeSeconds = req.waitingTimeSeconds,
          lastLatLong = req.lastLatLong <|> ride.lastLatLong,
          fare = req.fare <|> ride.fare,
          lastHeartbeatAt = Just sampledAt,
          updatedAt = now
        }
  where
    sampledAt = min now req.sampledAt

-- | The Meters argument is the server's linear distance along the uploaded polyline. Ending an
-- ENDED ride is a no-op. An AUTO_ENDED ride takes the client's data: the server only stopped
-- it for want of heartbeats, while the phone kept measuring. An end time before the start (the
-- phone's clock moved back during the ride) is stored as the start time. Refusing it would
-- leave the ride RUNNING with no way to close it.
applyEnd :: UTCTime -> API.OpenMeterRideEndReq -> Meters -> OpenMeterRide -> Either OpenMeterError (Maybe OpenMeterRide)
applyEnd now req serverLinear ride = case ride.status of
  ENDED -> Right Nothing
  CANCELLED -> Left (invalidState ride)
  RUNNING -> Right (Just ended)
  AUTO_ENDED -> Right (Just ended)
  where
    ended =
      ride
        { status = ENDED,
          endTime = Just (max ride.startTime req.endTime),
          endLatLong = req.endLatLong,
          distanceMeters = req.distanceMeters,
          serverLinearDistanceMeters = Just serverLinear,
          waitingTimeSeconds = req.waitingTimeSeconds,
          fare = req.fare <|> ride.fare,
          currency = req.currency <|> ride.currency,
          encodedPolyline = Just req.encodedPolyline,
          pointsCount = req.pointsCount,
          estimatedGapMeters = req.estimatedGapMeters,
          endReason = Just req.endReason,
          updatedAt = now
        }

-- | Cancelling a CANCELLED ride is a no-op; an ENDED ride cannot be cancelled.
applyCancel :: UTCTime -> OpenMeterRide -> Either OpenMeterError (Maybe OpenMeterRide)
applyCancel now ride = case ride.status of
  CANCELLED -> Right Nothing
  ENDED -> Left (invalidState ride)
  RUNNING -> Right (Just cancelled)
  AUTO_ENDED -> Right (Just cancelled)
  where
    cancelled = ride {status = CANCELLED, updatedAt = now}

-- | The server closes a RUNNING ride that no phone will end: one gone stale on start, or one
-- whose session logout or login has ended. It ends where the server last heard of it, at the last
-- heartbeat, or at the server's start time if none came, and keeps the heartbeat's totals. A later
-- END from the phone still replaces them (applyEnd accepts AUTO_ENDED). Any other status is left as
-- it is.
applyAutoEnd :: UTCTime -> OpenMeterRide -> Maybe OpenMeterRide
applyAutoEnd now ride = case ride.status of
  RUNNING -> Just ride {status = AUTO_ENDED, endTime = ride.lastHeartbeatAt <|> Just ride.serverStartedAt, updatedAt = now}
  _ -> Nothing

invalidState :: OpenMeterRide -> OpenMeterError
invalidState ride = OpenMeterRideInvalidState (show ride.status)

-- | Haversine length of an encoded polyline (precision 5). Nothing when the text is not a
-- well-formed polyline: Kernel's decoder takes the 'head' of each chunk's terminator and
-- crashes on a truncated string, so every character must be in '?'..'~' and the last one must
-- end a chunk (below '_'). Each value ends with such a character, and values come in latitude
-- and longitude pairs: the decoder silently drops an unpaired one, so an odd count is refused.
serverLinearDistance :: Text -> Maybe Meters
serverLinearDistance polyline
  | wellFormed = Just . highPrecMetersToMeters $ getRouteLinearLength (PolyLinePoints.decode polyline) Nothing
  | otherwise = Nothing
  where
    wellFormed =
      T.all (\c -> c >= '?' && c <= '~') polyline
        && maybe True ((< '_') . snd) (T.unsnoc polyline)
        && even (T.length (T.filter (< '_') polyline))

-- | True when the client's distance is more than 30% off the server's linear distance. A
-- zero server distance (empty or single-point route) is never reported.
isDistanceMismatch :: Meters -> Meters -> Bool
isDistanceMismatch (Meters client) (Meters server) =
  server > 0 && fromIntegral (abs (client - server)) / fromIntegral server > (0.3 :: Double)
