-- | The shared-cab session view rider-app serves on /internal/sharedCab/* and driver-app proxies to the driver app. One
-- definition for both apps, so the two cannot drift.
module Domain.Types.SharedCabSession where

import Data.Aeson (Value (..), defaultOptions, object, withObject, withText, (.:), (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

data SessionStatus = ACTIVE | PAUSED | ENDED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data PauseReason = NO_LOCATION | DRIVER_OFFLINE | ABSENT | OFF_ROUTE
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | Wire form: { tag: 'AT_STOP', sinceMin, stopName } | { tag: 'MOVING' }.
data SessionMovement
  = SessionMovementAtStop {sinceMin :: Int, stopName :: Text}
  | SessionMovementMoving
  deriving (Show, Eq, Generic, ToSchema)

instance ToJSON SessionMovement where
  toJSON (SessionMovementAtStop sinceMin stopName) =
    object ["tag" .= ("AT_STOP" :: Text), "sinceMin" .= sinceMin, "stopName" .= stopName]
  toJSON SessionMovementMoving = object ["tag" .= ("MOVING" :: Text)]

instance FromJSON SessionMovement where
  parseJSON = withObject "SessionMovement" $ \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      "AT_STOP" -> SessionMovementAtStop <$> o .: "sinceMin" <*> o .: "stopName"
      "MOVING" -> pure SessionMovementMoving
      other -> fail $ "Unknown session movement tag: " <> show other

-- | A status a newer rider-app adds decodes as UNKNOWN_STATUS instead of failing the driver's whole session view.
data RiderStatus = AT_STOP | MINUTES_AWAY | ARRIVING | BOARDED | UNKNOWN_STATUS
  deriving (Show, Eq, Generic, ToJSON, ToSchema)

instance FromJSON RiderStatus where
  parseJSON = withText "RiderStatus" $ \t ->
    pure $ case t of
      "AT_STOP" -> AT_STOP
      "MINUTES_AWAY" -> MINUTES_AWAY
      "ARRIVING" -> ARRIVING
      "BOARDED" -> BOARDED
      _ -> UNKNOWN_STATUS

-- | A stop with its coordinates, for the driver's map.
data SessionStop = SessionStop
  { code :: Text,
    name :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data SharedCabRoute = SharedCabRoute
  { code :: Text,
    name :: Text,
    direction :: Text,
    fromStop :: Text,
    toStop :: Text,
    distanceKm :: Maybe Double,
    -- | Riders with a booking still waiting for a cab on this route.
    waitingCount :: Int,
    -- | Riders who searched for a cab at one of the route's stops in the last 15 minutes without booking.
    searchingCount :: Int,
    -- | The route's stops in order, and its encoded shape, for the pre-start map.
    stops :: [SessionStop],
    shapePolyline :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, ToSchema)

-- | A response from before the demand and map fields has none of them: no one waiting or searching, no stops.
instance FromJSON SharedCabRoute where
  parseJSON = genericParseJSON defaultOptions . withDefaults [("waitingCount", Number 0), ("searchingCount", Number 0), ("stops", Array mempty)]

-- | A rider the driver has not yet accepted or declined: the accept sheet's content.
data PendingOffer = PendingOffer
  { bookingId :: Text,
    firstName :: Text,
    seats :: Int,
    fare :: HighPrecMoney,
    boardStop :: Text,
    dropStop :: Text,
    minutesAway :: Maybe Int,
    offerExpiresAt :: UTCTime,
    riderRating :: Maybe Double,
    boardStopAddress :: Maybe Text,
    dropStopAddress :: Maybe Text,
    distanceMeters :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data SessionRoute = SessionRoute
  { code :: Text,
    name :: Text,
    direction :: Text,
    nextStops :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data BoardingRider = BoardingRider
  { bookingId :: Text,
    firstName :: Text,
    seats :: Int,
    dropStop :: Text,
    fare :: HighPrecMoney,
    riderStatus :: RiderStatus,
    minutesAway :: Maybe Int,
    expiresAt :: Maybe UTCTime,
    chatChannelId :: Maybe Text,
    callable :: Bool,
    -- | The rider's rating, one decimal; none when unrated.
    riderRating :: Maybe Double,
    boardStopAddress :: Maybe Text,
    dropStopAddress :: Maybe Text,
    -- | Metres along the route from the board stop to the drop stop; none when it cannot be worked out.
    distanceMeters :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data AlightingRider = AlightingRider
  { bookingId :: Text,
    firstName :: Text,
    seats :: Int,
    chatChannelId :: Maybe Text,
    callable :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data RidersAtStop = RidersAtStop
  { stopName :: Text,
    boarding :: [BoardingRider],
    alighting :: [AlightingRider]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data DemandAtStop = DemandAtStop
  { stopCode :: Text,
    stopName :: Text,
    waiting :: Int,
    searching :: Int,
    windowMin :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, ToSchema)

-- | A row from before the stop code has none (the app then falls back to the name).
instance FromJSON DemandAtStop where
  parseJSON = genericParseJSON defaultOptions . withDefaults [("stopCode", String "")]

data SharedCabSession = SharedCabSession
  { route :: SessionRoute,
    queuedRoute :: Maybe SessionRoute,
    status :: SessionStatus,
    pauseReason :: Maybe PauseReason,
    movement :: SessionMovement,
    capacity :: Int,
    walkupCount :: Int,
    available :: Int,
    version :: Int,
    ridersByStop :: [RidersAtStop],
    pendingOffers :: [PendingOffer],
    -- | The route's stops in order, its encoded shape and the cab's latest position, for the driver's map.
    stops :: [SessionStop],
    shapePolyline :: Maybe Text,
    vehicle :: Maybe LatLong,
    demandAhead :: [DemandAtStop]
  }
  deriving (Show, Eq, Generic, ToJSON, ToSchema)

-- | A session from before the offer and map fields has none of them: no pending offers, no stops.
instance FromJSON SharedCabSession where
  parseJSON = genericParseJSON defaultOptions . withDefaults [("pendingOffers", Array mempty), ("stops", Array mempty)]

-- | Missing keys filled in before the generic decode; a key that is present is left alone.
withDefaults :: [(Text, Value)] -> Value -> Value
withDefaults defaults = \case
  Object obj -> Object (foldr (\(key, value) -> KeyMap.insertWith (\_ old -> old) (Key.fromText key) value) obj defaults)
  other -> other

data EndRouteNext = RETURN | CHANGE | END
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
