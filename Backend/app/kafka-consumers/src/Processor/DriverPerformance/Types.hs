-- | DRIVER_PERFORMANCE: the CDC rows it reads from the drainers' topics
-- (decoded leniently), the contributions it emits, the per-entity state it
-- keeps in the writer's Redis, and that state's keys and TTLs.
module Processor.DriverPerformance.Types
  ( -- * CDC rows
    SrdRow (..),
    RideRow (..),
    DriverBookingRow (..),
    CancellationRow (..),
    FareParamsRow (..),
    RatingRow (..),
    CdcMsg (..),
    decodeCdc,
    sentinelDriverId,

    -- * Contributions
    RowKey,
    Stamp,
    StampedKey,
    Measures (..),
    Contribution,
    moneyColumns,

    -- * State
    SrdState (..),
    DriverBookingFact (..),
    FareParamsFact (..),
    RideFacts (..),
    RideState (..),
    Inflight (..),

    -- * Redis keys
    srdKey,
    rideKey,
    bookingRideKey,
    fareParamsRideKey,
    pendingDriverBookingKey,
    pendingCancelByRideKey,
    pendingCancelByBookingKey,
    pendingFareParamsKey,
    pendingRatingsKey,
    watermarkKey,
    inflightKey,

    -- * TTLs (seconds)
    srdTtl,
    rideTtl,
    pendingTtl,
    longPendingTtl,
    fareParamsTtl,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Kernel.Prelude

------------------------------------------------------------
-- CDC rows
------------------------------------------------------------

-- Money fields (Money ints and HighPrecMoney numbers alike) are held in
-- ten-thousandths, the scale of the Decimal(18, 4) columns, so sums and
-- retractions are exact.

data SrdRow = SrdRow
  { srdId :: Text,
    srdDriverId :: Text,
    srdCityId :: Text,
    srdCreatedAt :: Integer,
    srdResponse :: Maybe Text,
    srdRenderedAt :: Maybe Integer,
    srdRespondedAt :: Maybe Integer,
    srdAutoAccepted :: Maybe Bool
  }

-- | Also the ride's stored state: every message is the full row.
data RideRow = RideRow
  { rideId :: Text,
    rideBookingId :: Text,
    rideDriverId :: Text,
    rideCityId :: Text,
    rideStatus :: Text,
    -- | NEW/UPCOMING 0 < INPROGRESS 1 < COMPLETED/CANCELLED 2
    rideRank :: Int,
    rideCreatedAt :: Integer,
    rideFare :: Maybe Integer
  }
  deriving (Generic, FromJSON, ToJSON)

data DriverBookingRow = DriverBookingRow
  { dbkId :: Text,
    dbkAutoAccepted :: Bool,
    dbkFareParamsId :: Text
  }

data CancellationRow = CancellationRow
  { bcrBookingId :: Text,
    bcrRideId :: Maybe Text,
    bcrSource :: Text
  }

data FareParamsRow = FareParamsRow
  { fpId :: Text,
    fpDriverSelectedFare :: Maybe Integer,
    fpCustomerExtraFee :: Maybe Integer
  }

data RatingRow = RatingRow
  { ratingId :: Text,
    ratingRideId :: Text,
    ratingValue :: Integer
  }

data CdcMsg
  = MsgSrd SrdRow
  | MsgRide RideRow
  | MsgDriverBooking DriverBookingRow
  | MsgCancellation CancellationRow
  | MsgFareParams FareParamsRow
  | MsgRating RatingRow

-- | SRD rows the allocator writes as a "drivers exhausted" marker.
sentinelDriverId :: Text
sentinelDriverId = "drivers-not-found"

srdTopic, rideTopic, driverBookingTopic, cancellationTopic, fareParamsTopic, ratingTopic :: Text
srdTopic = "adob-sessionizer-searchrequestfordriver"
rideTopic = "adob-sessionizer-ride"
driverBookingTopic = "adob-sessionizer-booking"
cancellationTopic = "adob-sessionizer-bookingcancellationreason"
fareParamsTopic = "adob-sessionizer-fareparameters"
ratingTopic = "adob-sessionizer-rating"

-- | One drainer message: a flat JSON object holding the full row. Right
-- Nothing for a row with nothing to count: no primary key (the drainer's @{}@
-- for an update without a model). Left for anything undecodable.
decodeCdc :: Text -> ByteString -> Either Text (Maybe CdcMsg)
decodeCdc topic raw = case A.decodeStrict raw of
  Just (A.Object o)
    | isNothing (textOf (if topic == cancellationTopic then "booking_id" else "id") o) -> Right Nothing
    | otherwise -> Just <$> decodeRow topic o
  _ -> Left "not a JSON object"

decodeRow :: Text -> A.Object -> Either Text CdcMsg
decodeRow topic o
  | topic == srdTopic = MsgSrd <$> srdRow o
  | topic == rideTopic = MsgRide <$> rideRow o
  | topic == driverBookingTopic = MsgDriverBooking <$> driverBookingRow o
  | topic == cancellationTopic = MsgCancellation <$> cancellationRow o
  | topic == fareParamsTopic = MsgFareParams <$> fareParamsRow o
  | topic == ratingTopic = MsgRating <$> ratingRow o
  | otherwise = Left ("unexpected topic " <> topic)

srdRow :: A.Object -> Either Text SrdRow
srdRow o =
  SrdRow
    <$> need "id" (textOf "id" o)
    <*> need "driver_id" (textOf "driver_id" o)
    <*> pure (cityOf o)
    <*> need "created_at" (integerOf "created_at" o)
    <*> pure (textOf "response" o)
    <*> pure (integerOf "rendered_at" o)
    <*> pure (integerOf "responded_at" o)
    <*> pure (boolOf "is_auto_accepted" o)

rideRow :: A.Object -> Either Text RideRow
rideRow o = do
  statusText <- need "status" (textOf "status" o)
  statusRank <- maybe (Left ("unknown ride status " <> statusText)) Right (rideStatusRank statusText)
  RideRow
    <$> need "id" (textOf "id" o)
    <*> need "booking_id" (textOf "booking_id" o)
    <*> need "driver_id" (textOf "driver_id" o)
    <*> pure (cityOf o)
    <*> pure statusText
    <*> pure statusRank
    <*> need "created_at" (integerOf "created_at" o)
    <*> pure (moneyOf "fare" o)

-- | updated_at is no version (1-second ties, duplicates), the status is.
rideStatusRank :: Text -> Maybe Int
rideStatusRank s
  | s == "NEW" || s == "UPCOMING" = Just 0
  | s == "INPROGRESS" = Just 1
  | s == "COMPLETED" || s == "CANCELLED" = Just 2
  | otherwise = Nothing

driverBookingRow :: A.Object -> Either Text DriverBookingRow
driverBookingRow o =
  DriverBookingRow
    <$> need "id" (textOf "id" o)
    <*> pure (boolOf "is_auto_accepted" o == Just True)
    <*> need "fare_parameters_id" (textOf "fare_parameters_id" o)

cancellationRow :: A.Object -> Either Text CancellationRow
cancellationRow o =
  CancellationRow
    <$> need "booking_id" (textOf "booking_id" o)
    <*> pure (textOf "ride_id" o)
    <*> need "source" (textOf "source" o)

fareParamsRow :: A.Object -> Either Text FareParamsRow
fareParamsRow o =
  FareParamsRow
    <$> need "id" (textOf "id" o)
    <*> pure (moneyOf "driver_selected_fare" o)
    <*> pure (moneyOf "customer_extra_fee" o)

ratingRow :: A.Object -> Either Text RatingRow
ratingRow o =
  RatingRow
    <$> need "id" (textOf "id" o)
    <*> need "ride_id" (textOf "ride_id" o)
    <*> need "rating_value" (integerOf "rating_value" o)

need :: Text -> Maybe a -> Either Text a
need column = maybe (Left ("missing or invalid " <> column)) Right

-- | Missing and null alike.
fieldOf :: Text -> A.Object -> Maybe A.Value
fieldOf k o = case AKM.lookup (AK.fromText k) o of
  Just A.Null -> Nothing
  found -> found

textOf :: Text -> A.Object -> Maybe Text
textOf k o = case fieldOf k o of
  Just (A.String s) -> Just s
  _ -> Nothing

cityOf :: A.Object -> Text
cityOf = fromMaybe "" . textOf "merchant_operating_city_id"

-- | Epoch seconds and plain ints.
integerOf :: Text -> A.Object -> Maybe Integer
integerOf k o = round <$> (doubleOf =<< fieldOf k o)

moneyOf :: Text -> A.Object -> Maybe Integer
moneyOf k o = (\d -> round (d * 10000)) <$> (doubleOf =<< fieldOf k o)

-- | Bools arrive as the strings "True"/"False".
boolOf :: Text -> A.Object -> Maybe Bool
boolOf k o = case fieldOf k o of
  Just (A.Bool b) -> Just b
  Just (A.String s)
    | T.toLower s == "true" -> Just True
    | T.toLower s == "false" -> Just False
  _ -> Nothing

-- | aeson's Double parser takes integers and exponent forms (5.0e-2) alike.
doubleOf :: A.Value -> Maybe Double
doubleOf v = case v of
  A.Number _ -> case A.fromJSON v of
    A.Success d -> keepFinite d
    A.Error _ -> Nothing
  A.String s -> keepFinite =<< readMaybe (T.unpack s)
  _ -> Nothing
  where
    keepFinite d = if isNaN d || isInfinite d then Nothing else Just d

------------------------------------------------------------
-- Contributions
------------------------------------------------------------

-- | (merchant_operating_city_id or "", 5-minute bucket start in epoch seconds, driver_id)
type RowKey = (Text, Integer, Text)

-- | The driver's state at an entity's event time: (ts of the writer's history
-- entry, the state's 'Processor.StateColumns.stateTag').
type Stamp = (Integer, Text)

-- | The full row key: the state columns are part of it (Nothing: no state known).
type StampedKey = (RowKey, Maybe Stamp)

-- | Column values of one contribution or delta. Money columns are in
-- ten-thousandths.
newtype Measures = Measures
  { msColumns :: M.Map Text Integer
  }
  deriving (Generic, FromJSON, ToJSON)

-- | What an entity adds to driver_analytics_5m; Nothing when it adds nothing.
type Contribution = Maybe (StampedKey, Measures)

-- | Decimal(18, 4) columns; every other column is an Int64 count.
moneyColumns :: [Text]
moneyColumns = ["driver_addition", "customer_tip", "gross_earnings"]

------------------------------------------------------------
-- State
------------------------------------------------------------

-- An entity's stamp is looked up once, when its state is created (Nothing:
-- looked up, no state known), and kept for every later version, so a
-- retraction always lands in the row it was counted in.

data SrdState = SrdState
  { -- | 0 unanswered, 1 answered (Accept/Reject/Pulled)
    srdStateRank :: Int,
    srdStateStamp :: Maybe Stamp,
    srdStateContribution :: Contribution
  }
  deriving (Generic, FromJSON, ToJSON)

data DriverBookingFact = DriverBookingFact
  { dbfAutoAccepted :: Bool,
    -- | the quote's fare parameters: driver addition and tip
    dbfFareParamsId :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

data FareParamsFact = FareParamsFact
  { fpfDriverSelectedFare :: Maybe Integer,
    fpfCustomerExtraFee :: Maybe Integer
  }
  deriving (Generic, FromJSON, ToJSON)

-- | Facts joined to a ride; they stay with the ride for its whole TTL.
data RideFacts = RideFacts
  { factDriverBooking :: Maybe DriverBookingFact,
    factCancelSource :: Maybe Text,
    -- | fare_parameters id -> fact, for the quote's id
    factFareParams :: M.Map Text FareParamsFact,
    -- | rating id -> rating_value
    factRatings :: M.Map Text Integer
  }
  deriving (Generic, FromJSON, ToJSON)

data RideState = RideState
  { rsRow :: RideRow,
    -- | the stamp of rsRow's driver; looked up again when the ride moves to another driver
    rsStamp :: Maybe Stamp,
    rsFacts :: RideFacts,
    rsContribution :: Contribution
  }
  deriving (Generic, FromJSON, ToJSON)

-- | The offsets of a flush that has not committed yet, recorded before its
-- insert so a restart can replay exactly that window with the same token.
data Inflight = Inflight
  { inflightToken :: Text,
    -- | (topic, partition, first offset, last offset)
    inflightOffsets :: [(Text, Int, Int64, Int64)]
  }
  deriving (Generic, FromJSON, ToJSON)

------------------------------------------------------------
-- Redis keys (the writer's Redis, never the app or LTS Redis)
------------------------------------------------------------

srdKey, rideKey, bookingRideKey, fareParamsRideKey, pendingDriverBookingKey, pendingCancelByRideKey, pendingCancelByBookingKey, pendingFareParamsKey, pendingRatingsKey :: Text -> Text
srdKey = ("perf:srd:" <>)
rideKey = ("perf:ride:" <>)
-- booking id -> ride id
bookingRideKey = ("perf:bk2ride:" <>)
-- fare_parameters id (the quote's) -> ride id
fareParamsRideKey = ("perf:fp2ride:" <>)
-- Facts that arrived before their ride, under their own join key.
pendingDriverBookingKey = ("perf:dbk:" <>)
pendingCancelByRideKey = ("perf:bcr:" <>)
pendingCancelByBookingKey = ("perf:bcrb:" <>)
pendingFareParamsKey = ("perf:fp:" <>)
pendingRatingsKey = ("perf:rating:" <>)

-- | Epoch ms of the last successful flush.
watermarkKey :: Text
watermarkKey = "perf:watermark"

inflightKey :: Text
inflightKey = "perf:flush_inflight"

srdTtl, rideTtl, pendingTtl, longPendingTtl, fareParamsTtl :: Integer
srdTtl = 2 * 3600
-- Also the join indexes. A rating later than this is dropped.
rideTtl = 8 * 86400
pendingTtl = 6 * 3600
-- Ratings (median 22 h after the ride) wait as long as a ride lives.
longPendingTtl = 8 * 86400
fareParamsTtl = 86400
