{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.TransporterConfig where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Text (Text, pack)
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types as DTC
import GHC.Generics (Generic)
import Kernel.Types.Common (HighPrecMeters, HighPrecMoney, Meters, Seconds)
import Lib.Yudhishthira.TypesTH (GenericDefaults (..))
import Sequelize.SQLObject (SQLObject (..), ToSQLObject (..))
import Prelude

data TdsConfig = TdsConfig {rate :: Double, thresholdAmount :: Maybe HighPrecMoney}
  deriving (Generic, Show, ToJSON, Read, Eq)

-- | Accepts the new nested @{ rate, thresholdAmount }@ object, and — for
-- backward compatibility with rows written before TdsConfig existed — a bare
-- number, read as @{ rate = n, thresholdAmount = Nothing }@. Mirrors the
-- legacy-shape handling in TollGate's FromJSON. No DB migration required.
instance FromJSON TdsConfig where
  parseJSON v = parseNested v <|> parseLegacyRate v
    where
      parseNested =
        withObject "TdsConfig" $ \o ->
          (TdsConfig <$> o .: "rate" <*> o .:? "thresholdAmount") >>= validateCfg
      parseLegacyRate =
        withScientific "TdsConfig" $ \n ->
          validateCfg TdsConfig {rate = realToFrac n, thresholdAmount = Nothing}
      validateCfg cfg@(TdsConfig r _)
        | r < 0 || r > 1 =
          fail $ "TdsConfig.rate must be a decimal fraction in [0,1] (0.001 = 0.1%), got: " <> show r
        | otherwise = pure cfg

data AppletKey = SosAppletID | RentalAppletID | FleetAppletID deriving (Show, Read, Eq, Ord, Generic)

instance Hashable AppletKey

-- Central conversion functions from AppletKey to Text and vice versa
appletKeyToString :: AppletKey -> Text
appletKeyToString = \case
  SosAppletID -> "SosAppletID"
  RentalAppletID -> "RentalAppletID"
  FleetAppletID -> "FleetAppletID"

stringToAppletKey :: Text -> Maybe AppletKey
stringToAppletKey = \case
  "SosAppletID" -> Just SosAppletID
  "RentalAppletID" -> Just RentalAppletID
  "FleetAppletID" -> Just FleetAppletID
  _ -> Nothing

instance ToJSON AppletKey where
  toJSON = String . appletKeyToString

instance FromJSON AppletKey where
  parseJSON = withText "AppletKey" $ maybe (fail "Invalid AppletKey") pure . stringToAppletKey

instance ToJSONKey AppletKey where
  toJSONKey = toJSONKeyText appletKeyToString

instance FromJSONKey AppletKey where
  fromJSONKey = FromJSONKeyText $ \t -> maybe (error "Unknown AppletKey") id (stringToAppletKey t)

data ExotelMapping = ExotelMapping
  { exotelMap :: HM.HashMap AppletKey Text
  }
  deriving (Show, Read, Eq, Ord, Generic)

fromFieldExotel ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion ExotelMapping
fromFieldExotel f mbValue = do
  value <- fromField f mbValue
  case fromJSON value of
    Success a -> pure a
    _ -> DPSF.returnError DPSF.ConversionFailed f "Conversion failed"

instance HasSqlValueSyntax be Value => HasSqlValueSyntax be ExotelMapping where
  sqlValueSyntax = sqlValueSyntax . toJSON

instance FromField ExotelMapping where
  fromField = fromFieldExotel

instance BeamSqlBackend be => B.HasSqlEqualityCheck be ExotelMapping

instance FromBackendRow Postgres ExotelMapping

instance {-# OVERLAPPING #-} ToSQLObject ExotelMapping where
  convertToSQLObject = SQLObjectValue . pack . show . encode

instance ToJSON ExotelMapping where
  toJSON = \case ExotelMapping m -> object ["exotelMap" .= m]

instance FromJSON ExotelMapping where
  parseJSON = withObject "ExotelMapping" $ \v -> ExotelMapping <$> v .: "exotelMap"

-- | Config: list of SOP type names only. Documents are stored in knowledge_center table and queried by sopType + merchantOpCityId.
newtype KnowledgeCenterSopTypesConfig = KnowledgeCenterSopTypesConfig
  { unKnowledgeCenterSopTypesConfig :: [Text]
  }
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON KnowledgeCenterSopTypesConfig where
  toJSON (KnowledgeCenterSopTypesConfig xs) = toJSON xs

instance FromJSON KnowledgeCenterSopTypesConfig where
  parseJSON v = KnowledgeCenterSopTypesConfig <$> parseJSON v

instance HasSqlValueSyntax be Value => HasSqlValueSyntax be KnowledgeCenterSopTypesConfig where
  sqlValueSyntax = sqlValueSyntax . toJSON

instance FromField KnowledgeCenterSopTypesConfig where
  fromField f mbValue = do
    value <- fromField f mbValue
    case fromJSON value of
      Success a -> pure a
      _ -> DPSF.returnError DPSF.ConversionFailed f "Conversion failed"

instance BeamSqlBackend be => B.HasSqlEqualityCheck be KnowledgeCenterSopTypesConfig

instance FromBackendRow Postgres KnowledgeCenterSopTypesConfig

-- | The unified end-ride fare recomputation policy: THE one config the
-- resolver ('Domain.Action.UI.Ride.EndRide.RecomputeDecision.mkRecomputeConfig')
-- reads. Absent fields fall back to fleet code defaults (which mirror the old
-- legacy-column DB defaults). The legacy scattered columns are no longer read
-- by any code; they remain in the DB for rollback only. Cities with custom
-- legacy values MUST be backfilled before this ships
-- (dev/sql-seed/fare-recompute-policy-backfill.sql).
--
-- Backfill mapping (policy field <- old legacy column):
--   upward.allowWithinThreshold          <- recomputeIfPickupDropNotOutsideOfThreshold
--   upward.bands                         <- recomputeDistanceThresholds
--   upward.smallOverageForgivenessMeters <- actualRideDistanceDiffThreshold (RETIRED 2026-10-15: bands gate both directions now)
--   upward.bufferMeters                  <- upwardsRecomputeBuffer
--   upward.bufferPercentage              <- upwardsRecomputeBufferPercentage
--   upward.dailyExtraKmsBudget           <- fareRecomputeDailyExtraKmsThreshold
--   upward.weeklyExtraKmsBudget          <- fareRecomputeWeeklyExtraKmsThreshold
--   downward.allowForChangedDestination  <- enableDownwardRecomputeForDifferentDestination
--   downward.forgivenessMeters           <- downwardRecomputeDistanceThreshold
--   downward.passThroughMinEstimateMeters <- minThresholdForPassThroughDestination
--   time.overageForgivenessSeconds       <- actualRideDurationDiffThreshold
--   time.gateExtraTimeOnEstimateBilled   <- gateExtraTimeChargeByRecompute
--   pinnedTripCategories                 <- noRecomputeTripCategories
--   upward.notifyDriverOnBudgetExceeded  <- toNotifyDriverForExtraKmsLimitExceed
--   recomputeCongestionOnEndRide         <- recomputeCongestionChargeOnEndRide
--   estimatedTollFallback                <- enableEstimatedTollFallback
data RecomputePolicy = RecomputePolicy
  { upward :: Maybe UpwardRecomputePolicy,
    downward :: Maybe DownwardRecomputePolicy,
    time :: Maybe TimeRecomputePolicy,
    pinnedTripCategories :: Maybe [DTC.TripCategory],
    -- | Re-run the congestion model at end ride with actual distance/duration
    -- (was recomputeCongestionChargeOnEndRide). Default off.
    recomputeCongestionOnEndRide :: Maybe Bool,
    -- | On distance-calc failure with no detected toll, charge the estimated
    -- toll (was enableEstimatedTollFallback). Default off (rider-favoring).
    estimatedTollFallback :: Maybe Bool
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- | When and how far the billed distance may move away from the estimate.
-- The band table gates BOTH directions: within the pickup/drop threshold it
-- qualifies upward recompute; outside the threshold the actual is billed only
-- when |traveled - estimate| clears the band (shortfalls included — sub-band
-- deltas keep the estimate). The former smallOverageForgivenessMeters flat
-- threshold is retired; a stored JSON key is ignored on decode.
data UpwardRecomputePolicy = UpwardRecomputePolicy
  { allowWithinThreshold :: Maybe Bool,
    bands :: Maybe [RecomputeBand],
    bufferMeters :: Maybe HighPrecMeters,
    bufferPercentage :: Maybe Int,
    dailyExtraKmsBudget :: Maybe HighPrecMeters,
    weeklyExtraKmsBudget :: Maybe HighPrecMeters,
    -- | Overlay the driver when the extra-km budget is exhausted (was
    -- toNotifyDriverForExtraKmsLimitExceed). Default on.
    notifyDriverOnBudgetExceeded :: Maybe Bool
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- | When the billed distance may shrink below the estimate.
data DownwardRecomputePolicy = DownwardRecomputePolicy
  { allowForChangedDestination :: Maybe Bool,
    forgivenessMeters :: Maybe HighPrecMeters,
    passThroughMinEstimateMeters :: Maybe Meters
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- | Time-overage billing controls.
data TimeRecomputePolicy = TimeRecomputePolicy
  { -- | Flat forgiveness window: deducted from an overrun's billed duration,
    -- clamped at the estimate — overage inside the window bills the
    -- estimated duration, overage beyond it bills only the excess.
    -- Fleet default: 300s (5 min). Ignored when 'forgivenessBands' is set.
    overageForgivenessSeconds :: Maybe Seconds,
    -- | Estimate-relative forgiveness, mirroring the distance bands: the band
    -- whose estimatedDurationUpper is the tightest fit for the ride's
    -- estimated duration supplies the window. Wins over the flat value.
    forgivenessBands :: Maybe [TimeForgivenessBand],
    gateExtraTimeOnEstimateBilled :: Maybe Bool,
    -- | Rides ending within the pickup/drop threshold never bill BELOW the
    -- estimated duration (no time refunds at the booked destination).
    -- Default ON. Binds only when perMinRateSections actually bill the
    -- chargeable minutes (the time-billing MECHANISM is not a config: a
    -- policy that configures perMinuteRideExtraTimeCharge bills overage
    -- through that charge alone — grace + forgiveness window deducted — with
    -- any sections pinned to the estimated minutes; only a policy with
    -- sections and no extra-time charge bills the sections on the
    -- chargeable/actual minutes at recompute).
    floorAtEstimateWithinThreshold :: Maybe Bool
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- | One time-forgiveness band: rides with estimated duration up to
-- estimatedDurationUpper get a forgivenessSeconds window deducted from any
-- duration overage (billing never drops below the estimate).
data TimeForgivenessBand = TimeForgivenessBand
  { estimatedDurationUpper :: Seconds,
    forgivenessSeconds :: Seconds
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- | One upward-recompute qualification band. Same JSON shape as the old
-- legacy DistanceRecomputeConfigs rows, so the backfill copies them verbatim.
data RecomputeBand = RecomputeBand
  { estimatedDistanceUpper :: Meters,
    minThresholdPercentage :: Int,
    minThresholdDistance :: Meters
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- | One node of the driver earnings fare breakup shown under "Fare paid by
-- customer". A leaf lists 'fareTags' (FareParameters charge field names) and
-- shows their sum; a group lists 'subItems' and shows the sum of its children.
-- 'key' is the translation key for the label. Shape rules (exactly one of
-- fareTags / subItems, no tag in two leaves, known tags only) are enforced by
-- the dashboard update API, not by this decoder.
data FareBreakupNode = FareBreakupNode
  { key :: Text,
    fareTags :: Maybe [Text],
    subItems :: Maybe [FareBreakupNode]
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- | Hand-written so the 'generateGenericDefault' splice over TransporterConfig
-- reuses it: deriving one would follow 'subItems' back into FareBreakupNode
-- and never terminate (compile-time stack overflow).
instance GenericDefaults FareBreakupNode where
  genDef _ = [FareBreakupNode {key = "defaultText", fareTags = Just ["baseFare"], subItems = Nothing}]
