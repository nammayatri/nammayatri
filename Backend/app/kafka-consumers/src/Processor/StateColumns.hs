-- | The driver_analytics_5m state columns: one per DriverPoolData key (named
-- exactly like it). The row key carries 'stateIdOf' the state instead of the
-- columns, so a bucket holds one row per distinct state the driver was in. Shared by the DRIVER_POOL_STATE writer
-- (which keys it copies into a snapshot) and by the SESSIONIZER (which keys
-- its rows by the state at a point in time).
module Processor.StateColumns
  ( stateColumnKeys,
    renderStateValues,
    stateTag,
    stateOfTag,
    stateIdOf,
    stampVehicleChange,
    statesAt,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import qualified Database.Redis as R
import Kernel.Prelude
import Processor.ClickhouseText (epochMsText)
import Processor.DriverPoolState.Types

-- | How a column's JSON value from the current-state hash is rendered.
data StateKind
  = -- | Nullable(Bool)
    KindBool
  | -- | Nullable(String)
    KindText
  | -- | Array(String)
    KindList
  | -- | Nullable(Int64)
    KindInt
  | -- | Nullable(Float64)
    KindFloat
  | -- | Nullable(DateTime64(3, 'UTC')), sent as text (date_time_input_format = 'best_effort')
    KindTime
  | -- | Nullable(DateTime64(3, 'UTC')) from an epoch-ms number
    KindEpochMs

-- | Every DriverPoolData key except deviceToken (credential), driverId and
-- merchantOperatingCityId (the row key), and the keys not
-- wanted: driverTripEndLocation, clientDevice, schemaVersion, clientSdkVersion,
-- clientBundleVersion, clientConfigVersion, enableCashRide, safetyPlusEnabled,
-- bankAccountPaymentMode, language, gender, totalRides, acRestrictionLiftCount,
-- luggageCapacity, lastRideEndedAt and mYManufacturing.
stateColumns :: [(Text, StateKind)]
stateColumns =
  map (,KindBool) ["active", "onRide", "enabled", "blocked", "subscribed", "canSwitchToRental", "canSwitchToInterCity", "canSwitchToIntraCity", "forwardBatchingEnabled", "isSpecialLocWarrior", "isPetModeEnabled", "chargesEnabled", "hasAdvanceBooking", "isDisabledReasonFlag", "hasRideStarted", "airConditioned"]
    <> map (,KindText) ["mode", "onRideTripCategory", "goHomeStatus", "variant", "rideBillingModel", "enableForAirport", "acUsageRestrictionType", "fleetOwnerId", "cloudType", "registrationNo", "latestScheduledPickup"]
    <> map (,KindList) ["selectedServiceTiers", "selectedAutoAcceptTiers", "softBlockStiers", "vehicleTags", "driverTag"]
    <> map (,KindInt) ["tripDistanceMinThreshold", "tripDistanceMaxThreshold", "maxPickupRadius"]
    <> map (,KindEpochMs) ["lastUpdatedAt", "registrationNoChangedAt"]
    <> map (,KindFloat) ["airConditionScore", "vehicleRating"]
    <> map (,KindTime) ["latestScheduledBooking", "tollRouteBlockedTill"]

stateColumnKeys :: [Text]
stateColumnKeys = map fst stateColumns

-- | One JSONEachRow field per column, the plain value. Rendering never fails:
-- a missing key or a value of an unexpected shape becomes null ([] for lists).
renderStateValues :: A.Object -> [(AK.Key, A.Value)]
renderStateValues fields =
  [ (columnKey, renderValue kind (AKM.lookup columnKey fields))
    | (column, kind) <- stateColumns,
      let columnKey = AK.fromText column
  ]

-- | The identity of a state: its compact JSON restricted to the column keys,
-- in 'stateColumns' order, so equal states give equal tags. Rows with the same
-- tag are the same row.
stateTag :: A.Object -> Text
stateTag fields =
  "{" <> T.intercalate "," [encodeText (A.String column) <> ":" <> encodeText v | (column, _) <- stateColumns, Just v <- [AKM.lookup (AK.fromText column) fields]] <> "}"

-- | The state behind a 'stateTag' (empty for text that is not one).
stateOfTag :: Text -> A.Object
stateOfTag tagText = fromMaybe AKM.empty (A.decodeStrict (TE.encodeUtf8 tagText))

-- | driver_analytics_5m.state_id: FNV-1a 64 of the 'stateTag', the same in
-- every process for the same state. 0 is the unknown state.
stateIdOf :: Text -> Word64
stateIdOf = BS.foldl' (\h b -> (h `xor` fromIntegral b) * 1099511628211) 14695981039346656037 . TE.encodeUtf8

-- | registrationNoChangedAt is the one column not copied from DriverPoolData:
-- the writer and the sessionizer set it to the event's ts when the event
-- changes the vehicle number from a known one ("" and null both mean no
-- vehicle), so every later state carries when the vehicle last changed.
stampVehicleChange :: Integer -> A.Object -> A.Object -> A.Object
stampVehicleChange ts current changes = case (AKM.lookup "registrationNo" current, AKM.lookup "registrationNo" changes) of
  (Just old, Just new) | vehicleOf old /= vehicleOf new -> AKM.insert "registrationNoChangedAt" (A.toJSON ts) changes
  _ -> changes
  where
    vehicleOf = \case
      A.String "" -> A.Null
      v -> v

encodeText :: A.Value -> Text
encodeText = TE.decodeUtf8 . LBS.toStrict . A.encode

renderValue :: StateKind -> Maybe A.Value -> A.Value
renderValue kind mbValue = case (kind, mbValue) of
  (KindBool, Just (A.Bool b)) -> A.Bool b
  (KindBool, Just (A.String s))
    | s `elem` ["True", "true"] -> A.Bool True
    | s `elem` ["False", "false"] -> A.Bool False
  (KindText, Just A.Null) -> A.Null
  (KindText, Just v) -> A.String (asText v)
  (KindList, Just (A.Array xs)) -> A.toJSON (map asText (toList xs))
  (KindList, _) -> A.toJSON ([] :: [Text])
  (KindInt, Just v@(A.Number _)) -> case A.fromJSON v :: A.Result Int64 of
    A.Success i -> A.toJSON i
    A.Error _ -> A.Null
  (KindFloat, Just v@(A.Number _)) -> v
  (KindTime, Just v@(A.String _)) -> v
  (KindEpochMs, Just v@(A.Number _)) -> case A.fromJSON v :: A.Result Integer of
    A.Success ms -> A.String (epochMsText ms)
    A.Error _ -> A.Null
  (KindEpochMs, Just v@(A.String _)) -> v
  _ -> A.Null
  where
    asText = \case
      A.String s -> s
      other -> encodeText other

-- | For each (driverId, atMs): the newest history entry at or before atMs,
-- with its ts. One pipelined round trip per 'lookupChunk' lookups. Misses and
-- undecodable entries are absent; a failed read fails the whole lookup.
statesAt :: R.Connection -> [(Text, Integer)] -> IO (Either Text (M.Map (Text, Integer) (Integer, DriverStateSnapshot)))
statesAt conn wanted = do
  found <- forM (inChunks (S.toList (S.fromList wanted))) $ \chunk -> do
    replies <- R.runRedis conn (forM chunk (\(did, atMs) -> R.zrevrangebyscoreLimit (TE.encodeUtf8 (historyKey did)) (fromIntegral atMs) (-1 / 0) 0 1))
    pure $ do
      memberLists <- either (Left . show) Right (sequence replies)
      pure [(lookupKey, entry) | (lookupKey, members) <- zip chunk memberLists, Just entry <- [listToMaybe members >>= decodeMember]]
  pure (M.fromList . concat <$> sequence found)
  where
    inChunks xs = case splitAt lookupChunk xs of
      (chunk, []) -> [chunk | not (null chunk)]
      (chunk, rest) -> chunk : inChunks rest

lookupChunk :: Int
lookupChunk = 1000
