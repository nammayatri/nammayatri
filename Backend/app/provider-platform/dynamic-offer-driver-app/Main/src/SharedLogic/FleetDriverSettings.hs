-- | A fleet's auto-assign and preference settings, applied to one of its drivers in a single place: when a vehicle is
-- assigned to the driver, and when a driver who already has a vehicle becomes active in the fleet.
--
-- Only the tiers that driver's vehicle serves get auto-assign, and nothing is removed when the driver leaves the fleet.
module SharedLogic.FleetDriverSettings
  ( autoAssignTagName,
    vehicleAutoAcceptTiers,
    autoAssignedTiers,
    applyFleetSettings,
    applyFleetSettingsToDriver,
    removeAutoAssign,
  )
where

import qualified Data.Aeson as A
import qualified Data.List as DL
import qualified Data.Vector as V
import Domain.Types.Common (ServiceTierType)
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person (Person)
import qualified Domain.Types.Vehicle as DV
import qualified Domain.Types.VehicleServiceTier as DVST
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.DriverPool.AreaPreference as AreaPref
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error

autoAssignTagName :: Text
autoAssignTagName = "AutoAssign"

-- | The city's auto-accept tiers this vehicle serves.
vehicleAutoAcceptTiers :: [DVST.VehicleServiceTier] -> DV.Vehicle -> [ServiceTierType]
vehicleAutoAcceptTiers cityTiers vehicle =
  [ tier.serviceTierType
    | tier <- cityTiers,
      maybe False (.enabled) tier.autoAcceptanceConfig,
      tier.serviceTierType `elem` vehicle.selectedServiceTiers
  ]

-- | Tiers on the driver's AutoAssign tag.
autoAssignedTiers :: [LYT.TagNameValueExpiry] -> [Text]
autoAssignedTiers tags = case Yudhishthira.accessTagKey (LYT.TagName autoAssignTagName) (Yudhishthira.convertTags tags) of
  Just (A.String value) -> [value]
  Just (A.Array values) -> [value | A.String value <- V.toList values]
  _ -> []

-- | Applies the driver's fleet settings to the driver and this vehicle, one after another on the same tags: auto-assign
-- on the vehicle's auto-accept tiers when the fleet has it on by default (or 'forceAutoAssign', the fleet's per-driver
-- switch), then area preference off and trip-distance preference off when the fleet ignores them. No-op for a driver
-- whose fleet has no auto-assign settings, unless forced.
applyFleetSettings ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Bool ->
  [DVST.VehicleServiceTier] ->
  Person ->
  DV.Vehicle ->
  m ()
applyFleetSettings forceAutoAssign cityTiers driver vehicle = do
  mbFleetAutoAssign <- QFDA.getDriverFleetAutoAssignConfig driver.id driver.merchantOperatingCityId
  when (forceAutoAssign || isJust mbFleetAutoAssign) $ do
    let tiers = if forceAutoAssign || maybe False (.defaultOn) mbFleetAutoAssign then vehicleAutoAcceptTiers cityTiers vehicle else []
        ignorePreferences = maybe False (.disableDriverAreaAndDistancePreference) mbFleetAutoAssign
        oldTags = fromMaybe [] driver.driverTag
        newTags =
          oldTags
            & addAutoAssign tiers
            & disablePreferenceIf ignorePreferences AreaPref.areaPreferenceEnabledTagName
            & disablePreferenceIf ignorePreferences AreaPref.distancePreferenceEnabledTagName
    when (newTags /= oldTags) $ QPerson.updateDriverTag (Just newTags) driver.id
    updateAutoAcceptTiers vehicle (DL.nub (fromMaybe [] vehicle.selectedAutoAcceptTiers <> tiers))
  where
    addAutoAssign [] tags = tags
    addAutoAssign tiers tags = setTag autoAssignTagName (LYT.ArrayValue (DL.nub (autoAssignedTiers tags <> map show tiers))) tags
    disablePreferenceIf ignore name tags =
      if ignore then setTag name (LYT.TextValue (AreaPref.mkEnabledTagValue False)) tags else tags

-- | For a driver who becomes active in a fleet: applies the fleet's settings to the vehicle they already have, if any.
applyFleetSettingsToDriver ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  Id MerchantOperatingCity ->
  m ()
applyFleetSettingsToDriver driverId merchantOpCityId =
  QVehicle.findById driverId >>= mapM_ \vehicle -> do
    driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    cityTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing
    applyFleetSettings False cityTiers driver vehicle

-- | The fleet's per-driver switch turned off: these tiers come off the AutoAssign tag and the vehicle's auto-accept tiers.
removeAutoAssign ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  [ServiceTierType] ->
  Person ->
  DV.Vehicle ->
  m ()
removeAutoAssign tiers driver vehicle = do
  let oldTags = fromMaybe [] driver.driverTag
      remaining = filter (`notElem` map show tiers) (autoAssignedTiers oldTags)
      newTags =
        if null remaining
          then Yudhishthira.removeTagName (Just oldTags) (LYT.TagNameValue autoAssignTagName)
          else setTag autoAssignTagName (LYT.ArrayValue remaining) oldTags
  when (newTags /= oldTags) $ QPerson.updateDriverTag (Just newTags) driver.id
  updateAutoAcceptTiers vehicle (filter (`notElem` tiers) (fromMaybe [] vehicle.selectedAutoAcceptTiers))

setTag :: Text -> LYT.TagValue -> [LYT.TagNameValueExpiry] -> [LYT.TagNameValueExpiry]
setTag name value tags =
  Yudhishthira.removeTagName (Just tags) (LYT.TagNameValue name) <> [Yudhishthira.mkTagNameValueExpiryAt (LYT.TagName name) value Nothing]

updateAutoAcceptTiers ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  DV.Vehicle ->
  [ServiceTierType] ->
  m ()
updateAutoAcceptTiers vehicle autoAcceptTiers =
  -- Only this column: the vehicle may be an older read than a concurrent service-tier update.
  when (autoAcceptTiers /= fromMaybe [] vehicle.selectedAutoAcceptTiers) $
    QVehicle.updateSelectedAutoAcceptTiers autoAcceptTiers vehicle.driverId
