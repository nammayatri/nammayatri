{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The driver state a fleet's 'fleet_config' writes, using the tags dispatch already reads:
--
-- * autoAssignDefaultOn: a never-expiring AutoAssign#<every auto-accept-enabled tier of the
--   city> tag, plus the VEHICLE PART (those tiers in selectedAutoAcceptTiers, and the
--   AutoAcceptOnly ones the vehicle serves in selectedServiceTiers).
-- * ignoreDriverAreaAndDistancePreference: never-expiring AreaPreferenceEnabled#false and
--   DistancePreferenceEnabled#false tags.
--
-- 'applyFleetTags' runs when a fleet-driver link becomes active and 'clearFleetTags' when an
-- active link ends; both are called only from the association queries in
-- Storage.Queries.FleetDriverAssociationExtra, which every link / unlink flow goes through.
-- A config change is not pushed to current drivers: it applies to drivers who link afterwards.
--
-- This module is low-level on purpose: the association queries import it, so it must not
-- import Storage.Queries.Person, SharedLogic.VehicleServiceTier or SharedLogic.FleetFeatures,
-- whose module graphs reach those queries. The pure blocks below hold every decision and are
-- unit-tested; the monadic wrappers only read and write.
module SharedLogic.FleetTags
  ( -- * Hooks (never throw)
    applyFleetTags,
    clearFleetTags,

    -- * Auto-assign writes (also used by the per-driver dashboard switch)
    writeAutoAssignOn,
    writeAutoAssignOff,
    ensureAutoAssignTag,
    getCityAutoAcceptTiers,
    withDriverTiersLock,
    selectedServiceTiersLockKey,

    -- * Pure blocks
    FleetTagSettings (..),
    fleetTagsAfterJoin,
    fleetTagsAfterLeave,
    autoAssignVehicleFields,
    clearAutoAssignVehicleFields,
    servedAutoAcceptOnlyTiers,
    shouldApplyFleetVehiclePart,
    carryOverAutoAcceptTiers,
    isAutoAcceptEnabledTier,
    isAutoAcceptOnlyTier,

    -- * Tag helpers
    autoAssignTagValues,
    isReservedSystemTagName,
    isAreaPreferenceEnabledTagOn,
    clearDistancePreferenceOffTag,
    applyIgnorePreferenceTags,
  )
where

import qualified Dashboard.Common as DC
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.List as DL
import qualified Data.Text as T
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Vehicle as DV
import qualified Domain.Types.VehicleServiceTier as DVST
import Kernel.Beam.Functions (findOneWithKV)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import qualified Sequelize as Se
import SharedLogic.DriverPool.AreaPreference (areaPreferenceCellTagPrefix, areaPreferenceEnabledTagName, distancePreferenceEnabledTagName, mkEnabledTagValue)
import qualified Storage.Beam.Person as BeamP
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import Storage.ConfigPilot.Config.FleetConfig (FleetConfigDimensions (..))
import Storage.Queries.OrphanInstances.Person ()
import Storage.Queries.Person.DriverTag (updateDriverTag)
import Storage.Queries.Person.GetNearestDrivers (isTierEligibleForDriver)
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error

type FleetTagsFlow m r =
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  )

-- ---------------------------------------------------------------------------
-- Hooks
-- ---------------------------------------------------------------------------

-- | The two fleet_config switches that write driver state. A fleet without a config row
-- has both off, like SharedLogic.FleetFeatures.defaultFleetFeatures.
data FleetTagSettings = FleetTagSettings
  { autoAssignDefaultOn :: Bool,
    ignoreDriverAreaAndDistancePreference :: Bool
  }
  deriving (Show, Eq)

-- | From the in-memory Config Pilot copy of the city's fleet configs.
getFleetTagSettings :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Id DP.Person -> m FleetTagSettings
getFleetTagSettings merchantOpCityId fleetOwnerId = do
  mbConfig <- getOneConfig (FleetConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, fleetOwnerId = Just fleetOwnerId.getId}) Nothing
  pure
    FleetTagSettings
      { autoAssignDefaultOn = maybe False (.autoAssignDefaultOn) mbConfig,
        ignoreDriverAreaAndDistancePreference = maybe False (.ignoreDriverAreaAndDistancePreference) mbConfig
      }

-- | A fleet-driver link became active: writes what the fleet's switches ask for (see the
-- module header). Switches that are off leave the driver unchanged. Idempotent; logs and
-- swallows any failure, so it never fails the link.
applyFleetTags :: FleetTagsFlow m r => Id DP.Person -> Id DP.Person -> m ()
applyFleetTags driverId fleetOwnerId = guarded ("applyFleetTags:" <> driverId.getId) $ do
  mbDriver <- findDriver driverId
  whenJust mbDriver $ \driver -> do
    settings <- getFleetTagSettings driver.merchantOperatingCityId fleetOwnerId
    when (settings.autoAssignDefaultOn || settings.ignoreDriverAreaAndDistancePreference) $ do
      cityTiers <- if settings.autoAssignDefaultOn then getCityAutoAcceptTiers driver.merchantOperatingCityId else pure []
      withDriverTiersLock driverId $ do
        mbFreshDriver <- findDriver driverId
        whenJust mbFreshDriver $ \freshDriver -> do
          now <- getCurrentTime
          let existingTags = fromMaybe [] freshDriver.driverTag
              newTags = fleetTagsAfterJoin now settings (map (.serviceTierType) cityTiers) existingTags
          when (newTags /= existingTags) $ updateDriverTag (Just newTags) driverId
          when (settings.autoAssignDefaultOn && not (null cityTiers)) $
            QVehicle.findById driverId >>= mapM_ (writeVehicleTiers driverId . autoAssignVehicleFields now (Just newTags) cityTiers)

-- | An ACTIVE fleet-driver link ended: removes the AutoAssign and DistancePreferenceEnabled
-- tags, clears selectedAutoAcceptTiers and drops AutoAcceptOnly tiers from
-- selectedServiceTiers. AreaPreferenceEnabled#false is removed only when the fleet being
-- left ignores preferences, so a driver's own "area preference off" survives the unlink.
-- Logs and swallows any failure, so it never fails the unlink.
clearFleetTags :: FleetTagsFlow m r => Id DP.Person -> Id DP.Person -> m ()
clearFleetTags driverId fleetOwnerId = guarded ("clearFleetTags:" <> driverId.getId) $ do
  mbDriver <- findDriver driverId
  whenJust mbDriver $ \driver -> do
    settings <- getFleetTagSettings driver.merchantOperatingCityId fleetOwnerId
    autoAcceptOnlyTiers <- getCityAutoAcceptOnlyTierTypes driver.merchantOperatingCityId
    withDriverTiersLock driverId $ do
      mbFreshDriver <- findDriver driverId
      whenJust mbFreshDriver $ \freshDriver -> do
        let existingTags = fromMaybe [] freshDriver.driverTag
            newTags = fleetTagsAfterLeave settings.ignoreDriverAreaAndDistancePreference existingTags
        when (newTags /= existingTags) $ updateDriverTag (Just newTags) driverId
        QVehicle.findById driverId >>= mapM_ (writeVehicleTiers driverId . clearAutoAssignVehicleFields autoAcceptOnlyTiers)

-- | Auto-assign ON for one driver (caller holds 'withDriverTiersLock'): merges the tiers
-- into a never-expiring AutoAssign tag and applies the VEHICLE PART.
writeAutoAssignOn :: FleetTagsFlow m r => [DVST.VehicleServiceTier] -> DP.Person -> DV.Vehicle -> m ()
writeAutoAssignOn cityAutoAcceptTiers driver vehicle = do
  now <- getCurrentTime
  let existingTags = fromMaybe [] driver.driverTag
      newTags = mergeAutoAssignTag now (map (.serviceTierType) cityAutoAcceptTiers) existingTags
  when (newTags /= existingTags) $ updateDriverTag (Just newTags) driver.id
  writeVehicleTiers driver.id (autoAssignVehicleFields now (Just newTags) cityAutoAcceptTiers vehicle)

-- | Auto-assign OFF for one driver (caller holds 'withDriverTiersLock'): removes the
-- AutoAssign tag, clears selectedAutoAcceptTiers and drops AutoAcceptOnly tiers.
writeAutoAssignOff :: FleetTagsFlow m r => DP.Person -> DV.Vehicle -> m ()
writeAutoAssignOff driver vehicle = do
  let existingTags = fromMaybe [] driver.driverTag
      newTags = removeTagsNamed autoAssignTagName existingTags
  when (newTags /= existingTags) $ updateDriverTag (Just newTags) driver.id
  autoAcceptOnlyTiers <- getCityAutoAcceptOnlyTierTypes driver.merchantOperatingCityId
  writeVehicleTiers driver.id (clearAutoAssignVehicleFields autoAcceptOnlyTiers vehicle)

-- | Merges the tiers into the driver's never-expiring AutoAssign tag (fleet driver's new
-- vehicle, see SharedLogic.DriverOnboarding.buildFullVehicleFromRC).
ensureAutoAssignTag :: FleetTagsFlow m r => Id DP.Person -> [DTC.ServiceTierType] -> m ()
ensureAutoAssignTag driverId tiers =
  withDriverTiersLock driverId $ do
    mbDriver <- findDriver driverId
    whenJust mbDriver $ \driver -> do
      now <- getCurrentTime
      let existingTags = fromMaybe [] driver.driverTag
          newTags = mergeAutoAssignTag now tiers existingTags
      when (newTags /= existingTags) $ updateDriverTag (Just newTags) driverId

-- | Writes the vehicle's selectedServiceTiers / selectedAutoAcceptTiers when they changed.
writeVehicleTiers :: FleetTagsFlow m r => Id DP.Person -> DV.Vehicle -> m ()
writeVehicleTiers driverId vehicle = do
  mbCurrent <- QVehicle.findById driverId
  let unchanged current =
        current.selectedServiceTiers == vehicle.selectedServiceTiers
          && fromMaybe [] current.selectedAutoAcceptTiers == fromMaybe [] vehicle.selectedAutoAcceptTiers
  unless (maybe True unchanged mbCurrent) $
    QVehicle.updateSelectedServiceTiersAndAutoAcceptTiers vehicle.selectedServiceTiers (fromMaybe [] vehicle.selectedAutoAcceptTiers) driverId

-- | Read straight from the person table: Storage.Queries.Person cannot be imported here
-- (see the module header).
findDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m (Maybe DP.Person)
findDriver driverId = findOneWithKV [Se.Is BeamP.id $ Se.Eq driverId.getId]

-- | Runs the action, logging (not rethrowing) any failure.
guarded :: MonadFlow m => Text -> m () -> m ()
guarded label action = do
  result <- withTryCatch label action
  case result of
    Left err -> logError $ label <> " failed: " <> show err
    Right () -> pure ()

-- | The city's tiers with auto-acceptance enabled.
getCityAutoAcceptTiers :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m [DVST.VehicleServiceTier]
getCityAutoAcceptTiers merchantOpCityId = filter isAutoAcceptEnabledTier <$> CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing

getCityAutoAcceptOnlyTierTypes :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m [DTC.ServiceTierType]
getCityAutoAcceptOnlyTierTypes merchantOpCityId = map (.serviceTierType) . filter isAutoAcceptOnlyTier <$> CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing

-- | Serialises every write of a driver's selectedServiceTiers / selectedAutoAcceptTiers
-- (driver tier update, wallet auto-revoke, behaviour consequences, fleet tags).
selectedServiceTiersLockKey :: Id DP.Person -> Text
selectedServiceTiersLockKey driverId = "Driver:SelectedServiceTiers:DId-" <> driverId.getId

-- | Runs the action under the driver's selectedServiceTiers lock, which also serialises
-- the fleet tag writes. Throws when the lock could not be taken (the action did not run).
withDriverTiersLock :: (MonadFlow m, Redis.HedisFlow m r) => Id DP.Person -> m a -> m a
withDriverTiersLock driverId action = do
  resultRef <- liftIO $ newIORef Nothing
  Redis.withWaitOnLockRedisWithExpiry (selectedServiceTiersLockKey driverId) 5 10 $
    action >>= liftIO . writeIORef resultRef . Just
  liftIO (readIORef resultRef)
    >>= fromMaybeM (InternalError $ "Could not acquire the selected service tiers lock for driver " <> driverId.getId)

-- ---------------------------------------------------------------------------
-- Pure blocks
-- ---------------------------------------------------------------------------

-- | Driver tags after joining a fleet with these settings ('applyFleetTags').
fleetTagsAfterJoin :: UTCTime -> FleetTagSettings -> [DTC.ServiceTierType] -> [LYT.TagNameValueExpiry] -> [LYT.TagNameValueExpiry]
fleetTagsAfterJoin now settings cityAutoAcceptTiers tags
  | settings.ignoreDriverAreaAndDistancePreference = applyIgnorePreferenceTags True withAutoAssign
  | otherwise = withAutoAssign
  where
    withAutoAssign
      | settings.autoAssignDefaultOn = mergeAutoAssignTag now cityAutoAcceptTiers tags
      | otherwise = tags

-- | Driver tags after leaving a fleet ('clearFleetTags'); the flag is the left fleet's
-- ignoreDriverAreaAndDistancePreference.
fleetTagsAfterLeave :: Bool -> [LYT.TagNameValueExpiry] -> [LYT.TagNameValueExpiry]
fleetTagsAfterLeave leftFleetIgnoredPreferences tags = foldr removeTagsNamed tags tagNamesToRemove
  where
    fleetWroteAreaPreferenceOff = leftFleetIgnoredPreferences && not (isAreaPreferenceEnabledTagOn tags)
    tagNamesToRemove =
      [autoAssignTagName, distancePreferenceEnabledTagName]
        <> [areaPreferenceEnabledTagName | fleetWroteAreaPreferenceOff]

-- | One never-expiring AutoAssign tag holding the existing (non-expired) values plus these
-- tiers. No tier leaves the tags unchanged.
mergeAutoAssignTag :: UTCTime -> [DTC.ServiceTierType] -> [LYT.TagNameValueExpiry] -> [LYT.TagNameValueExpiry]
mergeAutoAssignTag _ [] tags = tags
mergeAutoAssignTag now tiers tags =
  setPermanentTag autoAssignTagName (LYT.ArrayValue (DL.nub (autoAssignTagValues now tags <> map show tiers))) tags

-- | VEHICLE PART of auto-assign: every given auto-accept tier joins selectedAutoAcceptTiers
-- (union with what is there), and only the AutoAcceptOnly ones this vehicle serves join
-- selectedServiceTiers, so a tier the vehicle cannot serve is never selected. The tags are
-- the driver's, for cohort-gated tiers.
autoAssignVehicleFields :: UTCTime -> Maybe [LYT.TagNameValueExpiry] -> [DVST.VehicleServiceTier] -> DV.Vehicle -> DV.Vehicle
autoAssignVehicleFields now driverTags cityAutoAcceptTiers vehicle =
  vehicle
    { DV.selectedAutoAcceptTiers = Just (DL.nub (fromMaybe [] vehicle.selectedAutoAcceptTiers <> map (.serviceTierType) cityAutoAcceptTiers)),
      DV.selectedServiceTiers = DL.nub (vehicle.selectedServiceTiers <> servedAutoAcceptOnlyTiers now driverTags vehicle cityAutoAcceptTiers)
    }

-- | Clears selectedAutoAcceptTiers and drops the AutoAcceptOnly tiers, which have no
-- existence without auto-accept, from selectedServiceTiers.
clearAutoAssignVehicleFields :: [DTC.ServiceTierType] -> DV.Vehicle -> DV.Vehicle
clearAutoAssignVehicleFields autoAcceptOnlyTiers vehicle =
  vehicle
    { DV.selectedAutoAcceptTiers = Just [],
      DV.selectedServiceTiers = filter (`notElem` autoAcceptOnlyTiers) vehicle.selectedServiceTiers
    }

-- | AutoAcceptOnly tiers among these that the vehicle serves: its variant is allowed and,
-- for a cohort-gated tier, the driver holds the cohort tag.
servedAutoAcceptOnlyTiers :: UTCTime -> Maybe [LYT.TagNameValueExpiry] -> DV.Vehicle -> [DVST.VehicleServiceTier] -> [DTC.ServiceTierType]
servedAutoAcceptOnlyTiers now driverTags vehicle tiers =
  [ tier.serviceTierType
    | tier <- tiers,
      isAutoAcceptOnlyTier tier,
      vehicle.variant `elem` tier.allowedVehicleVariant,
      isTierEligibleForDriver now driverTags tierByType tier.serviceTierType
  ]
  where
    tierByType = HashMap.fromList [(tier.serviceTierType, tier) | tier <- tiers]

-- | Whether a vehicle rebuilt from an RC gets the fleet's VEHICLE PART: the fleet has
-- autoAssignDefaultOn and the driver either had no vehicle or had auto-accept on, so an
-- owner's per-driver OFF survives a vehicle change.
shouldApplyFleetVehiclePart :: Bool -> Maybe DV.Vehicle -> Bool
shouldApplyFleetVehiclePart autoAssignDefaultOn mbPreviousVehicle =
  autoAssignDefaultOn && maybe True (not . null . fromMaybe [] . (.selectedAutoAcceptTiers)) mbPreviousVehicle

-- | Keeps the previous vehicle's auto-accept tiers on a vehicle rebuilt from an RC, when
-- the variant is unchanged; of those, only the city tiers the rebuilt vehicle's variant
-- can serve join selectedServiceTiers (selectedAutoAcceptTiers may list every city tier).
-- Another variant has different tiers, so nothing is carried over then.
carryOverAutoAcceptTiers :: [DVST.VehicleServiceTier] -> Maybe DV.Vehicle -> DV.Vehicle -> DV.Vehicle
carryOverAutoAcceptTiers _ Nothing rebuiltVehicle = rebuiltVehicle
carryOverAutoAcceptTiers cityTiers (Just previousVehicle) rebuiltVehicle
  | previousVehicle.variant /= rebuiltVehicle.variant || null previousAutoAcceptTiers = rebuiltVehicle
  | otherwise =
    rebuiltVehicle
      { DV.selectedAutoAcceptTiers = Just previousAutoAcceptTiers,
        DV.selectedServiceTiers = DL.nub (rebuiltVehicle.selectedServiceTiers <> filter (`elem` servableTiers) previousAutoAcceptTiers)
      }
  where
    previousAutoAcceptTiers = fromMaybe [] previousVehicle.selectedAutoAcceptTiers
    servableTiers = [tier.serviceTierType | tier <- cityTiers, rebuiltVehicle.variant `elem` tier.allowedVehicleVariant]

isAutoAcceptEnabledTier :: DVST.VehicleServiceTier -> Bool
isAutoAcceptEnabledTier tier = maybe False (.enabled) tier.autoAcceptanceConfig

isAutoAcceptOnlyTier :: DVST.VehicleServiceTier -> Bool
isAutoAcceptOnlyTier tier = maybe False (\cfg -> cfg.enabled && cfg.mode == DC.AutoAcceptOnly) tier.autoAcceptanceConfig

-- ---------------------------------------------------------------------------
-- Tag helpers
-- ---------------------------------------------------------------------------

autoAssignTagName :: Text
autoAssignTagName = "AutoAssign"

-- | Tags owned by the system (fleet controls / preference switches); the driver
-- self-service tag API must not let a driver write these.
reservedSystemTagNames :: [Text]
reservedSystemTagNames = [autoAssignTagName, areaPreferenceEnabledTagName, distancePreferenceEnabledTagName]

-- | Case-insensitive, whitespace-trimmed. Also reserves every AreaPreference* tag
-- (cells, radius, enabled flag), which only the area-preference flow writes.
isReservedSystemTagName :: Text -> Bool
isReservedSystemTagName name =
  normalized `elem` map T.toLower reservedSystemTagNames
    || T.toLower areaPreferenceTagFamilyPrefix `T.isPrefixOf` normalized
  where
    normalized = T.toLower (T.strip name)
    -- "AreaPreference_<geohash>" minus the trailing underscore: covers
    -- AreaPreference_*, AreaPreferenceRadius and AreaPreferenceEnabled.
    areaPreferenceTagFamilyPrefix = fromMaybe areaPreferenceCellTagPrefix (T.stripSuffix "_" areaPreferenceCellTagPrefix)

-- | True unless an explicit AreaPreferenceEnabled#false tag is present.
isAreaPreferenceEnabledTagOn :: [LYT.TagNameValueExpiry] -> Bool
isAreaPreferenceEnabledTagOn tags =
  areaPreferenceEnabledTagName <> "#" <> mkEnabledTagValue False
    `notElem` map (LYT.getTagNameValue . Yudhishthira.convertToTagNameValue) tags

-- | Drops the DistancePreferenceEnabled switch (only fleet tags write it), so the driver's
-- trip-distance preference applies again.
clearDistancePreferenceOffTag :: [LYT.TagNameValueExpiry] -> [LYT.TagNameValueExpiry]
clearDistancePreferenceOffTag = removeTagsNamed distancePreferenceEnabledTagName

-- | True: never-expiring AreaPreferenceEnabled#false and DistancePreferenceEnabled#false.
-- False: both tags removed.
applyIgnorePreferenceTags :: Bool -> [LYT.TagNameValueExpiry] -> [LYT.TagNameValueExpiry]
applyIgnorePreferenceTags True =
  setPermanentTag distancePreferenceEnabledTagName disabled
    . setPermanentTag areaPreferenceEnabledTagName disabled
  where
    disabled = LYT.TextValue (mkEnabledTagValue False)
applyIgnorePreferenceTags False =
  removeTagsNamed distancePreferenceEnabledTagName
    . removeTagsNamed areaPreferenceEnabledTagName

-- | Removes every tag carrying this name, regardless of value/expiry.
removeTagsNamed :: Text -> [LYT.TagNameValueExpiry] -> [LYT.TagNameValueExpiry]
removeTagsNamed name tags = Yudhishthira.removeTagName (Just tags) (LYT.TagNameValue name)

-- | Replaces any tag with this name by a single never-expiring name#value tag.
setPermanentTag :: Text -> LYT.TagValue -> [LYT.TagNameValueExpiry] -> [LYT.TagNameValueExpiry]
setPermanentTag name value tags =
  removeTagsNamed name tags <> [Yudhishthira.mkTagNameValueExpiryAt (LYT.TagName name) value Nothing]

-- | The "&"-separated values of the (non-expired) AutoAssign tag(s).
autoAssignTagValues :: UTCTime -> [LYT.TagNameValueExpiry] -> [Text]
autoAssignTagValues now tags =
  DL.nub
    [ value
      | tag <- Yudhishthira.filterExpiredTags' now tags,
        (tagName : tagValue : _) <- [T.splitOn "#" (LYT.getTagNameValue (Yudhishthira.convertToTagNameValue tag))],
        tagName == autoAssignTagName,
        value <- T.splitOn "&" tagValue,
        not (T.null value)
    ]
