{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Fleet preference locks, v2: APPLYING the locked values to a driver (v1, 'SharedLogic.DriverPreferencePolicy', only
-- validates driver requests).
--
-- Called by system paths only, never by a driver handler: the fleet saving its policy (the
-- ApplyPreferencePolicy allocator job), a driver joining the fleet or getting a vehicle, the fleet turning the
-- driver's auto-assign ON, and a posting to the driver's wallet (ServiceTier only, with a cheap exit when nothing is
-- missing). It writes through the system writers and never goes through the v1 validators.
--
-- Per lock that carries a value (a frozen lock, value null, applies nothing):
--
--   * AirConditioned @ToggleValue v@: sets the vehicle's AC (vehicle + RC, then the automated tier recompute, the writers
--     'SharedLogic.DriverOnboarding.checkAndUpdateAirConditioned' uses). Never forces AC on while the driver has an AC
--     usage restriction from rider complaints, and never consumes their one-time lift. Forcing AC off is fine.
--   * ServiceTier @AutoAssignTiers@: selects every protected tier ('DPP.protectedTiers': on the AutoAssign tag, served by
--     the vehicle, not usage-restricted) with auto-accept, behind the same min-wallet check as the driver's own
--     service-tier update. Never removes anything.
--   * IntraCity / InterCity / Rental @ToggleValue v@: the trip-type switches the driver's service-tier update would write
--     for those values ('SVST.resultingTripTypeSwitches'), written together. A value those rules can't hold is logged
--     and left.
--
-- Everything for one driver runs under 'SVST.selectedServiceTiersLockKey' on fresh reads.
module SharedLogic.DriverPreferencePolicy.Apply
  ( ApplyOutcome (..),
    policyHasValues,
    applyPreferencePolicy,
    applyPreferencePolicyWithOutcome,
    applyFleetPolicyToDriver,
    restoreServiceTierLockOnWalletPosting,
  )
where

import qualified Dashboard.Common as DC
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.List as DL
import qualified Data.Text as T
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverPreferencePolicy as DDPP
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person (Person)
import qualified Domain.Types.Person as Fields
import qualified Domain.Types.TransporterConfig as Fields
import qualified Domain.Types.Vehicle as DV
import qualified Domain.Types.VehicleRegistrationCertificate as Fields
import qualified Domain.Types.VehicleServiceTier as DVST
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance (CounterpartyType (..))
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Beam.BeamFlow as IJBeamFlow
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.DriverPreferencePolicy as DPP
import SharedLogic.Finance.WalletAccount (getWalletBalanceByOwner)
import qualified SharedLogic.FleetDriverSettings as FDS
import qualified SharedLogic.VehicleServiceTier as SVST
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as QRC
import Tools.Error

data ApplyOutcome = Changed | Unchanged | Skipped Text
  deriving (Show, Eq)

-- | A policy with at least one lock that carries a value: the only kind there is anything to apply for.
policyHasValues :: DDPP.PreferencePolicy -> Bool
policyHasValues policy = any (isJust . (.value)) policy.locked

lockedToggle :: DDPP.DriverPreference -> DDPP.PreferencePolicy -> Maybe Bool
lockedToggle preference policy = case DPP.lockFor preference policy >>= (.value) of
  Just (DDPP.ToggleValue v) -> Just v
  _ -> Nothing

locksAutoAssignTiers :: DDPP.PreferencePolicy -> Bool
locksAutoAssignTiers policy = (DPP.lockFor DDPP.ServiceTier policy >>= (.value)) == Just DDPP.AutoAssignTiers

applyPreferencePolicy ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, EncFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  DDPP.PreferencePolicy ->
  Person ->
  m ()
applyPreferencePolicy policy driver = void $ applyPreferencePolicyWithOutcome policy driver

-- | Applies every lock of the policy that carries a value to the driver, and logs one summary line.
applyPreferencePolicyWithOutcome ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, EncFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  DDPP.PreferencePolicy ->
  Person ->
  m ApplyOutcome
applyPreferencePolicyWithOutcome policy driver
  | not (policyHasValues policy) = pure Unchanged
  | otherwise = withDriverLockResult driver.id Unchanged $ do
    let driverId = driver.id
        merchantOpCityId = driver.merchantOperatingCityId
    QVehicle.findById driverId >>= \case
      Nothing -> logSummary driverId [] (Skipped "no vehicle")
      Just vehicle -> do
        cityTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing
        driverInfo <- QDI.findById driverId >>= fromMaybeM DriverInfoNotFound
        acNotes <- maybe (pure []) (applyAirConditioned driverInfo vehicle cityTiers merchantOpCityId) (lockedToggle DDPP.AirConditioned policy)
        -- Re-read: the AC write and its tier recompute change both the vehicle and which tiers are usage-restricted.
        vehicle' <- QVehicle.findById driverId >>= fromMaybeM (VehicleNotFound driverId.getId)
        driverTiers <- SVST.fetchVehicleTierForDriverWithUsageRestriction SVST.AllowedVariants (Just driverInfo) (Just vehicle') Nothing (Just cityTiers) driverId merchantOpCityId
        tierNotes <-
          if locksAutoAssignTiers policy
            then applyServiceTiers vehicle' driverTiers driverId merchantOpCityId
            else pure []
        tripNotes <- applyTripTypes policy driverInfo driverTiers driverId
        let notes = acNotes <> tierNotes <> tripNotes
        logSummary driverId notes (if any (.noteChanged) notes then Changed else Unchanged)

-- | For a driver who just joined a fleet or got a vehicle, or whose fleet turned auto-assign on: applies their active
-- fleet's policy (newest association, as v1 reads it). No-op outside a fleet or when the policy has nothing to apply;
-- skipped (logged) while a campaign is in charge of the driver.
applyFleetPolicyToDriver ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, EncFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, IJBeamFlow.BeamFlow m r) =>
  Id Person ->
  Id MerchantOperatingCity ->
  m ()
applyFleetPolicyToDriver driverId merchantOpCityId = do
  context <- DPP.getDriverFleetContext DPP.Cached driverId merchantOpCityId
  if DPP.isCampaignControlled context
    then logInfo $ "Preference policy for driver " <> driverId.getId <> ": a campaign is in charge, the fleet's policy is not applied"
    else whenJust (find policyHasValues (DPP.contextPolicy context)) $ \policy -> do
      driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      applyPreferencePolicy policy driver

-- | After a posting to the driver's wallet (wallet gate on): re-applies the ServiceTier lock only, so a protected tier
-- the wallet now covers comes back (one the wallet strip removed, or one skipped as wallet-short).
--
-- Runs on every posting, so it exits early on the cheap reads first: a driver without a live AutoAssign tag stops at
-- the person read; then, from the vehicle and the (cached) city tiers, when no tier that could be protected is missing
-- ('missingAutoAssignTiers': tag ∩ vehicle variant, a superset of the protected tiers), it stops with a debug line, before
-- the fleet lookup, the lock and the wallet read. Only when something is missing does it read the fleet's policy and,
-- for a ServiceTier lock, take the lock and decide on fresh reads ('applyServiceTiers').
restoreServiceTierLockOnWalletPosting ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, IJBeamFlow.BeamFlow m r) =>
  Id Person ->
  Id MerchantOperatingCity ->
  m ()
restoreServiceTierLockOnWalletPosting driverId merchantOpCityId = do
  now <- getCurrentTime
  mbDriver <- QPerson.findById driverId
  whenJust mbDriver $ \driver -> do
    let liveTags = Yudhishthira.filterExpiredTags' now (fromMaybe [] driver.driverTag)
    unless (null (FDS.autoAssignedTiers liveTags)) $
      QVehicle.findById driverId >>= mapM_ \vehicle -> do
        cityTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing
        if null (missingAutoAssignTiers liveTags cityTiers vehicle)
          then logDebug $ "Preference policy (wallet posting) for driver " <> driverId.getId <> ": no protected tier missing, fast path (no lock, no wallet read)"
          else do
            -- The policy of the source in charge ('DPP.contextPolicy'): a campaign in charge has none in v2.
            context <- DPP.getDriverFleetContext DPP.Cached driverId merchantOpCityId
            let mbPolicy = DPP.contextPolicy context
            when (isNothing mbPolicy && DPP.isCampaignControlled context) $
              logDebug $ "Preference policy (wallet posting) for driver " <> driverId.getId <> ": a campaign is in charge, no policy to restore"
            when (maybe False locksAutoAssignTiers mbPolicy) $
              withDriverLock driverId $
                QVehicle.findById driverId >>= mapM_ \vehicle' -> do
                  driverTiers <- SVST.fetchVehicleTierForDriverWithUsageRestriction SVST.AllowedVariants Nothing (Just vehicle') Nothing (Just cityTiers) driverId merchantOpCityId
                  notes <- applyServiceTiers vehicle' driverTiers driverId merchantOpCityId
                  when (any (.noteChanged) notes) $
                    logInfo $ "Preference policy (wallet posting) for driver " <> driverId.getId <> ": " <> T.intercalate "; " (map (.noteText) notes)

-- | Tiers on the live AutoAssign tag that the vehicle's variant allows (a superset of 'DPP.protectedTiers', which also
-- drops cohort-ineligible and usage-restricted ones) and that are not selected, or not on auto-accept where the tier has
-- auto-accept enabled. Empty: no protected tier can be missing.
missingAutoAssignTiers :: [LYT.TagNameValueExpiry] -> [DVST.VehicleServiceTier] -> DV.Vehicle -> [DVST.VehicleServiceTier]
missingAutoAssignTiers liveTags cityTiers vehicle =
  [ tier
    | tier <- cityTiers,
      vehicle.variant `elem` tier.allowedVehicleVariant,
      show tier.serviceTierType `elem` tagged,
      tierMissing vehicle tier
  ]
  where
    tagged = FDS.autoAssignedTiers liveTags

tierMissing :: DV.Vehicle -> DVST.VehicleServiceTier -> Bool
tierMissing vehicle tier =
  tier.serviceTierType `notElem` vehicle.selectedServiceTiers
    || (isJust (find (.enabled) tier.autoAcceptanceConfig) && tier.serviceTierType `notElem` fromMaybe [] vehicle.selectedAutoAcceptTiers)

-------------------------------------------------------------------------------
-- Per lock

data Note = Note {noteChanged :: Bool, noteText :: Text}

withDriverLock :: (MonadFlow m, Redis.HedisFlow m r) => Id Person -> m () -> m ()
withDriverLock driverId = Redis.withWaitAndLockRedisWithTimeout (SVST.selectedServiceTiersLockKey driverId) 5 10 50000

-- | 'withDriverLock' for an action with a result (the lock helper only runs @m ()@).
withDriverLockResult :: (MonadFlow m, Redis.HedisFlow m r) => Id Person -> a -> m a -> m a
withDriverLockResult driverId fallback action = do
  ref <- liftIO (newIORef fallback)
  withDriverLock driverId (action >>= liftIO . writeIORef ref)
  liftIO (readIORef ref)

logSummary :: (MonadFlow m) => Id Person -> [Note] -> ApplyOutcome -> m ApplyOutcome
logSummary driverId notes outcome = do
  let details = case outcome of
        Skipped reason -> "skipped: " <> reason
        _ -> if null notes then "nothing to change" else T.intercalate "; " (map (.noteText) notes)
  logInfo $ "Preference policy applied for driver " <> driverId.getId <> ": " <> details
  pure outcome

-- | AC lock. The same writers as 'SharedLogic.DriverOnboarding.checkAndUpdateAirConditioned' (vehicle, last RC,
-- automated tier recompute), without its restriction handling: forcing AC on is skipped while the driver is
-- restricted, so their lift is never consumed. Stored AC is @vehicle.airConditioned /= Just False@ (v1's reading).
applyAirConditioned ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  DI.DriverInformation ->
  DV.Vehicle ->
  [DVST.VehicleServiceTier] ->
  Id MerchantOperatingCity ->
  Bool ->
  m [Note]
applyAirConditioned driverInfo vehicle cityTiers merchantOpCityId locked
  | (vehicle.airConditioned /= Just False) == locked = pure []
  | locked && acRestricted = pure [Note False "AC on skipped: AC usage restricted by rider complaints"]
  | otherwise = do
    let driverId = vehicle.driverId
    QVehicle.updateAirConditioned (Just locked) Nothing driverId
    mbRc <- QRC.findLastVehicleRCWrapper vehicle.registrationNo
    whenJust mbRc $ \rc -> QRC.updateAirConditioned (Just locked) rc.id
    SVST.recomputeSelectedServiceTiers (Just driverInfo) (Just vehicle {DV.airConditioned = Just locked}) Nothing (Just cityTiers) driverId merchantOpCityId
    pure [Note True ("AC set " <> if locked then "on" else "off")]
  where
    -- v1's restriction (validateAirConditioned lets a restricted driver turn AC off), or the score over every AC threshold
    -- of the vehicle's tiers (where checkAndUpdateAirConditioned would block or consume the lift).
    acThresholds = mapMaybe (.airConditionedThreshold) (filter (\tier -> vehicle.variant `elem` tier.allowedVehicleVariant) cityTiers)
    scoreOverThresholds = not (null acThresholds || any (fromMaybe 0 driverInfo.airConditionScore <=) acThresholds)
    acRestricted = driverInfo.acUsageRestrictionType /= DI.NoRestriction || scoreOverThresholds

-- | ServiceTier @AutoAssignTiers@: every protected tier selected, with auto-accept where the tier has it enabled. The
-- min-wallet check of postDriverUpdateServiceTiers (wallet gate on): a wallet-short tier gets no auto-accept, and an
-- AutoAcceptOnly one isn't selected either, so this never fights the wallet strip. Only adds. Caller holds the lock.
applyServiceTiers ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  DV.Vehicle ->
  [(DVST.VehicleServiceTier, Bool)] ->
  Id Person ->
  Id MerchantOperatingCity ->
  m [Note]
applyServiceTiers vehicle driverTiers driverId merchantOpCityId = do
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  now <- getCurrentTime
  let protected = DPP.protectedTiers now (fromMaybe [] driver.driverTag) driverTiers
      protectedConfigs = [tier | (tier, _) <- driverTiers, tier.serviceTierType `elem` protected]
      enabledAutoAccept tier = find (.enabled) tier.autoAcceptanceConfig
  if null protectedConfigs
    then pure [Note False "tiers: none protected"]
    else
      if not (any (tierMissing vehicle) protectedConfigs)
        then pure [Note False "tiers: all protected in place"]
        else do
          transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
          let walletGateEnabled = fromMaybe False transporterConfig.driverWalletConfig.enableWalletGatedTierCheck
              minBalance tier = enabledAutoAccept tier >>= (.minWalletBalance)
          mbBalance <-
            if walletGateEnabled && any (isJust . minBalance) protectedConfigs
              then getWalletBalanceByOwner DRIVER driverId.getId
              else pure Nothing
          -- A missing wallet row is not "below the minimum", the same rule as the platform's wallet strip
          -- ('checkAndAutoDisableWalletGatedTiers'), so a new driver without a wallet keeps auto-accept.
          let walletCovers tier = not walletGateEnabled || maybe True (\m -> maybe True (>= m) mbBalance) (minBalance tier)
              autoAcceptOnly tier = maybe False ((== DC.AutoAcceptOnly) . (.mode)) (enabledAutoAccept tier)
              baseTiers = [tier.serviceTierType | tier <- protectedConfigs, walletCovers tier || not (autoAcceptOnly tier)]
              autoAcceptTiers = [tier.serviceTierType | tier <- protectedConfigs, isJust (enabledAutoAccept tier), walletCovers tier]
              walletShort = [tier.serviceTierType | tier <- protectedConfigs, not (walletCovers tier)]
              currentAutoAccept = fromMaybe [] vehicle.selectedAutoAcceptTiers
              newSelected = DL.nub (vehicle.selectedServiceTiers <> baseTiers)
              newAutoAccept = DL.nub (currentAutoAccept <> autoAcceptTiers)
              addedSelected = newSelected DL.\\ vehicle.selectedServiceTiers
              addedAutoAccept = newAutoAccept DL.\\ currentAutoAccept
              isChange = not (null addedSelected && null addedAutoAccept)
          when isChange $ QVehicle.updateSelectedServiceTiersAndAutoAcceptTiers newSelected newAutoAccept driverId
          pure $
            [Note isChange ("tiers: selected +" <> show addedSelected <> ", auto-accept +" <> show addedAutoAccept)]
              <> [Note False ("tiers wallet-short: " <> show walletShort) | not (null walletShort)]

-- | IntraCity / InterCity / Rental with values: the switches postDriverUpdateServiceTiers would write when sent those
-- values ('SVST.resultingTripTypeSwitches'), written together, and only when a locked switch moves toward its value. A
-- locked switch those rules can't hold (no eligible tier for it, or intracity forced on) keeps its stored value: the
-- apply never moves a locked switch away from the lock. Caller holds the lock.
applyTripTypes ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisLTSFlowEnv r) =>
  DDPP.PreferencePolicy ->
  DI.DriverInformation ->
  [(DVST.VehicleServiceTier, Bool)] ->
  Id Person ->
  m [Note]
applyTripTypes policy driverInfo driverTiers driverId = do
  let mbRental = lockedToggle DDPP.Rental policy
      mbInterCity = lockedToggle DDPP.InterCity policy
      mbIntraCity = lockedToggle DDPP.IntraCity policy
      (rental, interCity, intraCity) = SVST.resultingTripTypeSwitches driverInfo driverTiers mbRental mbInterCity mbIntraCity
      -- A frozen lock (no value) holds the stored value; a valued lock never moves away from its value.
      isFrozen preference = isJust (DPP.lockFor preference policy) && isNothing (lockedToggle preference policy)
      holdOrKeep preference mbLocked stored result
        | isFrozen preference = stored
        | otherwise = case mbLocked of
          Just v | result /= v -> stored
          _ -> result
      rental' = holdOrKeep DDPP.Rental mbRental driverInfo.canSwitchToRental rental
      interCity' = holdOrKeep DDPP.InterCity mbInterCity driverInfo.canSwitchToInterCity interCity
      intraCity' = holdOrKeep DDPP.IntraCity mbIntraCity driverInfo.canSwitchToIntraCity intraCity
      lockedSwitches =
        [ (preference, v, stored, result)
          | (preference, Just v, stored, result) <-
              [ (DDPP.Rental, mbRental, driverInfo.canSwitchToRental, rental),
                (DDPP.InterCity, mbInterCity, driverInfo.canSwitchToInterCity, interCity),
                (DDPP.IntraCity, mbIntraCity, driverInfo.canSwitchToIntraCity, intraCity)
              ]
        ]
      moved = [preference | (preference, v, stored, result) <- lockedSwitches, result == v, stored /= v]
      unheld = [preference | (preference, v, _, result) <- lockedSwitches, result /= v]
  unless (null moved) $ QDI.updateRentalInterCityAndIntraCitySwitch rental' interCity' intraCity' driverId
  pure $
    [Note True ("trip types set: " <> show moved <> " (rental=" <> show rental' <> ", intercity=" <> show interCity' <> ", intracity=" <> show intraCity' <> ")") | not (null moved)]
      <> [Note False ("trip types can't hold: " <> show unheld <> " (no eligible tier, or a non-car / no intercity-rental forces intracity on)") | not (null unheld)]
