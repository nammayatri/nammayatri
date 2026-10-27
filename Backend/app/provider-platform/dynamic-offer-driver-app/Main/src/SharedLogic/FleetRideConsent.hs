-- | Daily ride consent gates Control Center "Fleet Features" for a driver without changing
-- membership (isActive stays as-is):
--   * Auto-assign / drivers can change auto-assign  -> AutoAssign tags + vehicle auto-accept tiers
--   * Ignore area and trip-distance preferences     -> AreaPreferenceEnabled / DistancePreferenceEnabled tags
--   * Pooling priority (Normal + location rules)    -> LTS fleetOwnerId (pool reads fleet config from it)
--   * Drivers can cancel rides                      -> LTS fleetOwnerId (search/accept/cancel use it)
--
-- Without today's consent the driver is treated as a normal driver. Fleets with
-- allowConsentPopup = False keep the old behaviour: active members always get these features.
-- Hot search/pool paths stay unchanged; this module only rewrites LTS/tags on answer or stale check.
module SharedLogic.FleetRideConsent
  ( isRideConsentActive,
    syncFleetRidePerks,
  )
where

import qualified Data.Time as Time
import qualified Domain.Types.DriverBankAccount as DDBA
import Domain.Types.FleetConfig (FleetConfig)
import Domain.Types.FleetDriverAssociation (FleetDriverAssociation)
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person (Person)
import Kernel.Beam.Functions (findOneWithKV)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified SharedLogic.DriverPool.LTSDataSync as LTSSync
import qualified SharedLogic.FleetDriverSettings as FDS
import qualified Storage.Beam.DriverBankAccount as BeamDBA
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.DriverInformationExtra as QDIE
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.FleetOwnerInformation as FOI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error

-- | True when this active member should receive fleet ride perks right now.
isRideConsentActive :: Time.UTCTime -> Time.UTCTime -> FleetConfig -> FleetDriverAssociation -> Bool
isRideConsentActive _now _localNow fleetConfig _association
  | not fleetConfig.allowConsentPopup = True
isRideConsentActive now localNow _fleetConfig association =
  association.consentGiven == Just True && maybe False (validForThisLocalDay now localNow) association.consentValidTill

-- | consentValidTill may still be in the future after midnight if the clear job missed;
-- a different local day is a new consent day.
validForThisLocalDay :: Time.UTCTime -> Time.UTCTime -> Time.UTCTime -> Bool
validForThisLocalDay nowUtc localTime validTill =
  let localOffset = Time.diffUTCTime localTime nowUtc
      validTillLocalDay = Time.utctDay (Time.addUTCTime localOffset validTill)
   in nowUtc < validTill && validTillLocalDay == Time.utctDay localTime

-- | Grant or revoke pooling + auto-accept for this driver from the association/config in hand.
-- Membership (isActive) is not changed.
syncFleetRidePerks ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  Id MerchantOperatingCity ->
  Time.UTCTime ->
  Time.UTCTime ->
  FleetConfig ->
  FleetDriverAssociation ->
  m ()
syncFleetRidePerks driverId merchantOpCityId now localNow fleetConfig association =
  if isRideConsentActive now localNow fleetConfig association
    then grantFleetRidePerks driverId merchantOpCityId association
    else revokeFleetRidePerks driverId merchantOpCityId association

grantFleetRidePerks ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  Id MerchantOperatingCity ->
  FleetDriverAssociation ->
  m ()
grantFleetRidePerks driverId merchantOpCityId association = do
  let fleetOwnerId = Id association.fleetOwnerId
  now <- getCurrentTime
  (mbFleetBa :: Maybe DDBA.DriverBankAccount) <- findOneWithKV [Se.Is BeamDBA.driverId $ Se.Eq fleetOwnerId.getId]
  mbFleetOwnerInfo <- FOI.findByPrimaryKey fleetOwnerId
  let cashRideEffective = QFDA.effectiveEnableCashRide now Nothing (Just association) (mbFleetOwnerInfo >>= (.enableCashRide))
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate
      { LTSSync.fleetOwnerId = LTSSync.Set (Just fleetOwnerId.getId),
        LTSSync.chargesEnabled = LTSSync.Set (maybe False (.chargesEnabled) mbFleetBa),
        LTSSync.bankAccountPaymentMode = LTSSync.Set ((.paymentMode) =<< mbFleetBa),
        LTSSync.enableCashRide = LTSSync.Set (Just cashRideEffective)
      }
  FDS.applyFleetSettingsToDriver driverId merchantOpCityId

revokeFleetRidePerks ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  Id MerchantOperatingCity ->
  FleetDriverAssociation ->
  m ()
revokeFleetRidePerks driverId merchantOpCityId association = do
  now <- getCurrentTime
  mbDriverInfo <- QDIE.findById driverId
  let effective = QFDA.effectiveEnableCashRide now (mbDriverInfo >>= (.enableCashRide)) (Just association) Nothing
  -- Clear fleetOwnerId so pooling priority and fleet billing exemptions stop on the hot path.
  -- isActive is left alone; the driver stays a fleet member.
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate
      { LTSSync.fleetOwnerId = LTSSync.Set Nothing,
        LTSSync.enableCashRide = LTSSync.Set (Just effective)
      }
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  FDS.clearFleetPreferenceOverrides driver
  mbVehicle <- QVehicle.findById driverId
  whenJust mbVehicle $ \vehicle -> do
    cityTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing
    -- Re-read tags after preference clear so auto-assign removal does not overwrite it.
    driver' <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    FDS.removeAutoAssign (FDS.vehicleAutoAcceptTiers cityTiers vehicle) driver' vehicle
