{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverInformation (module Storage.Queries.DriverInformation, module ReExport) where

import qualified Domain.Types.DriverInformation
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverInformation as Beam
import Storage.Queries.DriverInformationExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverInformation.DriverInformation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverInformation.DriverInformation] -> m ())
createMany = traverse_ create

addReferralCode ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
addReferralCode referralCode referredByDriverId (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.referralCode referralCode,
      Se.Set Beam.referredByDriverId (Kernel.Types.Id.getId <$> referredByDriverId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq driverId]

incrementReferralCountByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
incrementReferralCountByPersonId totalReferred (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.totalReferred totalReferred, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq driverId]

removeAcUsageRestriction ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Double -> Domain.Types.DriverInformation.AirConditionedRestrictionType -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
removeAcUsageRestriction airConditionScore acUsageRestrictionType acRestrictionLiftCount (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.airConditionScore airConditionScore,
      Se.Set Beam.acUsageRestrictionType (Kernel.Prelude.Just acUsageRestrictionType),
      Se.Set Beam.acRestrictionLiftCount acRestrictionLiftCount,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq driverId]

updateAadhaarVerifiedState :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateAadhaarVerifiedState aadhaarVerified (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.aadhaarVerified aadhaarVerified, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq driverId]

updateAcUsageRestrictionAndScore ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.DriverInformation.AirConditionedRestrictionType -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateAcUsageRestrictionAndScore acUsageRestrictionType airConditionScore (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.acUsageRestrictionType (Kernel.Prelude.Just acUsageRestrictionType),
      Se.Set Beam.airConditionScore airConditionScore,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq driverId]

updateActivity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.DriverInformation.DriverMode -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateActivity active mode (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.active active, Se.Set Beam.mode mode, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq driverId]

updateAirConditionScore :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateAirConditionScore airConditionScore (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.airConditionScore airConditionScore, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq driverId]

updateCompAadhaarImagePath :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateCompAadhaarImagePath compAadhaarImagePath (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.compAadhaarImagePath compAadhaarImagePath, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq driverId]

updateDriverDob :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateDriverDob driverDob (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.driverDob driverDob, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq driverId]

updateDriverDowngradeForSuv :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateDriverDowngradeForSuv canDowngradeToHatchback canDowngradeToTaxi (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.canDowngradeToHatchback canDowngradeToHatchback,
      Se.Set Beam.canDowngradeToTaxi canDowngradeToTaxi,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq driverId]

updateDriverInformation ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateDriverInformation canDowngradeToSedan canDowngradeToHatchback canDowngradeToTaxi canSwitchToRental canSwitchToInterCity availableUpiApps (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.canDowngradeToSedan canDowngradeToSedan,
      Se.Set Beam.canDowngradeToHatchback canDowngradeToHatchback,
      Se.Set Beam.canDowngradeToTaxi canDowngradeToTaxi,
      Se.Set Beam.canSwitchToRental (Kernel.Prelude.Just canSwitchToRental),
      Se.Set Beam.canSwitchToInterCity (Kernel.Prelude.Just canSwitchToInterCity),
      Se.Set Beam.availableUpiApps availableUpiApps,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq driverId]

updateLastACStatusCheckedAt :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateLastACStatusCheckedAt lastACStatusCheckedAt (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.lastACStatusCheckedAt lastACStatusCheckedAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq driverId]

updateOnRide :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateOnRide onRide (Kernel.Types.Id.Id driverId) = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.onRide onRide, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq driverId]

updatePendingPayment :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updatePendingPayment paymentPending (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.paymentPending paymentPending, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq driverId]

updateRentalAndInterCitySwitch :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateRentalAndInterCitySwitch canSwitchToRental canSwitchToInterCity (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.canSwitchToRental (Kernel.Prelude.Just canSwitchToRental),
      Se.Set Beam.canSwitchToInterCity (Kernel.Prelude.Just canSwitchToInterCity),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq driverId]

updateSubscription :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateSubscription subscribed (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.subscribed subscribed, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq driverId]

updateTollRelatedIssueCount :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateTollRelatedIssueCount tollRelatedIssueCount (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.tollRelatedIssueCount tollRelatedIssueCount, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq driverId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverInformation.DriverInformation))
findByPrimaryKey (Kernel.Types.Id.Id driverId) = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq driverId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverInformation.DriverInformation -> m ())
updateByPrimaryKey (Domain.Types.DriverInformation.DriverInformation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.aadhaarVerified aadhaarVerified,
      Se.Set Beam.acRestrictionLiftCount acRestrictionLiftCount,
      Se.Set Beam.acUsageRestrictionType (Kernel.Prelude.Just acUsageRestrictionType),
      Se.Set Beam.active active,
      Se.Set Beam.adminId (Kernel.Types.Id.getId <$> adminId),
      Se.Set Beam.airConditionScore airConditionScore,
      Se.Set Beam.autoPayStatus autoPayStatus,
      Se.Set Beam.availableUpiApps availableUpiApps,
      Se.Set Beam.blockExpiryTime blockExpiryTime,
      Se.Set Beam.blockStateModifier blockStateModifier,
      Se.Set Beam.blocked blocked,
      Se.Set Beam.blockedReason blockedReason,
      Se.Set Beam.canDowngradeToHatchback canDowngradeToHatchback,
      Se.Set Beam.canDowngradeToSedan canDowngradeToSedan,
      Se.Set Beam.canDowngradeToTaxi canDowngradeToTaxi,
      Se.Set Beam.canSwitchToInterCity (Kernel.Prelude.Just canSwitchToInterCity),
      Se.Set Beam.canSwitchToRental (Kernel.Prelude.Just canSwitchToRental),
      Se.Set Beam.compAadhaarImagePath compAadhaarImagePath,
      Se.Set Beam.driverDob driverDob,
      Se.Set Beam.enabled enabled,
      Se.Set Beam.enabledAt enabledAt,
      Se.Set Beam.hasAdvanceBooking (Kernel.Prelude.Just hasAdvanceBooking),
      Se.Set Beam.lastACStatusCheckedAt lastACStatusCheckedAt,
      Se.Set Beam.lastEnabledOn lastEnabledOn,
      Se.Set Beam.mode mode,
      Se.Set Beam.numOfLocks numOfLocks,
      Se.Set Beam.onRide onRide,
      Se.Set Beam.payerVpa payerVpa,
      Se.Set Beam.paymentPending paymentPending,
      Se.Set Beam.referralCode referralCode,
      Se.Set Beam.referredByDriverId (Kernel.Types.Id.getId <$> referredByDriverId),
      Se.Set Beam.subscribed subscribed,
      Se.Set Beam.tollRelatedIssueCount tollRelatedIssueCount,
      Se.Set Beam.totalReferred totalReferred,
      Se.Set Beam.verified verified,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]
