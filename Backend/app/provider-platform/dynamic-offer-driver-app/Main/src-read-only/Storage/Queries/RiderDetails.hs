{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RiderDetails (module Storage.Queries.RiderDetails, module ReExport) where

import qualified Domain.Types.Person
import qualified Domain.Types.RiderDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RiderDetails as Beam
import Storage.Queries.RiderDetailsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RiderDetails.RiderDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RiderDetails.RiderDetails] -> m ())
createMany = traverse_ create

findAllReferredByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> m [Domain.Types.RiderDetails.RiderDetails])
findAllReferredByDriverId referredByDriver = do findAllWithDb [Se.Is Beam.referredByDriver $ Se.Eq (Kernel.Types.Id.getId <$> referredByDriver)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m (Maybe Domain.Types.RiderDetails.RiderDetails))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateCancellationDues :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m ())
updateCancellationDues cancellationDues id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.cancellationDues cancellationDues, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateDisputeChancesUsed :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m ())
updateDisputeChancesUsed disputeChancesUsed id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.disputeChancesUsed disputeChancesUsed, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateDisputeChancesUsedAndCancellationDues ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Int -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m ())
updateDisputeChancesUsedAndCancellationDues disputeChancesUsed cancellationDues id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.disputeChancesUsed disputeChancesUsed, Se.Set Beam.cancellationDues cancellationDues, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateFirstRideIdAndFlagReason ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.RiderDetails.PayoutFlagReason -> Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m ())
updateFirstRideIdAndFlagReason firstRideId payoutFlagReason id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.firstRideId firstRideId, Se.Set Beam.payoutFlagReason payoutFlagReason, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateFlagReasonAndIsDeviceIdExists ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.RiderDetails.PayoutFlagReason -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m ())
updateFlagReasonAndIsDeviceIdExists payoutFlagReason isDeviceIdExists id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.payoutFlagReason payoutFlagReason, Se.Set Beam.isDeviceIdExists isDeviceIdExists, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateHasTakenValidRide ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m ())
updateHasTakenValidRide hasTakenValidRide hasTakenValidRideAt id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.hasTakenValidRide hasTakenValidRide, Se.Set Beam.hasTakenValidRideAt hasTakenValidRideAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateIsDeviceIdExists :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m ())
updateIsDeviceIdExists isDeviceIdExists id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.isDeviceIdExists isDeviceIdExists, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateIsFlagConfirmed :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m ())
updateIsFlagConfirmed isFlagConfirmed id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.isFlagConfirmed isFlagConfirmed, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateNightSafetyChecks :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m ())
updateNightSafetyChecks nightSafetyChecks id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.nightSafetyChecks nightSafetyChecks, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateOtpCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m ())
updateOtpCode otpCode id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.otpCode otpCode, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updatePayoutFlagReason ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.RiderDetails.PayoutFlagReason -> Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m ())
updatePayoutFlagReason payoutFlagReason id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.payoutFlagReason payoutFlagReason, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m (Maybe Domain.Types.RiderDetails.RiderDetails))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RiderDetails.RiderDetails -> m ())
updateByPrimaryKey (Domain.Types.RiderDetails.RiderDetails {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bapId bapId,
      Se.Set Beam.cancellationDueRides (Kernel.Prelude.Just cancellationDueRides),
      Se.Set Beam.cancellationDues cancellationDues,
      Se.Set Beam.cancellationDuesPaid (Kernel.Prelude.Just cancellationDuesPaid),
      Se.Set Beam.cancelledRides (Kernel.Prelude.Just cancelledRides),
      Se.Set Beam.completedRides (Kernel.Prelude.Just completedRides),
      Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.disputeChancesUsed disputeChancesUsed,
      Se.Set Beam.firstRideId firstRideId,
      Se.Set Beam.hasTakenValidRide hasTakenValidRide,
      Se.Set Beam.hasTakenValidRideAt hasTakenValidRideAt,
      Se.Set Beam.isDeviceIdExists isDeviceIdExists,
      Se.Set Beam.isFlagConfirmed isFlagConfirmed,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.mobileCountryCode mobileCountryCode,
      Se.Set Beam.mobileNumberEncrypted (mobileNumber & unEncrypted . encrypted),
      Se.Set Beam.mobileNumberHash (mobileNumber & hash),
      Se.Set Beam.nightSafetyChecks nightSafetyChecks,
      Se.Set Beam.noOfTimesCanellationDuesPaid (Kernel.Prelude.Just noOfTimesCanellationDuesPaid),
      Se.Set Beam.noOfTimesWaiveOffUsed (Kernel.Prelude.Just noOfTimesWaiveOffUsed),
      Se.Set Beam.otpCode otpCode,
      Se.Set Beam.payoutFlagReason payoutFlagReason,
      Se.Set Beam.referralCode (Kernel.Types.Id.getId <$> referralCode),
      Se.Set Beam.referredAt referredAt,
      Se.Set Beam.referredByDriver (Kernel.Types.Id.getId <$> referredByDriver),
      Se.Set Beam.totalBookings (Kernel.Prelude.Just totalBookings),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validCancellations (Kernel.Prelude.Just validCancellations),
      Se.Set Beam.waivedOffAmount (Kernel.Prelude.Just waivedOffAmount)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
