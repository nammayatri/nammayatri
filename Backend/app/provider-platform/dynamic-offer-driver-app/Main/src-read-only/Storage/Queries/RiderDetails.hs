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

updateHasTakenValidRide ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m ())
updateHasTakenValidRide hasTakenValidRide hasTakenValidRideAt id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.hasTakenValidRide hasTakenValidRide, Se.Set Beam.hasTakenValidRideAt hasTakenValidRideAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateNightSafetyChecks :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m ())
updateNightSafetyChecks nightSafetyChecks id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.nightSafetyChecks nightSafetyChecks, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateOtpCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m ())
updateOtpCode otpCode id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.otpCode otpCode, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m (Maybe Domain.Types.RiderDetails.RiderDetails))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RiderDetails.RiderDetails -> m ())
updateByPrimaryKey (Domain.Types.RiderDetails.RiderDetails {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.cancellationDues cancellationDues,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.disputeChancesUsed disputeChancesUsed,
      Se.Set Beam.hasTakenValidRide hasTakenValidRide,
      Se.Set Beam.hasTakenValidRideAt hasTakenValidRideAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.mobileCountryCode mobileCountryCode,
      Se.Set Beam.mobileNumberEncrypted (mobileNumber & unEncrypted . encrypted),
      Se.Set Beam.mobileNumberHash (mobileNumber & hash),
      Se.Set Beam.nightSafetyChecks nightSafetyChecks,
      Se.Set Beam.otpCode otpCode,
      Se.Set Beam.referralCode (Kernel.Types.Id.getId <$> referralCode),
      Se.Set Beam.referredAt referredAt,
      Se.Set Beam.referredByDriver (Kernel.Types.Id.getId <$> referredByDriver),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

{-
	DSL Source Link: file://./../../../spec/Storage/RiderDetails.yaml
-}
