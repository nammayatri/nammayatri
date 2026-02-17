{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SearchTry (module Storage.Queries.SearchTry, module ReExport) where

import qualified Domain.Types.SearchTry
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SearchTry as Beam
import Storage.Queries.SearchTryExtra as ReExport
import Storage.Queries.Transformers.SearchTry

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SearchTry.SearchTry -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SearchTry.SearchTry] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchTry.SearchTry -> m (Maybe Domain.Types.SearchTry.SearchTry))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SearchTry.SearchTryStatus -> Kernel.Types.Id.Id Domain.Types.SearchTry.SearchTry -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchTry.SearchTry -> m (Maybe Domain.Types.SearchTry.SearchTry))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SearchTry.SearchTry -> m ())
updateByPrimaryKey (Domain.Types.SearchTry.SearchTry {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.baseFare (Kernel.Prelude.roundToIntegral baseFare),
      Se.Set Beam.baseFareAmount (Kernel.Prelude.Just baseFare),
      Se.Set Beam.billingCategory (Kernel.Prelude.Just billingCategory),
      Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.customerExtraFee (Kernel.Prelude.roundToIntegral <$> customerExtraFee),
      Se.Set Beam.customerExtraFeeAmount customerExtraFee,
      Se.Set Beam.emailDomain emailDomain,
      Se.Set Beam.estimateId estimateId,
      Se.Set Beam.estimateIds (Kernel.Prelude.Just estimateIds),
      Se.Set Beam.isAdvancedBookingEnabled (Kernel.Prelude.Just isAdvancedBookingEnabled),
      Se.Set Beam.isScheduled (Kernel.Prelude.Just isScheduled),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Prelude.Just $ Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.messageId messageId,
      Se.Set Beam.petCharges (Kernel.Prelude.roundToIntegral <$> petCharges),
      Se.Set Beam.petChargesAmount petCharges,
      Se.Set Beam.preferSafetyPlus (Kernel.Prelude.Just preferSafetyPlus),
      Se.Set Beam.requestId (Kernel.Types.Id.getId requestId),
      Se.Set Beam.searchRepeatCounter searchRepeatCounter,
      Se.Set Beam.searchRepeatType searchRepeatType,
      Se.Set Beam.serviceTierArray (Kernel.Prelude.Just serviceTierArray),
      Se.Set Beam.startTime startTime,
      Se.Set Beam.status status,
      Se.Set Beam.tripCategory (Kernel.Prelude.Just tripCategory),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.vehicleVariant vehicleServiceTier,
      Se.Set Beam.vehicleServiceTierName (Kernel.Prelude.Just vehicleServiceTierName)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
