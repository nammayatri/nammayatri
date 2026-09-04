{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyChangeRequest where

import qualified Domain.Types.FarePolicyChangeRequest
import qualified Domain.Types.FareProduct
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyChangeRequest as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FarePolicyChangeRequest.FarePolicyChangeRequest -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FarePolicyChangeRequest.FarePolicyChangeRequest] -> m ())
createMany = traverse_ create

findAllByFareProductId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FareProduct.FareProduct -> m ([Domain.Types.FarePolicyChangeRequest.FarePolicyChangeRequest]))
findAllByFareProductId fareProductId = do findAllWithKV [Se.Is Beam.fareProductId $ Se.Eq (Kernel.Types.Id.getId fareProductId)]

findAllByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.FarePolicyChangeRequest.FarePolicyChangeRequest]))
findAllByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

updateStatusById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.FarePolicyChangeRequest.FarePolicyChangeStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.FarePolicyChangeRequest.FarePolicyChangeRequest -> m ())
updateStatusById status checkedBy remarks id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.checkedBy checkedBy, Se.Set Beam.remarks remarks, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FarePolicyChangeRequest.FarePolicyChangeRequest -> m (Maybe Domain.Types.FarePolicyChangeRequest.FarePolicyChangeRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FarePolicyChangeRequest.FarePolicyChangeRequest -> m ())
updateByPrimaryKey (Domain.Types.FarePolicyChangeRequest.FarePolicyChangeRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.action action,
      Se.Set Beam.checkedBy checkedBy,
      Se.Set Beam.fareProductId (Kernel.Types.Id.getId fareProductId),
      Se.Set Beam.fareProductSnapshot fareProductSnapshot,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.reason reason,
      Se.Set Beam.remarks remarks,
      Se.Set Beam.requestedBy requestedBy,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FarePolicyChangeRequest Domain.Types.FarePolicyChangeRequest.FarePolicyChangeRequest where
  fromTType' (Beam.FarePolicyChangeRequestT {..}) = do
    pure $
      Just
        Domain.Types.FarePolicyChangeRequest.FarePolicyChangeRequest
          { action = action,
            checkedBy = checkedBy,
            fareProductId = Kernel.Types.Id.Id fareProductId,
            fareProductSnapshot = fareProductSnapshot,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            reason = reason,
            remarks = remarks,
            requestedBy = requestedBy,
            status = status,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FarePolicyChangeRequest Domain.Types.FarePolicyChangeRequest.FarePolicyChangeRequest where
  toTType' (Domain.Types.FarePolicyChangeRequest.FarePolicyChangeRequest {..}) = do
    Beam.FarePolicyChangeRequestT
      { Beam.action = action,
        Beam.checkedBy = checkedBy,
        Beam.fareProductId = Kernel.Types.Id.getId fareProductId,
        Beam.fareProductSnapshot = fareProductSnapshot,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.reason = reason,
        Beam.remarks = remarks,
        Beam.requestedBy = requestedBy,
        Beam.status = status,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
