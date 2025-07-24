{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SharedSearchRequest where

import qualified Domain.Types.SharedSearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SharedSearchRequest as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedSearchRequest.SharedSearchRequest -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SharedSearchRequest.SharedSearchRequest] -> m ())
createMany = traverse_ create

findActiveRequests :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedSearchRequest.SearchRequestStatus -> m ([Domain.Types.SharedSearchRequest.SharedSearchRequest]))
findActiveRequests status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

findByStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedSearchRequest.SearchRequestStatus -> m ([Domain.Types.SharedSearchRequest.SharedSearchRequest]))
findByStatus status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

findByTransactionId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.SharedSearchRequest.SharedSearchRequest))
findByTransactionId transactionId = do findOneWithKV [Se.Is Beam.transactionId $ Se.Eq transactionId]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedSearchRequest.SearchRequestStatus -> Kernel.Types.Id.Id Domain.Types.SharedSearchRequest.SharedSearchRequest -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.SharedSearchRequest.SharedSearchRequest -> m (Maybe Domain.Types.SharedSearchRequest.SharedSearchRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedSearchRequest.SharedSearchRequest -> m ())
updateByPrimaryKey (Domain.Types.SharedSearchRequest.SharedSearchRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.estimatedDistance estimatedDistance,
      Se.Set Beam.estimatedDuration estimatedDuration,
      Se.Set Beam.fromLocationIds (Kernel.Types.Id.getId <$> fromLocationIds),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.status status,
      Se.Set Beam.toLocationIds (Kernel.Types.Id.getId <$> toLocationIds),
      Se.Set Beam.tollCharges tollCharges,
      Se.Set Beam.tollNames tollNames,
      Se.Set Beam.transactionId transactionId,
      Se.Set Beam.tripCategory tripCategory,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.vehicleCategory vehicleCategory
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SharedSearchRequest Domain.Types.SharedSearchRequest.SharedSearchRequest where
  fromTType' (Beam.SharedSearchRequestT {..}) = do
    pure $
      Just
        Domain.Types.SharedSearchRequest.SharedSearchRequest
          { createdAt = createdAt,
            estimatedDistance = estimatedDistance,
            estimatedDuration = estimatedDuration,
            fromLocationIds = Kernel.Types.Id.Id <$> fromLocationIds,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            status = status,
            toLocationIds = Kernel.Types.Id.Id <$> toLocationIds,
            tollCharges = tollCharges,
            tollNames = tollNames,
            transactionId = transactionId,
            tripCategory = tripCategory,
            updatedAt = updatedAt,
            validTill = validTill,
            vehicleCategory = vehicleCategory
          }

instance ToTType' Beam.SharedSearchRequest Domain.Types.SharedSearchRequest.SharedSearchRequest where
  toTType' (Domain.Types.SharedSearchRequest.SharedSearchRequest {..}) = do
    Beam.SharedSearchRequestT
      { Beam.createdAt = createdAt,
        Beam.estimatedDistance = estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.fromLocationIds = Kernel.Types.Id.getId <$> fromLocationIds,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.status = status,
        Beam.toLocationIds = Kernel.Types.Id.getId <$> toLocationIds,
        Beam.tollCharges = tollCharges,
        Beam.tollNames = tollNames,
        Beam.transactionId = transactionId,
        Beam.tripCategory = tripCategory,
        Beam.updatedAt = updatedAt,
        Beam.validTill = validTill,
        Beam.vehicleCategory = vehicleCategory
      }
