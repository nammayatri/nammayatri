{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.EstimateRevised where

import qualified Domain.Types.EstimateRevised
import qualified Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.EstimateRevised as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.EstimateRevised.EstimateRevised -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.EstimateRevised.EstimateRevised] -> m ())
createMany = traverse_ create

findAllByParentSearchId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ([Domain.Types.EstimateRevised.EstimateRevised]))
findAllByParentSearchId (Kernel.Types.Id.Id parentSearchId) = do findAllWithKV [Se.And [Se.Is Beam.parentSearchId $ Se.Eq parentSearchId]]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.EstimateRevised.EstimateRevised -> m (Maybe Domain.Types.EstimateRevised.EstimateRevised))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.EstimateRevised.EstimateRevised -> m (Maybe Domain.Types.EstimateRevised.EstimateRevised))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.EstimateRevised.EstimateRevised -> m ())
updateByPrimaryKey (Domain.Types.EstimateRevised.EstimateRevised {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.parentSearchId (Kernel.Types.Id.getId parentSearchId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.EstimateRevised Domain.Types.EstimateRevised.EstimateRevised where
  fromTType' (Beam.EstimateRevisedT {..}) = do
    pure $
      Just
        Domain.Types.EstimateRevised.EstimateRevised
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            parentSearchId = Kernel.Types.Id.Id parentSearchId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.EstimateRevised Domain.Types.EstimateRevised.EstimateRevised where
  toTType' (Domain.Types.EstimateRevised.EstimateRevised {..}) = do
    Beam.EstimateRevisedT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.parentSearchId = Kernel.Types.Id.getId parentSearchId,
        Beam.updatedAt = updatedAt
      }
