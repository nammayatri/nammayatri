{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SearchRevised where

import qualified Domain.Types.SearchRequest
import qualified Domain.Types.SearchRevised
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRevised as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SearchRevised.SearchRevised -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SearchRevised.SearchRevised] -> m ())
createMany = traverse_ create

findAllByParentSearchId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ([Domain.Types.SearchRevised.SearchRevised]))
findAllByParentSearchId (Kernel.Types.Id.Id parentSearchId) = do findAllWithKV [Se.And [Se.Is Beam.parentSearchId $ Se.Eq parentSearchId]]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchRevised.SearchRevised -> m (Maybe Domain.Types.SearchRevised.SearchRevised))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchRevised.SearchRevised -> m (Maybe Domain.Types.SearchRevised.SearchRevised))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SearchRevised.SearchRevised -> m ())
updateByPrimaryKey (Domain.Types.SearchRevised.SearchRevised {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.parentSearchId (Kernel.Types.Id.getId parentSearchId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SearchRevised Domain.Types.SearchRevised.SearchRevised where
  fromTType' (Beam.SearchRevisedT {..}) = do
    pure $
      Just
        Domain.Types.SearchRevised.SearchRevised
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            parentSearchId = Kernel.Types.Id.Id parentSearchId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SearchRevised Domain.Types.SearchRevised.SearchRevised where
  toTType' (Domain.Types.SearchRevised.SearchRevised {..}) = do
    Beam.SearchRevisedT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.parentSearchId = Kernel.Types.Id.getId parentSearchId,
        Beam.updatedAt = updatedAt
      }
