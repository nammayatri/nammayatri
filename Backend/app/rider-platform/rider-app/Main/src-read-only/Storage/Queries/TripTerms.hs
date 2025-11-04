{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TripTerms where

import qualified Domain.Types.TripTerms
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TripTerms as Beam
import Storage.Queries.Transformers.TripTerms

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TripTerms.TripTerms -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TripTerms.TripTerms] -> m ())
createMany = traverse_ create

findById'' :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TripTerms.TripTerms -> m (Maybe Domain.Types.TripTerms.TripTerms))
findById'' id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TripTerms.TripTerms -> m (Maybe Domain.Types.TripTerms.TripTerms))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TripTerms.TripTerms -> m ())
updateByPrimaryKey (Domain.Types.TripTerms.TripTerms {..}) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.descriptions (intercalateDescriptions descriptions), Se.Set Beam.updatedAt (Just _now)] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.TripTerms Domain.Types.TripTerms.TripTerms where
  fromTType' (Beam.TripTermsT {..}) = do
    createdAt' <- getCreatedAt createdAt
    updatedAt' <- getUpdatedAt updatedAt
    pure $ Just Domain.Types.TripTerms.TripTerms {createdAt = createdAt', descriptions = splitDescriptions descriptions, id = Kernel.Types.Id.Id id, updatedAt = updatedAt'}

instance ToTType' Beam.TripTerms Domain.Types.TripTerms.TripTerms where
  toTType' (Domain.Types.TripTerms.TripTerms {..}) = do
    Beam.TripTermsT
      { Beam.createdAt = Kernel.Prelude.Just createdAt,
        Beam.descriptions = intercalateDescriptions descriptions,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt
      }
