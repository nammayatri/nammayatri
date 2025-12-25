{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FlaggedCategory where

import qualified Domain.Types.FlaggedCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FlaggedCategory as Beam

instance FromTType' Beam.FlaggedCategory Domain.Types.FlaggedCategory.FlaggedCategory where
  fromTType' (Beam.FlaggedCategoryT {..}) = do pure $ Just Domain.Types.FlaggedCategory.FlaggedCategory {createdAt = createdAt, id = Kernel.Types.Id.Id id, name = name, updatedAt = updatedAt}

instance ToTType' Beam.FlaggedCategory Domain.Types.FlaggedCategory.FlaggedCategory where
  toTType' (Domain.Types.FlaggedCategory.FlaggedCategory {..}) = do
    Beam.FlaggedCategoryT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.updatedAt = updatedAt
      }
