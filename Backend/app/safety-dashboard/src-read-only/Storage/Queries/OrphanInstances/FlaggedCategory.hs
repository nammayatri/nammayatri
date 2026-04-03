{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.FlaggedCategory where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.FlaggedCategory
import qualified Storage.Beam.FlaggedCategory as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.FlaggedCategory Domain.Types.FlaggedCategory.FlaggedCategory
    where fromTType' (Beam.FlaggedCategoryT {..}) = do pure $ Just Domain.Types.FlaggedCategory.FlaggedCategory{createdAt = createdAt, id = Kernel.Types.Id.Id id, name = name, updatedAt = updatedAt}
instance ToTType' Beam.FlaggedCategory Domain.Types.FlaggedCategory.FlaggedCategory
    where toTType' (Domain.Types.FlaggedCategory.FlaggedCategory {..}) = do Beam.FlaggedCategoryT{Beam.createdAt = createdAt,
                                                                                                  Beam.id = Kernel.Types.Id.getId id,
                                                                                                  Beam.name = name,
                                                                                                  Beam.updatedAt = updatedAt}



