{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.PassCategory where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.PassCategory
import qualified Storage.Beam.PassCategory as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.PassCategory Domain.Types.PassCategory.PassCategory
    where fromTType' (Beam.PassCategoryT {..}) = do pure $ Just Domain.Types.PassCategory.PassCategory{description = description,
                                                                                                       id = Kernel.Types.Id.Id id,
                                                                                                       merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                       merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                       name = name,
                                                                                                       createdAt = createdAt,
                                                                                                       updatedAt = updatedAt}
instance ToTType' Beam.PassCategory Domain.Types.PassCategory.PassCategory
    where toTType' (Domain.Types.PassCategory.PassCategory {..}) = do Beam.PassCategoryT{Beam.description = description,
                                                                                         Beam.id = Kernel.Types.Id.getId id,
                                                                                         Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                         Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                         Beam.name = name,
                                                                                         Beam.createdAt = createdAt,
                                                                                         Beam.updatedAt = updatedAt}



