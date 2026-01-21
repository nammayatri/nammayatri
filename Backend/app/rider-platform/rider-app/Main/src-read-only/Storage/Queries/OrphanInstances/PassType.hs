{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PassType where

import qualified Domain.Types.PassType
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PassType as Beam

instance FromTType' Beam.PassType Domain.Types.PassType.PassType where
  fromTType' (Beam.PassTypeT {..}) = do
    pure $
      Just
        Domain.Types.PassType.PassType
          { catchline = catchline,
            description = description,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            order = order,
            passCategoryId = Kernel.Types.Id.Id passCategoryId,
            passEnum = passEnum,
            title = title,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PassType Domain.Types.PassType.PassType where
  toTType' (Domain.Types.PassType.PassType {..}) = do
    Beam.PassTypeT
      { Beam.catchline = catchline,
        Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.order = order,
        Beam.passCategoryId = Kernel.Types.Id.getId passCategoryId,
        Beam.passEnum = passEnum,
        Beam.title = title,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
