{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SharedCabBlameCount where

import qualified Domain.Types.SharedCabBlameCount
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.SharedCabBlameCount as Beam

instance FromTType' Beam.SharedCabBlameCount Domain.Types.SharedCabBlameCount.SharedCabBlameCount where
  fromTType' (Beam.SharedCabBlameCountT {..}) = do
    pure $
      Just
        Domain.Types.SharedCabBlameCount.SharedCabBlameCount
          { count = count,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            lastAt = lastAt,
            lastBookingId = lastBookingId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            subjectId = subjectId,
            subjectType = subjectType,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SharedCabBlameCount Domain.Types.SharedCabBlameCount.SharedCabBlameCount where
  toTType' (Domain.Types.SharedCabBlameCount.SharedCabBlameCount {..}) = do
    Beam.SharedCabBlameCountT
      { Beam.count = count,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lastAt = lastAt,
        Beam.lastBookingId = lastBookingId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.subjectId = subjectId,
        Beam.subjectType = subjectType,
        Beam.updatedAt = updatedAt
      }
