{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FRFSFleetRating where

import qualified Domain.Types.FRFSFleetRating
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FRFSFleetRating as Beam

instance FromTType' Beam.FRFSFleetRating Domain.Types.FRFSFleetRating.FRFSFleetRating where
  fromTType' (Beam.FRFSFleetRatingT {..}) = do
    pure $
      Just
        Domain.Types.FRFSFleetRating.FRFSFleetRating
          { createdAt = createdAt,
            fleetNumber = fleetNumber,
            gtfsId = gtfsId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            rating = rating,
            totalRatingCount = totalRatingCount,
            totalRatingScore = totalRatingScore,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSFleetRating Domain.Types.FRFSFleetRating.FRFSFleetRating where
  toTType' (Domain.Types.FRFSFleetRating.FRFSFleetRating {..}) = do
    Beam.FRFSFleetRatingT
      { Beam.createdAt = createdAt,
        Beam.fleetNumber = fleetNumber,
        Beam.gtfsId = gtfsId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.rating = rating,
        Beam.totalRatingCount = totalRatingCount,
        Beam.totalRatingScore = totalRatingScore,
        Beam.updatedAt = updatedAt
      }
