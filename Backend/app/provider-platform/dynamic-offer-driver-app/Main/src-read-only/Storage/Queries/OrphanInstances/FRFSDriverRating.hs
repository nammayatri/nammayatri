{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FRFSDriverRating where

import qualified Domain.Types.FRFSDriverRating
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FRFSDriverRating as Beam

instance FromTType' Beam.FRFSDriverRating Domain.Types.FRFSDriverRating.FRFSDriverRating where
  fromTType' (Beam.FRFSDriverRatingT {..}) = do
    pure $
      Just
        Domain.Types.FRFSDriverRating.FRFSDriverRating
          { bookingId = bookingId,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            driverRatingValue = driverRatingValue,
            feedbackDetails = feedbackDetails,
            fleetNumber = fleetNumber,
            fleetRatingValue = fleetRatingValue,
            gtfsId = gtfsId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            operatorBadgeToken = operatorBadgeToken,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSDriverRating Domain.Types.FRFSDriverRating.FRFSDriverRating where
  toTType' (Domain.Types.FRFSDriverRating.FRFSDriverRating {..}) = do
    Beam.FRFSDriverRatingT
      { Beam.bookingId = bookingId,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.driverRatingValue = driverRatingValue,
        Beam.feedbackDetails = feedbackDetails,
        Beam.fleetNumber = fleetNumber,
        Beam.fleetRatingValue = fleetRatingValue,
        Beam.gtfsId = gtfsId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.operatorBadgeToken = operatorBadgeToken,
        Beam.updatedAt = updatedAt
      }
