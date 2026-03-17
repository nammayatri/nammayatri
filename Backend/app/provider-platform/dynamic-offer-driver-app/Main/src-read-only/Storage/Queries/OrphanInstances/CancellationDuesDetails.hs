{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.CancellationDuesDetails where

import qualified Domain.Types.CancellationDuesDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.CancellationDuesDetails as Beam

instance FromTType' Beam.CancellationDuesDetails Domain.Types.CancellationDuesDetails.CancellationDuesDetails where
  fromTType' (Beam.CancellationDuesDetailsT {..}) = do
    pure $
      Just
        Domain.Types.CancellationDuesDetails.CancellationDuesDetails
          { cancellationAmount = cancellationAmount,
            createdAt = createdAt,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            id = Kernel.Types.Id.Id id,
            paymentStatus = paymentStatus,
            rideId = Kernel.Types.Id.Id rideId,
            riderId = Kernel.Types.Id.Id riderId,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.CancellationDuesDetails Domain.Types.CancellationDuesDetails.CancellationDuesDetails where
  toTType' (Domain.Types.CancellationDuesDetails.CancellationDuesDetails {..}) = do
    Beam.CancellationDuesDetailsT
      { Beam.cancellationAmount = cancellationAmount,
        Beam.createdAt = createdAt,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.paymentStatus = paymentStatus,
        Beam.rideId = Kernel.Types.Id.getId rideId,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
