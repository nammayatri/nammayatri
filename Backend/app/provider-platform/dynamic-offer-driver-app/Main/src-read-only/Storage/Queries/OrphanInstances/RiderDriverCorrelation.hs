{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RiderDriverCorrelation where

import qualified Domain.Types.RiderDriverCorrelation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.RiderDriverCorrelation as Beam

instance FromTType' Beam.RiderDriverCorrelation Domain.Types.RiderDriverCorrelation.RiderDriverCorrelation where
  fromTType' (Beam.RiderDriverCorrelationT {..}) = do
    pure $
      Just
        Domain.Types.RiderDriverCorrelation.RiderDriverCorrelation
          { createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            favourite = favourite,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
            riderDetailId = Kernel.Types.Id.Id riderDetailId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RiderDriverCorrelation Domain.Types.RiderDriverCorrelation.RiderDriverCorrelation where
  toTType' (Domain.Types.RiderDriverCorrelation.RiderDriverCorrelation {..}) = do
    Beam.RiderDriverCorrelationT
      { Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.favourite = favourite,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.mobileNumberEncrypted = mobileNumber & unEncrypted . encrypted,
        Beam.mobileNumberHash = mobileNumber & hash,
        Beam.riderDetailId = Kernel.Types.Id.getId riderDetailId,
        Beam.updatedAt = updatedAt
      }
