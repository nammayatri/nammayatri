{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.UnserviceableCityLead where

import qualified Domain.Types.UnserviceableCityLead
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.UnserviceableCityLead as Beam

instance FromTType' Beam.UnserviceableCityLead Domain.Types.UnserviceableCityLead.UnserviceableCityLead where
  fromTType' (Beam.UnserviceableCityLeadT {..}) = do
    pure $
      Just
        Domain.Types.UnserviceableCityLead.UnserviceableCityLead
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            merchantId = Kernel.Types.Id.Id merchantId,
            mobileCountryCode = mobileCountryCode,
            mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
            name = name,
            updatedAt = updatedAt,
            vehicleCategory = vehicleCategory
          }

instance ToTType' Beam.UnserviceableCityLead Domain.Types.UnserviceableCityLead.UnserviceableCityLead where
  toTType' (Domain.Types.UnserviceableCityLead.UnserviceableCityLead {..}) = do
    Beam.UnserviceableCityLeadT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.mobileCountryCode = mobileCountryCode,
        Beam.mobileNumberEncrypted = mobileNumber & unEncrypted . encrypted,
        Beam.mobileNumberHash = mobileNumber & hash,
        Beam.name = name,
        Beam.updatedAt = updatedAt,
        Beam.vehicleCategory = vehicleCategory
      }
