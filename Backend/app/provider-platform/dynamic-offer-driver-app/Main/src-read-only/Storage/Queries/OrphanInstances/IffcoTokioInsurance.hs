{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.IffcoTokioInsurance where

import qualified Domain.Types.IffcoTokioInsurance
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.IffcoTokioInsurance as Beam

instance FromTType' Beam.IffcoTokioInsurance Domain.Types.IffcoTokioInsurance.IffcoTokioInsurance where
  fromTType' (Beam.IffcoTokioInsuranceT {..}) = do
    pure $
      Just
        Domain.Types.IffcoTokioInsurance.IffcoTokioInsurance
          { certificateNumber = certificateNumber,
            createdAt = createdAt,
            declarationId = declarationId,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            iffcoStatus = iffcoStatus,
            insuranceStatus = insuranceStatus,
            invoiceRequestNumber = invoiceRequestNumber,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.IffcoTokioInsurance Domain.Types.IffcoTokioInsurance.IffcoTokioInsurance where
  toTType' (Domain.Types.IffcoTokioInsurance.IffcoTokioInsurance {..}) = do
    Beam.IffcoTokioInsuranceT
      { Beam.certificateNumber = certificateNumber,
        Beam.createdAt = createdAt,
        Beam.declarationId = declarationId,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.iffcoStatus = iffcoStatus,
        Beam.insuranceStatus = insuranceStatus,
        Beam.invoiceRequestNumber = invoiceRequestNumber,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.updatedAt = updatedAt
      }
