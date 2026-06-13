{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverIdentityInfo where

import qualified Domain.Types.DriverIdentityInfo
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverIdentityInfo as Beam

instance FromTType' Beam.DriverIdentityInfo Domain.Types.DriverIdentityInfo.DriverIdentityInfo where
  fromTType' (Beam.DriverIdentityInfoT {..}) = do
    pure $
      Just
        Domain.Types.DriverIdentityInfo.DriverIdentityInfo
          { address = address,
            addressDocumentType = addressDocumentType,
            addressState = addressState,
            courtRecord = courtRecord,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            nomineeDob = nomineeDob,
            nomineeName = nomineeName,
            nomineeRelationship = nomineeRelationship,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverIdentityInfo Domain.Types.DriverIdentityInfo.DriverIdentityInfo where
  toTType' (Domain.Types.DriverIdentityInfo.DriverIdentityInfo {..}) = do
    Beam.DriverIdentityInfoT
      { Beam.address = address,
        Beam.addressDocumentType = addressDocumentType,
        Beam.addressState = addressState,
        Beam.courtRecord = courtRecord,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.nomineeDob = nomineeDob,
        Beam.nomineeName = nomineeName,
        Beam.nomineeRelationship = nomineeRelationship,
        Beam.updatedAt = updatedAt
      }
