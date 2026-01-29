{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.College where

import qualified Domain.Types.College
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.College as Beam

instance FromTType' Beam.College Domain.Types.College.College where
  fromTType' (Beam.CollegeT {..}) = do
    pure $
      Just
        Domain.Types.College.College
          { collegeAddress = collegeAddress,
            collegeName = collegeName,
            contactName = contactName,
            contactPhoneNumber = EncryptedHashed <$> (Encrypted <$> contactPhoneNumberEncrypted) <*> contactPhoneNumberHash,
            contactRole = contactRole,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.College Domain.Types.College.College where
  toTType' (Domain.Types.College.College {..}) = do
    Beam.CollegeT
      { Beam.collegeAddress = collegeAddress,
        Beam.collegeName = collegeName,
        Beam.contactName = contactName,
        Beam.contactPhoneNumberEncrypted = contactPhoneNumber,
        Beam.contactPhoneNumberHash = (contactPhoneNumber <&> (.hash)),
        Beam.contactRole = contactRole,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.updatedAt = updatedAt
      }
