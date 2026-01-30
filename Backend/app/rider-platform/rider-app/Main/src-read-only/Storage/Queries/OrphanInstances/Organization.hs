{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Organization where

import qualified Domain.Types.Organization
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Organization as Beam

instance FromTType' Beam.Organization Domain.Types.Organization.Organization where
  fromTType' (Beam.OrganizationT {..}) = do
    pure $
      Just
        Domain.Types.Organization.Organization
          { contactName = contactName,
            contactPhoneNumber = EncryptedHashed <$> (Encrypted <$> contactPhoneNumberEncrypted) <*> contactPhoneNumberHash,
            contactRole = contactRole,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            organizationAddress = organizationAddress,
            organizationName = organizationName,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Organization Domain.Types.Organization.Organization where
  toTType' (Domain.Types.Organization.Organization {..}) = do
    Beam.OrganizationT
      { Beam.contactName = contactName,
        Beam.contactPhoneNumberEncrypted = contactPhoneNumber,
        Beam.contactPhoneNumberHash = (contactPhoneNumber <&> (.hash)),
        Beam.contactRole = contactRole,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.organizationAddress = organizationAddress,
        Beam.organizationName = organizationName,
        Beam.updatedAt = updatedAt
      }
