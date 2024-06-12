{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PartnerOrganization where

import qualified Domain.Types.PartnerOrganization
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PartnerOrganization as Beam

instance FromTType' Beam.PartnerOrganization Domain.Types.PartnerOrganization.PartnerOrganization where
  fromTType' (Beam.PartnerOrganizationT {..}) = do
    pure $
      Just
        Domain.Types.PartnerOrganization.PartnerOrganization
          { apiKey = EncryptedHashed (Encrypted apiKeyEncrypted) apiKeyHash,
            createdAt = createdAt,
            merchantId = Kernel.Types.Id.Id merchantId,
            name = name,
            orgId = Kernel.Types.Id.Id orgId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PartnerOrganization Domain.Types.PartnerOrganization.PartnerOrganization where
  toTType' (Domain.Types.PartnerOrganization.PartnerOrganization {..}) = do
    Beam.PartnerOrganizationT
      { Beam.apiKeyEncrypted = unEncrypted . (.encrypted) $ apiKey,
        Beam.apiKeyHash = (.hash) apiKey,
        Beam.createdAt = createdAt,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.name = name,
        Beam.orgId = Kernel.Types.Id.getId orgId,
        Beam.updatedAt = updatedAt
      }
