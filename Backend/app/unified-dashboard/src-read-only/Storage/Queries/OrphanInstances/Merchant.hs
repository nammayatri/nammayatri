{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Merchant where

import qualified Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Merchant as Beam

instance FromTType' Beam.Merchant Domain.Types.Merchant.Merchant where
  fromTType' (Beam.MerchantT {..}) = do
    pure $
      Just
        Domain.Types.Merchant.Merchant
          { authToken = EncryptedHashed <$> (Encrypted <$> authTokenEncrypted) <*> authTokenHash,
            createdAt = createdAt,
            defaultOperatingCity = defaultOperatingCity,
            domain = domain,
            enabled = enabled,
            hasFleetMemberHierarchy = hasFleetMemberHierarchy,
            id = Kernel.Types.Id.Id id,
            is2faMandatory = is2faMandatory,
            isStrongNameCheckRequired = isStrongNameCheckRequired,
            requireAdminApprovalForFleetOnboarding = requireAdminApprovalForFleetOnboarding,
            serverNames = serverNames,
            shortId = Kernel.Types.Id.ShortId shortId,
            singleActiveSessionOnly = singleActiveSessionOnly,
            supportedOperatingCities = supportedOperatingCities,
            verifyFleetWhileLogin = verifyFleetWhileLogin,
            website = website,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Merchant Domain.Types.Merchant.Merchant where
  toTType' (Domain.Types.Merchant.Merchant {..}) = do
    Beam.MerchantT
      { Beam.authTokenEncrypted = ((authToken <&> unEncrypted . (.encrypted))),
        Beam.authTokenHash = (authToken <&> (.hash)),
        Beam.createdAt = createdAt,
        Beam.defaultOperatingCity = defaultOperatingCity,
        Beam.domain = domain,
        Beam.enabled = enabled,
        Beam.hasFleetMemberHierarchy = hasFleetMemberHierarchy,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.is2faMandatory = is2faMandatory,
        Beam.isStrongNameCheckRequired = isStrongNameCheckRequired,
        Beam.requireAdminApprovalForFleetOnboarding = requireAdminApprovalForFleetOnboarding,
        Beam.serverNames = serverNames,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.singleActiveSessionOnly = singleActiveSessionOnly,
        Beam.supportedOperatingCities = supportedOperatingCities,
        Beam.verifyFleetWhileLogin = verifyFleetWhileLogin,
        Beam.website = website,
        Beam.updatedAt = updatedAt
      }
