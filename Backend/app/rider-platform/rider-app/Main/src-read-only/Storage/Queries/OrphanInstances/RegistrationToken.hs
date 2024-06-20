{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RegistrationToken where

import qualified Domain.Types.RegistrationToken
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.RegistrationToken as Beam

instance FromTType' Beam.RegistrationToken Domain.Types.RegistrationToken.RegistrationToken where
  fromTType' (Beam.RegistrationTokenT {..}) = do
    pure $
      Just
        Domain.Types.RegistrationToken.RegistrationToken
          { id = Kernel.Types.Id.Id id,
            token = token,
            attempts = attempts,
            authMedium = authMedium,
            authType = authType,
            authValueHash = authValueHash,
            verified = verified,
            authExpiry = authExpiry,
            tokenExpiry = tokenExpiry,
            entityId = entityId,
            merchantId = merchantId,
            entityType = entityType,
            createdAt = createdAt,
            updatedAt = updatedAt,
            info = info,
            createdViaPartnerOrgId = Kernel.Types.Id.Id <$> createdViaPartnerOrgId
          }

instance ToTType' Beam.RegistrationToken Domain.Types.RegistrationToken.RegistrationToken where
  toTType' (Domain.Types.RegistrationToken.RegistrationToken {..}) = do
    Beam.RegistrationTokenT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.token = token,
        Beam.attempts = attempts,
        Beam.authMedium = authMedium,
        Beam.authType = authType,
        Beam.authValueHash = authValueHash,
        Beam.verified = verified,
        Beam.authExpiry = authExpiry,
        Beam.tokenExpiry = tokenExpiry,
        Beam.entityId = entityId,
        Beam.merchantId = merchantId,
        Beam.entityType = entityType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt,
        Beam.info = info,
        Beam.createdViaPartnerOrgId = Kernel.Types.Id.getId <$> createdViaPartnerOrgId
      }
