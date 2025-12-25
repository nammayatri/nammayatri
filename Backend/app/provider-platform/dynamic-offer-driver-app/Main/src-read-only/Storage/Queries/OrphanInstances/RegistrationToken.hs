{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RegistrationToken where

import qualified Domain.Types.RegistrationToken
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.RegistrationToken as Beam
import qualified Storage.Queries.Transformers.RegistrationToken

instance FromTType' Beam.RegistrationToken Domain.Types.RegistrationToken.RegistrationToken where
  fromTType' (Beam.RegistrationTokenT {..}) = do
    merchantOperatingCityId' <- Storage.Queries.Transformers.RegistrationToken.getMerchantOperatingCityId merchantOperatingCityId merchantId
    pure $
      Just
        Domain.Types.RegistrationToken.RegistrationToken
          { alternateNumberAttempts = alternateNumberAttempts,
            attempts = attempts,
            authExpiry = authExpiry,
            authMedium = authMedium,
            authType = authType,
            authValueHash = authValueHash,
            createdAt = createdAt,
            entityId = entityId,
            entityType = entityType,
            id = Kernel.Types.Id.Id id,
            info = info,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            token = token,
            tokenExpiry = tokenExpiry,
            updatedAt = updatedAt,
            verified = verified
          }

instance ToTType' Beam.RegistrationToken Domain.Types.RegistrationToken.RegistrationToken where
  toTType' (Domain.Types.RegistrationToken.RegistrationToken {..}) = do
    Beam.RegistrationTokenT
      { Beam.alternateNumberAttempts = alternateNumberAttempts,
        Beam.attempts = attempts,
        Beam.authExpiry = authExpiry,
        Beam.authMedium = authMedium,
        Beam.authType = authType,
        Beam.authValueHash = authValueHash,
        Beam.createdAt = createdAt,
        Beam.entityId = entityId,
        Beam.entityType = entityType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.info = info,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = Kernel.Prelude.Just merchantOperatingCityId,
        Beam.token = token,
        Beam.tokenExpiry = tokenExpiry,
        Beam.updatedAt = updatedAt,
        Beam.verified = verified
      }
