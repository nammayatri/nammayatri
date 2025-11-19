{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DigilockerVerification where

import qualified Domain.Types.DigilockerVerification
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DigilockerVerification as Beam
import qualified Storage.Queries.Transformers.DigilockerVerification

instance FromTType' Beam.DigilockerVerification Domain.Types.DigilockerVerification.DigilockerVerification where
  fromTType' (Beam.DigilockerVerificationT {..}) = do
    pure $
      Just
        Domain.Types.DigilockerVerification.DigilockerVerification
          { accessToken = Storage.Queries.Transformers.DigilockerVerification.mkEncryptedItem accessTokenEncrypted accessTokenHash,
            accessTokenExpiresAt = accessTokenExpiresAt,
            authorizationCode = authorizationCode,
            codeChallenge = codeChallenge,
            codeMethod = codeMethod,
            codeVerifier = codeVerifier,
            createdAt = createdAt,
            docStatus = docStatus,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            responseCode = responseCode,
            responseDescription = responseDescription,
            scope = scope,
            sessionStatus = sessionStatus,
            stateId = stateId,
            tokenResponse = tokenResponse,
            updatedAt = updatedAt,
            vehicleCategory = vehicleCategory
          }

instance ToTType' Beam.DigilockerVerification Domain.Types.DigilockerVerification.DigilockerVerification where
  toTType' (Domain.Types.DigilockerVerification.DigilockerVerification {..}) = do
    Beam.DigilockerVerificationT
      { Beam.accessTokenEncrypted = Storage.Queries.Transformers.DigilockerVerification.mkFieldEncrypted accessToken,
        Beam.accessTokenHash = Storage.Queries.Transformers.DigilockerVerification.mkFieldHash accessToken,
        Beam.accessTokenExpiresAt = accessTokenExpiresAt,
        Beam.authorizationCode = authorizationCode,
        Beam.codeChallenge = codeChallenge,
        Beam.codeMethod = codeMethod,
        Beam.codeVerifier = codeVerifier,
        Beam.createdAt = createdAt,
        Beam.docStatus = docStatus,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.responseCode = responseCode,
        Beam.responseDescription = responseDescription,
        Beam.scope = scope,
        Beam.sessionStatus = sessionStatus,
        Beam.stateId = stateId,
        Beam.tokenResponse = tokenResponse,
        Beam.updatedAt = updatedAt,
        Beam.vehicleCategory = vehicleCategory
      }
