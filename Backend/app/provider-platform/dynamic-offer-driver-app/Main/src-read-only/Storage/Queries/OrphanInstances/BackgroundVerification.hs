{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.BackgroundVerification where

import qualified Domain.Types.BackgroundVerification
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.BackgroundVerification as Beam

instance FromTType' Beam.BackgroundVerification Domain.Types.BackgroundVerification.BackgroundVerification where
  fromTType' (Beam.BackgroundVerificationT {..}) = do
    invitationUrl' <- Kernel.Prelude.parseBaseUrl invitationUrl
    pure $
      Just
        Domain.Types.BackgroundVerification.BackgroundVerification
          { candidateId = candidateId,
            driverId = Kernel.Types.Id.Id driverId,
            expiresAt = expiresAt,
            invitationId = invitationId,
            invitationStatus = invitationStatus,
            invitationUrl = invitationUrl',
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            reportId = reportId,
            reportStatus = reportStatus,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.BackgroundVerification Domain.Types.BackgroundVerification.BackgroundVerification where
  toTType' (Domain.Types.BackgroundVerification.BackgroundVerification {..}) = do
    Beam.BackgroundVerificationT
      { Beam.candidateId = candidateId,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.expiresAt = expiresAt,
        Beam.invitationId = invitationId,
        Beam.invitationStatus = invitationStatus,
        Beam.invitationUrl = Kernel.Prelude.showBaseUrl invitationUrl,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.reportId = reportId,
        Beam.reportStatus = reportStatus,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
