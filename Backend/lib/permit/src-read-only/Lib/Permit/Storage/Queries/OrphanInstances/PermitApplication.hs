{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Permit.Storage.Queries.OrphanInstances.PermitApplication where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Permit.Domain.Types.PermitApplication
import qualified Lib.Permit.Storage.Beam.PermitApplication as Beam

instance FromTType' Beam.PermitApplication Lib.Permit.Domain.Types.PermitApplication.PermitApplication where
  fromTType' (Beam.PermitApplicationT {..}) = do
    pure $
      Just
        Lib.Permit.Domain.Types.PermitApplication.PermitApplication
          { applicantId = Kernel.Types.Id.Id applicantId,
            approvalLevel = approvalLevel,
            consentAt = consentAt,
            createdAt = createdAt,
            decidedAt = decidedAt,
            decidedById = decidedById,
            decidedByName = decidedByName,
            decisionReason = decisionReason,
            details = EncryptedHashed (Encrypted detailsEncrypted) detailsHash,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            permitDate = permitDate,
            permitType = permitType,
            primaryConflictHash = primaryConflictHash,
            qrToken = EncryptedHashed <$> (Encrypted <$> qrTokenEncrypted) <*> qrTokenHash,
            referenceNo = referenceNo,
            secondaryConflictHash = secondaryConflictHash,
            status = status,
            submittedAt = submittedAt,
            updatedAt = updatedAt,
            validFrom = validFrom,
            validTill = validTill
          }

instance ToTType' Beam.PermitApplication Lib.Permit.Domain.Types.PermitApplication.PermitApplication where
  toTType' (Lib.Permit.Domain.Types.PermitApplication.PermitApplication {..}) = do
    Beam.PermitApplicationT
      { Beam.applicantId = Kernel.Types.Id.getId applicantId,
        Beam.approvalLevel = approvalLevel,
        Beam.consentAt = consentAt,
        Beam.createdAt = createdAt,
        Beam.decidedAt = decidedAt,
        Beam.decidedById = decidedById,
        Beam.decidedByName = decidedByName,
        Beam.decisionReason = decisionReason,
        Beam.detailsEncrypted = ((details & unEncrypted . encrypted)),
        Beam.detailsHash = (details & hash),
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.permitDate = permitDate,
        Beam.permitType = permitType,
        Beam.primaryConflictHash = primaryConflictHash,
        Beam.qrTokenEncrypted = ((qrToken <&> unEncrypted . (.encrypted))),
        Beam.qrTokenHash = (qrToken <&> (.hash)),
        Beam.referenceNo = referenceNo,
        Beam.secondaryConflictHash = secondaryConflictHash,
        Beam.status = status,
        Beam.submittedAt = submittedAt,
        Beam.updatedAt = updatedAt,
        Beam.validFrom = validFrom,
        Beam.validTill = validTill
      }
