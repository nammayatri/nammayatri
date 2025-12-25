{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SuspectFlagRequest where

import qualified Domain.Types.SuspectFlagRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.SuspectFlagRequest as Beam

instance FromTType' Beam.SuspectFlagRequest Domain.Types.SuspectFlagRequest.SuspectFlagRequest where
  fromTType' (Beam.SuspectFlagRequestT {..}) = do
    pure $
      Just
        Domain.Types.SuspectFlagRequest.SuspectFlagRequest
          { adminApproval = adminApproval,
            approvedBy = approvedBy,
            createdAt = createdAt,
            dl = dl,
            firstName = firstName,
            flaggedBy = flaggedBy,
            flaggedCategory = flaggedCategory,
            flaggedReason = flaggedReason,
            flaggedStatus = flaggedStatus,
            id = Kernel.Types.Id.Id id,
            lastName = lastName,
            merchantShortId = merchantShortId,
            mobileNumber = mobileNumber,
            reportDetails = reportDetails,
            totalComplaintsCount = totalComplaintsCount,
            updatedAt = updatedAt,
            voterId = voterId,
            merchantId = Kernel.Types.Id.Id <$> merchantId
          }

instance ToTType' Beam.SuspectFlagRequest Domain.Types.SuspectFlagRequest.SuspectFlagRequest where
  toTType' (Domain.Types.SuspectFlagRequest.SuspectFlagRequest {..}) = do
    Beam.SuspectFlagRequestT
      { Beam.adminApproval = adminApproval,
        Beam.approvedBy = approvedBy,
        Beam.createdAt = createdAt,
        Beam.dl = dl,
        Beam.firstName = firstName,
        Beam.flaggedBy = flaggedBy,
        Beam.flaggedCategory = flaggedCategory,
        Beam.flaggedReason = flaggedReason,
        Beam.flaggedStatus = flaggedStatus,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lastName = lastName,
        Beam.merchantShortId = merchantShortId,
        Beam.mobileNumber = mobileNumber,
        Beam.reportDetails = reportDetails,
        Beam.totalComplaintsCount = totalComplaintsCount,
        Beam.updatedAt = updatedAt,
        Beam.voterId = voterId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId
      }
