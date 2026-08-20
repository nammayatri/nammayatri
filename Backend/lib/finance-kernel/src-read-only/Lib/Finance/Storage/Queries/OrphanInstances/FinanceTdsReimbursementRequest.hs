{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.FinanceTdsReimbursementRequest where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest
import qualified Lib.Finance.Storage.Beam.FinanceTdsReimbursementRequest as Beam

instance FromTType' Beam.FinanceTdsReimbursementRequest Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest where
  fromTType' (Beam.FinanceTdsReimbursementRequestT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest
          { assessmentYear = assessmentYear,
            certAmount = certAmount,
            certNumber = certNumber,
            createdAt = createdAt,
            documentId = Kernel.Types.Id.Id documentId,
            fleetOwnerId = fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            quarter = quarter,
            rejectionReason = rejectionReason,
            status = status,
            tanNumber = tanNumber,
            tdsRate = tdsRate,
            tdsSection = tdsSection,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FinanceTdsReimbursementRequest Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest where
  toTType' (Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest {..}) = do
    Beam.FinanceTdsReimbursementRequestT
      { Beam.assessmentYear = assessmentYear,
        Beam.certAmount = certAmount,
        Beam.certNumber = certNumber,
        Beam.createdAt = createdAt,
        Beam.documentId = Kernel.Types.Id.getId documentId,
        Beam.fleetOwnerId = fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.quarter = quarter,
        Beam.rejectionReason = rejectionReason,
        Beam.status = status,
        Beam.tanNumber = tanNumber,
        Beam.tdsRate = tdsRate,
        Beam.tdsSection = tdsSection,
        Beam.updatedAt = updatedAt
      }
