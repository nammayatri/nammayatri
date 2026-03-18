{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.Chargeback where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.Chargeback
import qualified Lib.Finance.Storage.Beam.Chargeback as Beam

instance FromTType' Beam.Chargeback Lib.Finance.Domain.Types.Chargeback.Chargeback where
  fromTType' (Beam.ChargebackT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.Chargeback.Chargeback
          { id = Kernel.Types.Id.Id id,
            settlementReportId = Kernel.Types.Id.Id settlementReportId,
            transactionId = transactionId,
            chargebackReasonCode = chargebackReasonCode,
            chargebackAmount = chargebackAmount,
            chargebackStatus = chargebackStatus,
            responseDeadline = responseDeadline,
            evidenceUrl = evidenceUrl,
            adminNotes = adminNotes,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Chargeback Lib.Finance.Domain.Types.Chargeback.Chargeback where
  toTType' (Lib.Finance.Domain.Types.Chargeback.Chargeback {..}) = do
    Beam.ChargebackT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.settlementReportId = Kernel.Types.Id.getId settlementReportId,
        Beam.transactionId = transactionId,
        Beam.chargebackReasonCode = chargebackReasonCode,
        Beam.chargebackAmount = chargebackAmount,
        Beam.chargebackStatus = chargebackStatus,
        Beam.responseDeadline = responseDeadline,
        Beam.evidenceUrl = evidenceUrl,
        Beam.adminNotes = adminNotes,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
