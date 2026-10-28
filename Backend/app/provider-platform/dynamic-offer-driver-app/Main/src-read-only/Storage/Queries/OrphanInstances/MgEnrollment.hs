{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.MgEnrollment where

import qualified Domain.Types.Extra.MgEnrollment
import qualified Domain.Types.MgEnrollment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Text
import qualified Storage.Beam.MgEnrollment as Beam

instance FromTType' Beam.MgEnrollment Domain.Types.MgEnrollment.MgEnrollment where
  fromTType' (Beam.MgEnrollmentT {..}) = do
    pure $
      Just
        Domain.Types.MgEnrollment.MgEnrollment
          { beneficiaryId = beneficiaryId,
            beneficiaryType = beneficiaryType,
            campaignId = Kernel.Types.Id.Id campaignId,
            driverId = Kernel.Types.Id.Id driverId,
            events = fromMaybe [] (Kernel.Utils.Text.decodeFromText events),
            id = Kernel.Types.Id.Id id,
            ledgerEntryId = ledgerEntryId,
            metrics = fromMaybe Domain.Types.Extra.MgEnrollment.emptyMgMetrics (Kernel.Utils.Text.decodeFromText metrics),
            optedInAt = optedInAt,
            optedInBy = Kernel.Types.Id.Id optedInBy,
            payoutPercent = payoutPercent,
            periodDate = periodDate,
            settledAt = settledAt,
            status = status,
            termsAcceptedAt = termsAcceptedAt,
            topUpAmount = topUpAmount,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MgEnrollment Domain.Types.MgEnrollment.MgEnrollment where
  toTType' (Domain.Types.MgEnrollment.MgEnrollment {..}) = do
    Beam.MgEnrollmentT
      { Beam.beneficiaryId = beneficiaryId,
        Beam.beneficiaryType = beneficiaryType,
        Beam.campaignId = Kernel.Types.Id.getId campaignId,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.events = Kernel.Utils.Text.encodeToText events,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.ledgerEntryId = ledgerEntryId,
        Beam.metrics = Kernel.Utils.Text.encodeToText metrics,
        Beam.optedInAt = optedInAt,
        Beam.optedInBy = Kernel.Types.Id.getId optedInBy,
        Beam.payoutPercent = payoutPercent,
        Beam.periodDate = periodDate,
        Beam.settledAt = settledAt,
        Beam.status = status,
        Beam.termsAcceptedAt = termsAcceptedAt,
        Beam.topUpAmount = topUpAmount,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
