{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PenaltyRecord (module Storage.Queries.PenaltyRecord, module ReExport) where

import qualified Domain.Types.Merchant
import qualified Domain.Types.PenaltyRecord
import qualified Domain.Types.PenaltyRule
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PenaltyRecord as Beam
import Storage.Queries.PenaltyRecordExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PenaltyRecord.PenaltyRecord -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PenaltyRecord.PenaltyRecord] -> m ())
createMany = traverse_ create

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PenaltyRecord.PenaltyRecord -> m (Maybe Domain.Types.PenaltyRecord.PenaltyRecord))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.PenaltyRecord.PenaltyRecord])
findAllByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findAllByDriverIdAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.PenaltyRecord.PenaltyStatus -> m [Domain.Types.PenaltyRecord.PenaltyRecord])
findAllByDriverIdAndStatus driverId status = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.status $ Se.Eq status
        ]
    ]

findAllByMerchantIdAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Domain.Types.PenaltyRecord.PenaltyStatus -> m [Domain.Types.PenaltyRecord.PenaltyRecord])
findAllByMerchantIdAndStatus merchantId status = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.status $ Se.Eq status
        ]
    ]

findAllByMerchantId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m [Domain.Types.PenaltyRecord.PenaltyRecord])
findAllByMerchantId merchantId = do findAllWithKV [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]

updateStatusById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.PenaltyRecord.PenaltyStatus -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.PenaltyRecord.PenaltyRecord -> m ())
updateStatusById status updatedAt id = do
  updateWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateDisputeById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.PenaltyRecord.PenaltyStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.PenaltyRecord.PenaltyRecord -> m ())
updateDisputeById status disputeReason disputeEvidence updatedAt id = do
  updateWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.disputeReason disputeReason,
      Se.Set Beam.disputeEvidence disputeEvidence,
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateDisputeResolutionById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.PenaltyRecord.PenaltyStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.PenaltyRecord.PenaltyRecord -> m ())
updateDisputeResolutionById status disputeResolvedBy disputeResolvedAt updatedAt id = do
  updateWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.disputeResolvedBy disputeResolvedBy,
      Se.Set Beam.disputeResolvedAt disputeResolvedAt,
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateLedgerAndInvoiceById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.PenaltyRecord.PenaltyStatus -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.PenaltyRecord.PenaltyRecord -> m ())
updateLedgerAndInvoiceById ledgerEntryId invoiceId status updatedAt id = do
  updateWithKV
    [ Se.Set Beam.ledgerEntryId ledgerEntryId,
      Se.Set Beam.invoiceId invoiceId,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PenaltyRecord.PenaltyRecord -> m (Maybe Domain.Types.PenaltyRecord.PenaltyRecord))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PenaltyRecord.PenaltyRecord -> m ())
updateByPrimaryKey (Domain.Types.PenaltyRecord.PenaltyRecord {..}) = do
  updateWithKV
    [ Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.ruleId (Kernel.Types.Id.getId ruleId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.triggerEvent triggerEvent,
      Se.Set Beam.triggerEntityId triggerEntityId,
      Se.Set Beam.amount amount,
      Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.reason reason,
      Se.Set Beam.status status,
      Se.Set Beam.disputeReason disputeReason,
      Se.Set Beam.disputeEvidence disputeEvidence,
      Se.Set Beam.disputeResolvedBy disputeResolvedBy,
      Se.Set Beam.disputeResolvedAt disputeResolvedAt,
      Se.Set Beam.ledgerEntryId ledgerEntryId,
      Se.Set Beam.invoiceId invoiceId,
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PenaltyRecord Domain.Types.PenaltyRecord.PenaltyRecord where
  fromTType' (Beam.PenaltyRecordT {..}) = do
    pure $
      Just
        Domain.Types.PenaltyRecord.PenaltyRecord
          { id = Kernel.Types.Id.Id id,
            driverId = Kernel.Types.Id.Id driverId,
            ruleId = Kernel.Types.Id.Id ruleId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            triggerEvent = triggerEvent,
            triggerEntityId = triggerEntityId,
            amount = amount,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            reason = reason,
            status = status,
            disputeReason = disputeReason,
            disputeEvidence = disputeEvidence,
            disputeResolvedBy = disputeResolvedBy,
            disputeResolvedAt = disputeResolvedAt,
            ledgerEntryId = ledgerEntryId,
            invoiceId = invoiceId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PenaltyRecord Domain.Types.PenaltyRecord.PenaltyRecord where
  toTType' (Domain.Types.PenaltyRecord.PenaltyRecord {..}) = do
    Beam.PenaltyRecordT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.ruleId = Kernel.Types.Id.getId ruleId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.triggerEvent = triggerEvent,
        Beam.triggerEntityId = triggerEntityId,
        Beam.amount = amount,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.reason = reason,
        Beam.status = status,
        Beam.disputeReason = disputeReason,
        Beam.disputeEvidence = disputeEvidence,
        Beam.disputeResolvedBy = disputeResolvedBy,
        Beam.disputeResolvedAt = disputeResolvedAt,
        Beam.ledgerEntryId = ledgerEntryId,
        Beam.invoiceId = invoiceId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
