{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RsfReconLedgerEntry (module Storage.Queries.RsfReconLedgerEntry, module ReExport) where

import qualified Domain.Types.RsfReconLedgerEntry
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RsfReconLedgerEntry as Beam
import Storage.Queries.RsfReconLedgerEntryExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RsfReconLedgerEntry.RsfReconLedgerEntry -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RsfReconLedgerEntry.RsfReconLedgerEntry] -> m ())
createMany = traverse_ create

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.RsfReconLedgerEntry.RsfReconLedgerEntry -> m (Maybe Domain.Types.RsfReconLedgerEntry.RsfReconLedgerEntry))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RsfReconLedgerEntry.RsfReconLedgerEntry -> m ())
updateByPrimaryKey (Domain.Types.RsfReconLedgerEntry.RsfReconLedgerEntry {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actorId actorId,
      Se.Set Beam.actorType actorType,
      Se.Set Beam.amount amount,
      Se.Set Beam.bapId bapId,
      Se.Set Beam.bapUri bapUri,
      Se.Set Beam.bffAmount bffAmount,
      Se.Set Beam.bffType bffType,
      Se.Set Beam.claimStatus claimStatus,
      Se.Set Beam.collectorAppId collectorAppId,
      Se.Set Beam.contextTransactionId contextTransactionId,
      Se.Set Beam.counterpartyReconStatus counterpartyReconStatus,
      Se.Set Beam.currency currency,
      Se.Set Beam.deductionByCollector deductionByCollector,
      Se.Set Beam.diffMessageCode diffMessageCode,
      Se.Set Beam.diffMessageName diffMessageName,
      Se.Set Beam.driverId driverId,
      Se.Set Beam.effectiveAt effectiveAt,
      Se.Set Beam.entryType entryType,
      Se.Set Beam.invoiceNo invoiceNo,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.messageId messageId,
      Se.Set Beam.orderId orderId,
      Se.Set Beam.orderPaymentAmount orderPaymentAmount,
      Se.Set Beam.orderState orderState,
      Se.Set Beam.orderTransactionId orderTransactionId,
      Se.Set Beam.rawJson rawJson,
      Se.Set Beam.reason reason,
      Se.Set Beam.rejectionDiff rejectionDiff,
      Se.Set Beam.reportedAt reportedAt,
      Se.Set Beam.reportedInMessageId reportedInMessageId,
      Se.Set Beam.rideId rideId,
      Se.Set Beam.settlementId settlementId,
      Se.Set Beam.settlementReasonCode settlementReasonCode,
      Se.Set Beam.source source,
      Se.Set Beam.ttl ttl,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.utr utr,
      Se.Set Beam.withholdingTaxGst withholdingTaxGst,
      Se.Set Beam.withholdingTaxTds withholdingTaxTds
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
