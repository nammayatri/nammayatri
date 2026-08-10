{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.ReconSettlementOrder (module Lib.Finance.Storage.Queries.ReconSettlementOrder, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.ReconSettlementOrder
import qualified Lib.Finance.Domain.Types.ReconUtrSettlement
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.ReconSettlementOrder as Beam
import Lib.Finance.Storage.Queries.ReconSettlementOrderExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder] -> m ())
createMany = traverse_ create

findByMessageId :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder]))
findByMessageId messageId = do findAllWithKV [Se.Is Beam.messageId $ Se.Eq messageId]

findByOrderId :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder]))
findByOrderId orderId = do findAllWithKV [Se.Is Beam.orderId $ Se.Eq orderId]

findByRideId :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder]))
findByRideId rideId = do findAllWithKV [Se.Is Beam.rideId $ Se.Eq rideId]

findByUtrSettlementId ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement) -> m ([Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder]))
findByUtrSettlementId utrSettlementId = do findAllWithKV [Se.Is Beam.utrSettlementId $ Se.Eq (Kernel.Types.Id.getId <$> utrSettlementId)]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder -> m (Maybe Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.allocatedBankCash allocatedBankCash,
      Se.Set Beam.bffAmount bffAmount,
      Se.Set Beam.bffType bffType,
      Se.Set Beam.claimedGrossAmount claimedGrossAmount,
      Se.Set Beam.claimedSettlementAmount claimedSettlementAmount,
      Se.Set Beam.correctionForOrderRowId (Kernel.Types.Id.getId <$> correctionForOrderRowId),
      Se.Set Beam.deductionByCollector deductionByCollector,
      Se.Set Beam.diffAmount diffAmount,
      Se.Set Beam.driverId driverId,
      Se.Set Beam.invoiceNo invoiceNo,
      Se.Set Beam.manualConfirmationReason manualConfirmationReason,
      Se.Set Beam.manuallyConfirmedAt manuallyConfirmedAt,
      Se.Set Beam.manuallyConfirmedBy manuallyConfirmedBy,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.messageId messageId,
      Se.Set Beam.orderId orderId,
      Se.Set Beam.orderSequence orderSequence,
      Se.Set Beam.orderState orderState,
      Se.Set Beam.orderTransactionId orderTransactionId,
      Se.Set Beam.ourReconStatus ourReconStatus,
      Se.Set Beam.paymentStatus paymentStatus,
      Se.Set Beam.payoutEligible payoutEligible,
      Se.Set Beam.platformGrossFare platformGrossFare,
      Se.Set Beam.platformNetReceivable platformNetReceivable,
      Se.Set Beam.rawJson rawJson,
      Se.Set Beam.reasonCode reasonCode,
      Se.Set Beam.receivedAt receivedAt,
      Se.Set Beam.reconTransactionId reconTransactionId,
      Se.Set Beam.reconciliationStatus reconciliationStatus,
      Se.Set Beam.refundReference refundReference,
      Se.Set Beam.refundStatus refundStatus,
      Se.Set Beam.refundedAt refundedAt,
      Se.Set Beam.remarks remarks,
      Se.Set Beam.rideId rideId,
      Se.Set Beam.settlementClearedAt settlementClearedAt,
      Se.Set Beam.settlementDate settlementDate,
      Se.Set Beam.settlementDetailIndex settlementDetailIndex,
      Se.Set Beam.settlementId settlementId,
      Se.Set Beam.settlementReferenceNo settlementReferenceNo,
      Se.Set Beam.settlementType settlementType,
      Se.Set Beam.sourceType sourceType,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.utrSettlementId (Kernel.Types.Id.getId <$> utrSettlementId),
      Se.Set Beam.wireOrderReconStatus wireOrderReconStatus,
      Se.Set Beam.wireReconStatus wireReconStatus,
      Se.Set Beam.withholdingTaxGst withholdingTaxGst,
      Se.Set Beam.withholdingTaxTds withholdingTaxTds
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
