{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.PgPaymentSettlementReport (module Lib.Finance.Storage.Queries.PgPaymentSettlementReport, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.PgPaymentSettlementReport as Beam
import Lib.Finance.Storage.Queries.PgPaymentSettlementReportExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.PgPaymentSettlementReport.PgPaymentSettlementReport -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.PgPaymentSettlementReport.PgPaymentSettlementReport] -> m ())
createMany = traverse_ create

findById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.PgPaymentSettlementReport.PgPaymentSettlementReport -> m (Maybe Lib.Finance.Domain.Types.PgPaymentSettlementReport.PgPaymentSettlementReport))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateReconStatus ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Finance.Domain.Types.PgPaymentSettlementReport.ReconStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.PgPaymentSettlementReport.PgPaymentSettlementReport -> m ())
updateReconStatus reconStatus reconMessage id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.reconStatus reconStatus, Se.Set Beam.reconMessage reconMessage, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.PgPaymentSettlementReport.PgPaymentSettlementReport -> m (Maybe Lib.Finance.Domain.Types.PgPaymentSettlementReport.PgPaymentSettlementReport))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.PgPaymentSettlementReport.PgPaymentSettlementReport -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.PgPaymentSettlementReport.PgPaymentSettlementReport {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currency currency,
      Se.Set Beam.disputeId disputeId,
      Se.Set Beam.disputeType disputeType,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.orderId orderId,
      Se.Set Beam.paymentGateway paymentGateway,
      Se.Set Beam.paymentMethod paymentMethod,
      Se.Set Beam.paymentMethodSubType paymentMethodSubType,
      Se.Set Beam.pgBaseFee pgBaseFee,
      Se.Set Beam.pgTax pgTax,
      Se.Set Beam.rawData rawData,
      Se.Set Beam.reconMessage reconMessage,
      Se.Set Beam.reconStatus reconStatus,
      Se.Set Beam.referenceId referenceId,
      Se.Set Beam.referenceType referenceType,
      Se.Set Beam.refundAmount refundAmount,
      Se.Set Beam.refundArn refundArn,
      Se.Set Beam.refundBaseFee refundBaseFee,
      Se.Set Beam.refundDate refundDate,
      Se.Set Beam.refundId refundId,
      Se.Set Beam.refundTax refundTax,
      Se.Set Beam.rrn rrn,
      Se.Set Beam.settlementAmount settlementAmount,
      Se.Set Beam.settlementDate settlementDate,
      Se.Set Beam.settlementId settlementId,
      Se.Set Beam.settlementMode settlementMode,
      Se.Set Beam.settlementType settlementType,
      Se.Set Beam.txnAmount txnAmount,
      Se.Set Beam.txnDate txnDate,
      Se.Set Beam.txnId txnId,
      Se.Set Beam.txnStatus txnStatus,
      Se.Set Beam.txnType txnType,
      Se.Set Beam.uniqueSplitId uniqueSplitId,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.utr utr,
      Se.Set Beam.vendorId vendorId
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
