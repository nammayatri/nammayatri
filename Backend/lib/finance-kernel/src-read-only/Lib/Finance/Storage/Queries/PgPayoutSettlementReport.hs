{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Lib.Finance.Storage.Queries.PgPayoutSettlementReport (module Lib.Finance.Storage.Queries.PgPayoutSettlementReport, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Lib.Finance.Storage.Queries.PgPayoutSettlementReportExtra as ReExport
import qualified Lib.Finance.Domain.Types.PgPayoutSettlementReport
import qualified Lib.Finance.Storage.Beam.PgPayoutSettlementReport as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Sequelize as Se



create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.PgPayoutSettlementReport.PgPayoutSettlementReport -> m ())
create = createWithKV
createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.PgPayoutSettlementReport.PgPayoutSettlementReport] -> m ())
createMany = traverse_ create
updateReconStatus :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
                     (Lib.Finance.Domain.Types.PgPaymentSettlementReport.ReconStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.PgPayoutSettlementReport.PgPayoutSettlementReport -> m ())
updateReconStatus reconStatus reconMessage id = do {_now <- getCurrentTime;
                                                    updateOneWithKV [Se.Set Beam.reconStatus reconStatus, Se.Set Beam.reconMessage reconMessage, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]}
findByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
                    (Kernel.Types.Id.Id Lib.Finance.Domain.Types.PgPayoutSettlementReport.PgPayoutSettlementReport -> m (Maybe Lib.Finance.Domain.Types.PgPayoutSettlementReport.PgPayoutSettlementReport))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.PgPayoutSettlementReport.PgPayoutSettlementReport -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.PgPayoutSettlementReport.PgPayoutSettlementReport {..}) = do {_now <- getCurrentTime;
                                                                                                           updateWithKV [Se.Set Beam.bankName bankName,
                                                                                                                         Se.Set Beam.beneficiaryAccountNumber beneficiaryAccountNumber,
                                                                                                                         Se.Set Beam.beneficiaryIfsc beneficiaryIfsc,
                                                                                                                         Se.Set Beam.beneficiaryType beneficiaryType,
                                                                                                                         Se.Set Beam.currency currency,
                                                                                                                         Se.Set Beam.fulfillmentAmount fulfillmentAmount,
                                                                                                                         Se.Set Beam.fulfillmentDate fulfillmentDate,
                                                                                                                         Se.Set Beam.fulfillmentId fulfillmentId,
                                                                                                                         Se.Set Beam.fulfillmentInstrumentType fulfillmentInstrumentType,
                                                                                                                         Se.Set Beam.fulfillmentMethod fulfillmentMethod,
                                                                                                                         Se.Set Beam.fulfillmentResponseCode fulfillmentResponseCode,
                                                                                                                         Se.Set Beam.fulfillmentResponseMessage fulfillmentResponseMessage,
                                                                                                                         Se.Set Beam.fulfillmentStatus fulfillmentStatus,
                                                                                                                         Se.Set Beam.merchantId merchantId,
                                                                                                                         Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
                                                                                                                         Se.Set Beam.orderId orderId,
                                                                                                                         Se.Set Beam.paymentGateway paymentGateway,
                                                                                                                         Se.Set Beam.payoutCustomerId payoutCustomerId,
                                                                                                                         Se.Set Beam.payoutRequestId payoutRequestId,
                                                                                                                         Se.Set Beam.rawData rawData,
                                                                                                                         Se.Set Beam.reconMessage reconMessage,
                                                                                                                         Se.Set Beam.reconStatus reconStatus,
                                                                                                                         Se.Set Beam.referenceType referenceType,
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
                                                                                                                         Se.Set Beam.updatedAt _now,
                                                                                                                         Se.Set Beam.utr utr] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



