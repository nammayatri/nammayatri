{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.PPFRecon (module Storage.Queries.PPFRecon, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.PPFReconExtra as ReExport
import qualified Domain.Types.PPFRecon
import qualified Storage.Beam.PPFRecon as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PPFRecon.PPFRecon -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PPFRecon.PPFRecon] -> m ())
createMany = traverse_ create
findByEntityIdAndEntityType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Domain.Types.PPFRecon.PPFEntityType -> m (Maybe Domain.Types.PPFRecon.PPFRecon))
findByEntityIdAndEntityType entityId entityType = do findOneWithKV [Se.And [Se.Is Beam.entityId $ Se.Eq entityId, Se.Is Beam.entityType $ Se.Eq entityType]]
findByNetworkOrderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.PPFRecon.PPFRecon))
findByNetworkOrderId networkOrderId = do findOneWithKV [Se.And [Se.Is Beam.networkOrderId $ Se.Eq networkOrderId]]
findByTransactionId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ([Domain.Types.PPFRecon.PPFRecon]))
findByTransactionId transactionId = do findAllWithKV [Se.And [Se.Is Beam.transactionId $ Se.Eq transactionId]]
findPendingReconciliation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                             (Domain.Types.PPFRecon.PPFSettlementStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> m ([Domain.Types.PPFRecon.PPFRecon]))
findPendingReconciliation settlementStatus merchantId = do findAllWithKV [Se.And [Se.Is Beam.settlementStatus $ Se.Eq settlementStatus,
                                                                                  Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId)]]
updatePaymentStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PPFRecon.PPFPaymentStatus -> Kernel.Types.Id.Id Domain.Types.PPFRecon.PPFRecon -> m ())
updatePaymentStatusById paymentStatus id = do {_now <- getCurrentTime;
                                               updateWithKV [Se.Set Beam.paymentStatus paymentStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]}
updateReconStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                         (Domain.Types.PPFRecon.PPFSettlementStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.PPFRecon.PPFRecon -> m ())
updateReconStatusById settlementStatus reconInitiatedAt reconCompletedAt message id = do {_now <- getCurrentTime;
                                                                                          updateWithKV [Se.Set Beam.settlementStatus settlementStatus,
                                                                                                        Se.Set Beam.reconInitiatedAt reconInitiatedAt,
                                                                                                        Se.Set Beam.reconCompletedAt reconCompletedAt,
                                                                                                        Se.Set Beam.message message,
                                                                                                        Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]}
updateSettlementStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                              (Domain.Types.PPFRecon.PPFSettlementStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.PPFRecon.PPFRecon -> m ())
updateSettlementStatusById settlementStatus settlementRefNo settlementDate settledTimestamp id = do {_now <- getCurrentTime;
                                                                                                     updateWithKV [Se.Set Beam.settlementStatus settlementStatus,
                                                                                                                   Se.Set Beam.settlementRefNo settlementRefNo,
                                                                                                                   Se.Set Beam.settlementDate settlementDate,
                                                                                                                   Se.Set Beam.settledTimestamp settledTimestamp,
                                                                                                                   Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]}
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PPFRecon.PPFRecon -> m (Maybe Domain.Types.PPFRecon.PPFRecon))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PPFRecon.PPFRecon -> m ())
updateByPrimaryKey (Domain.Types.PPFRecon.PPFRecon {..}) = do {_now <- getCurrentTime;
                                                               updateWithKV [Se.Set Beam.beneficiaryBankAccount beneficiaryBankAccount,
                                                                             Se.Set Beam.beneficiaryIFSC beneficiaryIFSC,
                                                                             Se.Set Beam.buyerAppCommission ((.amount) buyerAppCommission),
                                                                             Se.Set Beam.collectorBankAccount collectorBankAccount,
                                                                             Se.Set Beam.collectorIFSC collectorIFSC,
                                                                             Se.Set Beam.collectorSubscriberId collectorSubscriberId,
                                                                             Se.Set Beam.differenceAmount (Kernel.Prelude.fmap (.amount) differenceAmount),
                                                                             Se.Set Beam.domain domain,
                                                                             Se.Set Beam.entityId entityId,
                                                                             Se.Set Beam.entityType entityType,
                                                                             Se.Set Beam.fulfilledTimestamp fulfilledTimestamp,
                                                                             Se.Set Beam.gstAmount (Kernel.Prelude.fmap (.amount) gstAmount),
                                                                             Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
                                                                             Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
                                                                             Se.Set Beam.message message,
                                                                             Se.Set Beam.networkFee (Kernel.Prelude.fmap (.amount) networkFee),
                                                                             Se.Set Beam.networkOrderId networkOrderId,
                                                                             Se.Set Beam.currency (((Kernel.Prelude.Just . (.currency))) orderAmount),
                                                                             Se.Set Beam.orderAmount ((.amount) orderAmount),
                                                                             Se.Set Beam.orderStatus orderStatus,
                                                                             Se.Set Beam.paymentAmount ((.amount) paymentAmount),
                                                                             Se.Set Beam.paymentReference paymentReference,
                                                                             Se.Set Beam.paymentStatus paymentStatus,
                                                                             Se.Set Beam.paymentTransactionId paymentTransactionId,
                                                                             Se.Set Beam.receiverSubscriberId receiverSubscriberId,
                                                                             Se.Set Beam.reconCompletedAt reconCompletedAt,
                                                                             Se.Set Beam.reconInitiatedAt reconInitiatedAt,
                                                                             Se.Set Beam.sellerShare ((.amount) sellerShare),
                                                                             Se.Set Beam.settledTimestamp settledTimestamp,
                                                                             Se.Set Beam.settlementAmount (Kernel.Prelude.fmap (.amount) settlementAmount),
                                                                             Se.Set Beam.settlementDate settlementDate,
                                                                             Se.Set Beam.settlementRefNo settlementRefNo,
                                                                             Se.Set Beam.settlementStatus settlementStatus,
                                                                             Se.Set Beam.tcs (Kernel.Prelude.fmap (.amount) tcs),
                                                                             Se.Set Beam.tds (Kernel.Prelude.fmap (.amount) tds),
                                                                             Se.Set Beam.transactionId transactionId,
                                                                             Se.Set Beam.withholdingAmount (Kernel.Prelude.fmap (.amount) withholdingAmount),
                                                                             Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



