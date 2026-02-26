{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.PPFRecon where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.PPFRecon
import qualified Storage.Beam.PPFRecon as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id



instance FromTType' Beam.PPFRecon Domain.Types.PPFRecon.PPFRecon
    where fromTType' (Beam.PPFReconT {..}) = do pure $ Just Domain.Types.PPFRecon.PPFRecon{beneficiaryBankAccount = beneficiaryBankAccount,
                                                                                           beneficiaryIFSC = beneficiaryIFSC,
                                                                                           buyerAppCommission = Kernel.Types.Common.mkPrice currency buyerAppCommission,
                                                                                           collectorBankAccount = collectorBankAccount,
                                                                                           collectorIFSC = collectorIFSC,
                                                                                           collectorSubscriberId = collectorSubscriberId,
                                                                                           differenceAmount = Kernel.Prelude.fmap (Kernel.Types.Common.mkPrice currency) differenceAmount,
                                                                                           domain = domain,
                                                                                           entityId = entityId,
                                                                                           entityType = entityType,
                                                                                           fulfilledTimestamp = fulfilledTimestamp,
                                                                                           gstAmount = Kernel.Prelude.fmap (Kernel.Types.Common.mkPrice currency) gstAmount,
                                                                                           id = Kernel.Types.Id.Id id,
                                                                                           merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                           merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
                                                                                           message = message,
                                                                                           networkFee = Kernel.Prelude.fmap (Kernel.Types.Common.mkPrice currency) networkFee,
                                                                                           networkOrderId = networkOrderId,
                                                                                           orderAmount = Kernel.Types.Common.mkPrice currency orderAmount,
                                                                                           orderStatus = orderStatus,
                                                                                           paymentAmount = Kernel.Types.Common.mkPrice currency paymentAmount,
                                                                                           paymentReference = paymentReference,
                                                                                           paymentStatus = paymentStatus,
                                                                                           paymentTransactionId = paymentTransactionId,
                                                                                           receiverSubscriberId = receiverSubscriberId,
                                                                                           reconCompletedAt = reconCompletedAt,
                                                                                           reconInitiatedAt = reconInitiatedAt,
                                                                                           sellerShare = Kernel.Types.Common.mkPrice currency sellerShare,
                                                                                           settledTimestamp = settledTimestamp,
                                                                                           settlementAmount = Kernel.Prelude.fmap (Kernel.Types.Common.mkPrice currency) settlementAmount,
                                                                                           settlementDate = settlementDate,
                                                                                           settlementRefNo = settlementRefNo,
                                                                                           settlementStatus = settlementStatus,
                                                                                           tcs = Kernel.Prelude.fmap (Kernel.Types.Common.mkPrice currency) tcs,
                                                                                           tds = Kernel.Prelude.fmap (Kernel.Types.Common.mkPrice currency) tds,
                                                                                           transactionId = transactionId,
                                                                                           withholdingAmount = Kernel.Prelude.fmap (Kernel.Types.Common.mkPrice currency) withholdingAmount,
                                                                                           createdAt = createdAt,
                                                                                           updatedAt = updatedAt}
instance ToTType' Beam.PPFRecon Domain.Types.PPFRecon.PPFRecon
    where toTType' (Domain.Types.PPFRecon.PPFRecon {..}) = do Beam.PPFReconT{Beam.beneficiaryBankAccount = beneficiaryBankAccount,
                                                                             Beam.beneficiaryIFSC = beneficiaryIFSC,
                                                                             Beam.buyerAppCommission = (.amount) buyerAppCommission,
                                                                             Beam.collectorBankAccount = collectorBankAccount,
                                                                             Beam.collectorIFSC = collectorIFSC,
                                                                             Beam.collectorSubscriberId = collectorSubscriberId,
                                                                             Beam.differenceAmount = Kernel.Prelude.fmap (.amount) differenceAmount,
                                                                             Beam.domain = domain,
                                                                             Beam.entityId = entityId,
                                                                             Beam.entityType = entityType,
                                                                             Beam.fulfilledTimestamp = fulfilledTimestamp,
                                                                             Beam.gstAmount = Kernel.Prelude.fmap (.amount) gstAmount,
                                                                             Beam.id = Kernel.Types.Id.getId id,
                                                                             Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                             Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
                                                                             Beam.message = message,
                                                                             Beam.networkFee = Kernel.Prelude.fmap (.amount) networkFee,
                                                                             Beam.networkOrderId = networkOrderId,
                                                                             Beam.currency = ((Kernel.Prelude.Just . (.currency))) orderAmount,
                                                                             Beam.orderAmount = (.amount) orderAmount,
                                                                             Beam.orderStatus = orderStatus,
                                                                             Beam.paymentAmount = (.amount) paymentAmount,
                                                                             Beam.paymentReference = paymentReference,
                                                                             Beam.paymentStatus = paymentStatus,
                                                                             Beam.paymentTransactionId = paymentTransactionId,
                                                                             Beam.receiverSubscriberId = receiverSubscriberId,
                                                                             Beam.reconCompletedAt = reconCompletedAt,
                                                                             Beam.reconInitiatedAt = reconInitiatedAt,
                                                                             Beam.sellerShare = (.amount) sellerShare,
                                                                             Beam.settledTimestamp = settledTimestamp,
                                                                             Beam.settlementAmount = Kernel.Prelude.fmap (.amount) settlementAmount,
                                                                             Beam.settlementDate = settlementDate,
                                                                             Beam.settlementRefNo = settlementRefNo,
                                                                             Beam.settlementStatus = settlementStatus,
                                                                             Beam.tcs = Kernel.Prelude.fmap (.amount) tcs,
                                                                             Beam.tds = Kernel.Prelude.fmap (.amount) tds,
                                                                             Beam.transactionId = transactionId,
                                                                             Beam.withholdingAmount = Kernel.Prelude.fmap (.amount) withholdingAmount,
                                                                             Beam.createdAt = createdAt,
                                                                             Beam.updatedAt = updatedAt}



