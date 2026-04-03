{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Lib.Payment.Storage.Queries.PayoutRequest where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Lib.Payment.Domain.Types.PayoutRequest
import qualified Lib.Payment.Storage.Beam.PayoutRequest as Beam
import qualified Kernel.Prelude
import qualified Lib.Payment.Domain.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Sequelize as Se



create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> m ())
create = createWithKV
createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest] -> m ())
createMany = traverse_ create
findByEntity :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
                (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Lib.Payment.Domain.Types.Common.EntityName -> m (Maybe Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest))
findByEntity entityId entityName = do findOneWithKV [Se.And [Se.Is Beam.entityId $ Se.Eq entityId, Se.Is Beam.entityName $ Se.Eq entityName]]
findById :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
            (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> m (Maybe Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
updateCashDetailsById :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
                         (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> m ())
updateCashDetailsById cashMarkedById cashMarkedByName cashMarkedAt id = do {_now <- getCurrentTime;
                                                                            updateOneWithKV [Se.Set Beam.cashMarkedById cashMarkedById,
                                                                                             Se.Set Beam.cashMarkedByName cashMarkedByName,
                                                                                             Se.Set Beam.cashMarkedAt cashMarkedAt,
                                                                                             Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]}
updatePayoutTransactionIdById :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
                                 (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> m ())
updatePayoutTransactionIdById payoutTransactionId id = do {_now <- getCurrentTime;
                                                           updateOneWithKV [Se.Set Beam.payoutTransactionId payoutTransactionId, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]}
updateRetryCountById :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> m ())
updateRetryCountById retryCount id = do {_now <- getCurrentTime; updateOneWithKV [Se.Set Beam.retryCount retryCount, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]}
updateStatusById :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
                    (Lib.Payment.Domain.Types.PayoutRequest.PayoutRequestStatus -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> m ())
updateStatusById status id = do {_now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]}
updateStatusWithReasonById :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
                              (Lib.Payment.Domain.Types.PayoutRequest.PayoutRequestStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> m ())
updateStatusWithReasonById status failureReason id = do {_now <- getCurrentTime;
                                                         updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.failureReason failureReason, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]}
findByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
                    (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> m (Maybe Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest {..}) = do {_now <- getCurrentTime;
                                                                                     updateWithKV [Se.Set Beam.amount amount,
                                                                                                   Se.Set Beam.beneficiaryId beneficiaryId,
                                                                                                   Se.Set Beam.cashMarkedAt cashMarkedAt,
                                                                                                   Se.Set Beam.cashMarkedById cashMarkedById,
                                                                                                   Se.Set Beam.cashMarkedByName cashMarkedByName,
                                                                                                   Se.Set Beam.city city,
                                                                                                   Se.Set Beam.coverageFrom coverageFrom,
                                                                                                   Se.Set Beam.coverageTo coverageTo,
                                                                                                   Se.Set Beam.customerEmail customerEmail,
                                                                                                   Se.Set Beam.customerName customerName,
                                                                                                   Se.Set Beam.customerPhone customerPhone,
                                                                                                   Se.Set Beam.customerVpa customerVpa,
                                                                                                   Se.Set Beam.entityId entityId,
                                                                                                   Se.Set Beam.entityName entityName,
                                                                                                   Se.Set Beam.entityRefId entityRefId,
                                                                                                   Se.Set Beam.expectedCreditTime expectedCreditTime,
                                                                                                   Se.Set Beam.failureReason failureReason,
                                                                                                   Se.Set Beam.merchantId merchantId,
                                                                                                   Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
                                                                                                   Se.Set Beam.orderType orderType,
                                                                                                   Se.Set Beam.payoutFee payoutFee,
                                                                                                   Se.Set Beam.payoutTransactionId payoutTransactionId,
                                                                                                   Se.Set Beam.payoutType payoutType,
                                                                                                   Se.Set Beam.remark remark,
                                                                                                   Se.Set Beam.retryCount retryCount,
                                                                                                   Se.Set Beam.scheduledAt scheduledAt,
                                                                                                   Se.Set Beam.status status,
                                                                                                   Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



instance FromTType' Beam.PayoutRequest Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest
    where fromTType' (Beam.PayoutRequestT {..}) = do pure $ Just Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest{amount = amount,
                                                                                                                      beneficiaryId = beneficiaryId,
                                                                                                                      cashMarkedAt = cashMarkedAt,
                                                                                                                      cashMarkedById = cashMarkedById,
                                                                                                                      cashMarkedByName = cashMarkedByName,
                                                                                                                      city = city,
                                                                                                                      coverageFrom = coverageFrom,
                                                                                                                      coverageTo = coverageTo,
                                                                                                                      createdAt = createdAt,
                                                                                                                      customerEmail = customerEmail,
                                                                                                                      customerName = customerName,
                                                                                                                      customerPhone = customerPhone,
                                                                                                                      customerVpa = customerVpa,
                                                                                                                      entityId = entityId,
                                                                                                                      entityName = entityName,
                                                                                                                      entityRefId = entityRefId,
                                                                                                                      expectedCreditTime = expectedCreditTime,
                                                                                                                      failureReason = failureReason,
                                                                                                                      id = Kernel.Types.Id.Id id,
                                                                                                                      merchantId = merchantId,
                                                                                                                      merchantOperatingCityId = merchantOperatingCityId,
                                                                                                                      orderType = orderType,
                                                                                                                      payoutFee = payoutFee,
                                                                                                                      payoutTransactionId = payoutTransactionId,
                                                                                                                      payoutType = payoutType,
                                                                                                                      remark = remark,
                                                                                                                      retryCount = retryCount,
                                                                                                                      scheduledAt = scheduledAt,
                                                                                                                      status = status,
                                                                                                                      updatedAt = updatedAt}
instance ToTType' Beam.PayoutRequest Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest
    where toTType' (Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest {..}) = do Beam.PayoutRequestT{Beam.amount = amount,
                                                                                                        Beam.beneficiaryId = beneficiaryId,
                                                                                                        Beam.cashMarkedAt = cashMarkedAt,
                                                                                                        Beam.cashMarkedById = cashMarkedById,
                                                                                                        Beam.cashMarkedByName = cashMarkedByName,
                                                                                                        Beam.city = city,
                                                                                                        Beam.coverageFrom = coverageFrom,
                                                                                                        Beam.coverageTo = coverageTo,
                                                                                                        Beam.createdAt = createdAt,
                                                                                                        Beam.customerEmail = customerEmail,
                                                                                                        Beam.customerName = customerName,
                                                                                                        Beam.customerPhone = customerPhone,
                                                                                                        Beam.customerVpa = customerVpa,
                                                                                                        Beam.entityId = entityId,
                                                                                                        Beam.entityName = entityName,
                                                                                                        Beam.entityRefId = entityRefId,
                                                                                                        Beam.expectedCreditTime = expectedCreditTime,
                                                                                                        Beam.failureReason = failureReason,
                                                                                                        Beam.id = Kernel.Types.Id.getId id,
                                                                                                        Beam.merchantId = merchantId,
                                                                                                        Beam.merchantOperatingCityId = merchantOperatingCityId,
                                                                                                        Beam.orderType = orderType,
                                                                                                        Beam.payoutFee = payoutFee,
                                                                                                        Beam.payoutTransactionId = payoutTransactionId,
                                                                                                        Beam.payoutType = payoutType,
                                                                                                        Beam.remark = remark,
                                                                                                        Beam.retryCount = retryCount,
                                                                                                        Beam.scheduledAt = scheduledAt,
                                                                                                        Beam.status = status,
                                                                                                        Beam.updatedAt = updatedAt}



