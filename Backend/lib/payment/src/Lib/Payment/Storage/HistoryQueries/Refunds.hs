module Lib.Payment.Storage.HistoryQueries.Refunds
  ( create,
    findAllByOrderId,
    findById,
    findByShortId,
    updateIsApiCallSuccess,
    updateRefundStatus,
    updateRefundsEntryByResponse,
    updateRefundsEntryByStripeResponse,
    findLatestByOrderId,
  )
where

import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import Lib.Payment.Domain.Types.Common
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Domain.Types.Refunds as DRefunds
import qualified Lib.Payment.Refunds.History as RefundsHistory
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.Refunds as QRefunds

-- ---------------------------------------------------------------------------
-- Queries with history
-- ---------------------------------------------------------------------------

type BeamFlow m r = (FinanceBeamFlow.BeamFlow m r, PaymentBeamFlow.BeamFlow m r)

create ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  Id MerchantOperatingCity ->
  DRefunds.Refunds ->
  Maybe Text ->
  m ()
create merchantOpCityId refunds mbAction = do
  let historyMessage =
        "Create refunds: "
          <> RefundsHistory.getStatusMessage refunds.status
          <> maybe "" ("; action: " <>) mbAction
  QRefunds.create refunds
  RefundsHistory.recordRefundsHistory merchantOpCityId Nothing refunds.status (Just historyMessage) refunds

findAllByOrderId ::
  PaymentBeamFlow.BeamFlow m r =>
  ShortId DPaymentOrder.PaymentOrder ->
  m [DRefunds.Refunds]
findAllByOrderId = QRefunds.findAllByOrderId

findById ::
  PaymentBeamFlow.BeamFlow m r =>
  Id DRefunds.Refunds ->
  m (Maybe DRefunds.Refunds)
findById = QRefunds.findById

findByShortId ::
  PaymentBeamFlow.BeamFlow m r =>
  ShortId DRefunds.Refunds ->
  m (Maybe DRefunds.Refunds)
findByShortId = QRefunds.findByShortId

updateIsApiCallSuccess ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  Id MerchantOperatingCity ->
  Maybe Bool ->
  DRefunds.Refunds ->
  Maybe Text ->
  m ()
updateIsApiCallSuccess merchantOpCityId isApiCallSuccess refunds mbAction = do
  let historyMessage =
        "Update is api call status: "
          <> show isApiCallSuccess
          <> maybe "" ("; action: " <>) mbAction
  QRefunds.updateIsApiCallSuccess isApiCallSuccess refunds.id
  RefundsHistory.recordRefundsHistory merchantOpCityId (Just refunds.status) refunds.status (Just historyMessage) refunds

updateRefundStatus ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  Id MerchantOperatingCity ->
  Payment.RefundStatus ->
  DRefunds.Refunds ->
  Maybe Text ->
  m ()
updateRefundStatus merchantOpCityId newStatus refunds mbAction = do
  let historyMessage =
        "Update refund status: "
          <> show refunds.status
          <> " -> "
          <> show newStatus
          <> maybe "" ("; action: " <>) mbAction
  QRefunds.updateStatus newStatus refunds.id
  RefundsHistory.recordRefundsHistory merchantOpCityId (Just refunds.status) newStatus (Just historyMessage) refunds

updateRefundsEntryByResponse ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  Id MerchantOperatingCity ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Payment.RefundStatus ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe HighPrecMoney ->
  DRefunds.Refunds ->
  Maybe Text ->
  m ()
updateRefundsEntryByResponse merchantOpCityId initiatedBy idAssignedByServiceProvider mbErrorMessage mbErrorCode status arn completedAt actualRefundedAmount refunds mbAction = do
  let historyMessage =
        "Update refunds entry by response: "
          <> RefundsHistory.getStatusMessage status
          <> maybe "" ("; action: " <>) mbAction
          <> maybe "" ("; error code: " <>) mbErrorCode
          <> maybe "" ("; error message: " <>) mbErrorMessage
  QRefunds.updateRefundsEntryByResponse initiatedBy idAssignedByServiceProvider mbErrorMessage mbErrorCode status arn completedAt actualRefundedAmount refunds.id
  RefundsHistory.recordRefundsHistory merchantOpCityId (Just refunds.status) status (Just historyMessage) refunds

updateRefundsEntryByStripeResponse ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  Id MerchantOperatingCity ->
  Maybe Text ->
  Maybe Text ->
  Payment.RefundStatus ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe UTCTime ->
  Maybe HighPrecMoney ->
  DRefunds.Refunds ->
  Maybe Text ->
  m ()
updateRefundsEntryByStripeResponse merchantOpCityId idAssignedByServiceProvider mbErrorCode status arn referenceType isApiCallSuccess completedAt actualRefundedAmount refunds mbAction = do
  let historyMessage =
        "Update refunds entry by Stripe response: "
          <> RefundsHistory.getStatusMessage status
          <> maybe "" ("; action: " <>) mbAction
          <> maybe "" ("; error code: " <>) mbErrorCode
          <> maybe "" (("; is api call success: " <>) . show) isApiCallSuccess
  QRefunds.updateRefundsEntryByStripeResponse idAssignedByServiceProvider mbErrorCode status arn referenceType isApiCallSuccess completedAt actualRefundedAmount refunds.id
  RefundsHistory.recordRefundsHistory merchantOpCityId (Just refunds.status) status (Just historyMessage) refunds

findLatestByOrderId ::
  PaymentBeamFlow.BeamFlow m r =>
  ShortId DPaymentOrder.PaymentOrder ->
  m (Maybe DRefunds.Refunds)
findLatestByOrderId = QRefunds.findLatestByOrderId
