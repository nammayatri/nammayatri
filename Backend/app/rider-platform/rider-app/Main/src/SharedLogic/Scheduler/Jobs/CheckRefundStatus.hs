{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.CheckRefundStatus where

import Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.Person as DP
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Payment.Storage.Queries.Refunds as QRefunds
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.JobScheduler
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.Person as QP
import Tools.Error
import qualified Tools.Payment as Payment

checkRefundStatusJob ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    ServiceFlow m r
  ) =>
  Job 'CheckRefundStatus ->
  m ExecutionResult
checkRefundStatusJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      refundId = (Id jobData.refundId)
      currentRetries = jobData.numberOfRetries
  refundEntry <- QRefunds.findById refundId >>= fromMaybeM (InvalidRequest $ "refund not found for id: " <> show refundId)
  paymentOrder <- QPaymentOrder.findById refundEntry.orderId >>= fromMaybeM (InvalidRequest $ "payment order not found for refund: " <> show refundId)
  frfsTicketBookingPayments <- QFRFSTicketBookingPayment.findAllByOrderId paymentOrder.id
  person <- QP.findById (cast paymentOrder.personId) >>= fromMaybeM (PersonNotFound paymentOrder.personId.getId)
  let orderStatusCall = Payment.orderStatus person.merchantId person.merchantOperatingCityId Nothing Payment.FRFSMultiModalBooking (Just person.id.getId) person.clientSdkVersion
      commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person person.id
  paymentStatusResp <- DPayment.orderStatusService commonPersonId refundEntry.orderId orderStatusCall
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  let matchingRefund = find (\refund -> refund.requestId == refundId.getId) paymentStatusResp.refunds
  case matchingRefund of
    Just refund -> do
      let newStatus = refund.status
      when (newStatus /= refundEntry.status) $ do
        DPayment.updateRefundStatus refund
        logInfo $ "Updated refund status for " <> refundId.getId <> " to " <> show newStatus

      let nonTerminalStatuses = [Payment.REFUND_PENDING, Payment.MANUAL_REVIEW]

      if newStatus `elem` nonTerminalStatuses
        then do
          let maxRetries = riderConfig.refundStatusUpdateRetries
          if currentRetries >= maxRetries
            then do
              logInfo $ "Maximum retries reached for refund " <> refundId.getId <> ". Stopping further checks."
            else do
              let nextSchedule = riderConfig.refundStatusUpdateInterval
                  newJobData =
                    CheckRefundStatusJobData
                      { refundId = refundId.getId,
                        numberOfRetries = currentRetries + 1
                      }
              createJobIn @_ @'CheckRefundStatus (Just person.merchantId) (Just person.merchantOperatingCityId) nextSchedule (newJobData :: CheckRefundStatusJobData)
              logInfo $ "Scheduled next refund status check for " <> refundId.getId <> " in " <> show nextSchedule <> " (retry " <> show (currentRetries + 1) <> "/" <> show maxRetries <> ")"
        else do
          logInfo $ "Refund " <> refundId.getId <> " completed with status: " <> show newStatus
          mapM_
            ( \frfsTicketBookingPayment -> do
                let refundStatus =
                      case newStatus of
                        Payment.REFUND_SUCCESS -> DFRFSTicketBookingPayment.REFUNDED
                        Payment.REFUND_FAILURE -> DFRFSTicketBookingPayment.REFUND_FAILED
                        _ -> DFRFSTicketBookingPayment.REFUND_PENDING
                QFRFSTicketBookingPayment.updateStatusByTicketBookingId refundStatus frfsTicketBookingPayment.frfsTicketBookingId
            )
            frfsTicketBookingPayments

      return Complete
    Nothing -> do
      logError $ "Refund " <> refundId.getId <> " not found in payment status response"
      return Complete
