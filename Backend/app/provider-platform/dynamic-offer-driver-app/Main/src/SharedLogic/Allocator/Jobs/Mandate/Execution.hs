module SharedLogic.Allocator.Jobs.Mandate.Execution where

import qualified Data.Map.Strict as Map
import Domain.Types.DriverFee as DF
import Domain.Types.DriverInformation as DI
import qualified Domain.Types.Invoice as INV
import Domain.Types.Mandate (Mandate)
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.TransporterConfig
import qualified Domain.Types.Notification as NTF
import Domain.Types.Person as P
import Domain.Types.Plan as Plan
import qualified Kernel.External.Payment.Interface.Types as PaymentInterface
import qualified Kernel.External.Payment.Juspay.Types as JuspayTypes
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Error
import Kernel.Types.Id (Id (Id), cast)
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as APayments
import Lib.Scheduler
import SharedLogic.Allocator
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverPlan as QDP
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.Notification as QNTF
import qualified Tools.Payment as TPayment

startMandateExecutionForDriver ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EncFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  Job 'MandateExecution ->
  m ExecutionResult
startMandateExecutionForDriver Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  transporterConfig <- SCT.findByMerchantId jobData.merchantId >>= fromMaybeM (TransporterConfigNotFound jobData.merchantId.getId)
  let startTime = jobData.startTime
      endTime = jobData.endTime
      limit = transporterConfig.driverFeeMandateExecutionBatchSize
  executionDate' <- addUTCTime transporterConfig.driverAutoPayExecutionTime <$> getCurrentTime
  driverFees <- QDF.findDriverFeeInRangeWithOrderNotExecutedAndPending limit startTime endTime
  if null driverFees
    then return Complete
    else do
      let driverIdsWithPendingFee = driverFees <&> (.driverId)
      activeSubscribedDrivers <- QDI.findAllSubscribedByAutoPayStatusAndMerchantIdInDriverIds jobData.merchantId (Just DI.ACTIVE) driverIdsWithPendingFee True
      mandateIdAndDriverIdsToNotify <- mandateIdAndDriverId <$> QDP.findAllByDriverIdsAndPaymentMode (DI.driverId <$> activeSubscribedDrivers) Plan.AUTOPAY
      successfulNotifications <- QNTF.findAllByDriverFeeIdAndStatus (driverFees <&> (.id)) JuspayTypes.SUCCESS
      let mapDriverFeeById_ = Map.fromList (map (\driverFee_ -> (driverFee_.id, driverFee_)) driverFees)
          mapMandateIdByDriverId_ = Map.fromList mandateIdAndDriverIdsToNotify
      driverRequestAndInvoiceToExecute <- sequence $ mapExecutionRequestAndInvoice mapDriverFeeById_ mapMandateIdByDriverId_ executionDate' jobData.merchantId successfulNotifications
      for_ driverRequestAndInvoiceToExecute $ \(executionRequest, invoice) -> do
        --- To do need to call offer apply api also here ---
        exec <- try @_ @SomeException $ withShortRetry (APayments.createExecutionService (executionRequest, invoice.id.getId) (TPayment.mandateExecution jobData.merchantId))
        case exec of
          Left _ -> throwError (InternalError $ "Execution failed for driverFeeId" <> invoice.driverFeeId.getId)
          Right _ -> QINV.create invoice
      ReSchedule <$> getRescheduledTime transporterConfig
  where
    mandateIdAndDriverId =
      mapMaybe
        ( \dplan ->
            case dplan.mandateId of
              Just mandateId_ -> Just (dplan.driverId, mandateId_)
              Nothing -> Nothing
        )
    mapExecutionRequestAndInvoice mapDriverFeeById mapMandateIdByDriverId executionDate merchantId =
      mapMaybe
        ( \notification ->
            case mapDriverFeeById Map.!? NTF.driverFeeId notification of
              Just driverFee -> buildExecutionRequestAndInvoice driverFee notification executionDate merchantId <$> (mapMandateIdByDriverId Map.!? cast @P.Driver @P.Person (DF.driverId driverFee))
              Nothing -> Nothing
        )

buildExecutionRequestAndInvoice ::
  ( MonadFlow m
  ) =>
  DF.DriverFee ->
  NTF.Notification ->
  UTCTime ->
  Id Merchant ->
  Id Mandate ->
  m (PaymentInterface.MandateExecutionReq, INV.Invoice)
buildExecutionRequestAndInvoice driverFee notification executionDate merchantId mandateId = do
  invoice <- mkInvoiceAgainstDriverFee driverFee
  return
    ( PaymentInterface.MandateExecutionReq
        { orderId = invoice.invoiceShortId,
          merchantId = merchantId.getId, --- check this once here it is same merchantId as our system or something else ---
          amount = fromIntegral driverFee.govtCharges + fromIntegral driverFee.platformFee.fee + driverFee.platformFee.cgst + driverFee.platformFee.sgst,
          customerId = driverFee.driverId.getId,
          notificationId = notification.id.getId,
          mandateId = mandateId.getId,
          executionDate
        },
      invoice
    )

mkInvoiceAgainstDriverFee ::
  ( MonadFlow m
  ) =>
  DF.DriverFee ->
  m INV.Invoice
mkInvoiceAgainstDriverFee driverFee = do
  invoiceId <- generateGUID
  shortId <- generateShortId
  now <- getCurrentTime
  return $
    INV.Invoice
      { id = Id invoiceId,
        invoiceShortId = shortId.getShortId,
        driverFeeId = driverFee.id,
        invoiceStatus = INV.ACTIVE_INVOICE,
        maxMandateAmount = Nothing,
        updatedAt = now,
        createdAt = now
      }

getRescheduledTime :: MonadTime m => TransporterConfig -> m UTCTime
getRescheduledTime tc = addUTCTime tc.mandateExecutionRescheduleInterval <$> getCurrentTime
