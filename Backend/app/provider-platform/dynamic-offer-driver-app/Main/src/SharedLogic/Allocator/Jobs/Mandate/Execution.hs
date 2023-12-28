module SharedLogic.Allocator.Jobs.Mandate.Execution where

import Data.List (nubBy)
import qualified Data.Map.Strict as Map
import Domain.Types.DriverFee as DF
import Domain.Types.DriverInformation as DI
import Domain.Types.DriverPlan as DP
import qualified Domain.Types.Invoice as INV
import Domain.Types.Mandate (Mandate)
import Domain.Types.Merchant
import Domain.Types.Merchant.TransporterConfig
import qualified Domain.Types.Notification as NTF
import Domain.Types.Person as P
import Domain.Types.Plan as Plan
import qualified Kernel.External.Payment.Interface.Types as PaymentInterface
import qualified Kernel.External.Payment.Juspay.Types as JuspayTypes
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Error
import Kernel.Types.Id (Id, cast)
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as APayments
import Lib.Scheduler
import SharedLogic.Allocator
import SharedLogic.DriverFee (changeAutoPayFeesAndInvoicesForDriverFeesToManual, roundToHalf)
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
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
  ----- added measure duration for debugging -------
  (response, timetaken) <- measureDuration $ do
    let jobData = jobInfo.jobData
        merchantId = jobData.merchantId
        mbMerchantOpCityId = jobData.merchantOperatingCityId
        startTime = jobData.startTime
        endTime = jobData.endTime
    merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
    merchantOpCityId <- CQMOC.getMerchantOpCityId mbMerchantOpCityId merchant Nothing
    transporterConfig <- SCT.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    let limit = transporterConfig.driverFeeMandateExecutionBatchSize
    executionDate' <- getCurrentTime
    driverFees <- QDF.findDriverFeeInRangeWithOrderNotExecutedAndPending merchantId limit startTime endTime
    if null driverFees
      then return Complete
      else do
        let driverIdsWithPendingFee = driverFees <&> (.driverId)
        activeSubscribedDrivers <- QDI.findAllByAutoPayStatusAndMerchantIdInDriverIds merchantId (Just DI.ACTIVE) driverIdsWithPendingFee
        driverIdsAndDriverPlanToNotify <- driverIdAndDriverPlanTuple <$> QDP.findAllByDriverIdsAndPaymentMode (DI.driverId <$> activeSubscribedDrivers) AUTOPAY
        successfulNotifications <- nubBy (\x y -> x.driverFeeId == y.driverFeeId) <$> QNTF.findAllByDriverFeeIdAndStatus (driverFees <&> (.id)) JuspayTypes.SUCCESS --- notification_success instead of success in shared kernel---
        let mapDriverFeeById_ = Map.fromList (map (\driverFee_ -> (driverFee_.id, driverFee_)) driverFees)
            mapDriverPlanByDriverId = Map.fromList driverIdsAndDriverPlanToNotify
        driverExecutionRequests <- mapMaybe identity <$> sequence (mapExecutionRequestAndInvoice mapDriverFeeById_ mapDriverPlanByDriverId executionDate' successfulNotifications)
        changeAutoPayFeesAndInvoicesForDriverFeesToManual (driverFees <&> (.id)) (driverExecutionRequests <&> (.driverFee) <&> (.id))
        QDF.updateAutopayPaymentStageByIds (Just EXECUTION_ATTEMPTING) ((.driverFee.id) <$> driverExecutionRequests)
        for_ driverExecutionRequests $ \executionData -> do
          fork ("execution for driverFeeId : " <> executionData.driverFee.id.getId) $ do
            asyncExecutionCall executionData merchantId
        ReSchedule <$> getRescheduledTime transporterConfig
  logInfo ("duration of job " <> show timetaken)
  return response
  where
    driverIdAndDriverPlanTuple =
      mapMaybe
        ( \dplan ->
            case dplan.mandateId of
              Just mandateId -> Just (dplan.driverId, (dplan, mandateId))
              Nothing -> Nothing
        )
    mapExecutionRequestAndInvoice mapDriverFeeById mapDriverPlanByDriverId_ executionDate = do
      mapMaybe
        ( \notification -> do
            case mapDriverFeeById Map.!? NTF.driverFeeId notification of
              Just driverFee -> do
                let dplan = mapDriverPlanByDriverId_ Map.!? cast @P.Driver @P.Person (DF.driverId driverFee)
                buildExecutionRequestAndInvoice driverFee notification executionDate <$> dplan
              Nothing -> Nothing
        )

buildExecutionRequestAndInvoice ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  DF.DriverFee ->
  NTF.Notification ->
  UTCTime ->
  (DP.DriverPlan, Id Mandate) ->
  m (Maybe ExecutionData)
buildExecutionRequestAndInvoice driverFee notification executionDate (driverPlan, mandateId) = do
  invoice' <- listToMaybe <$> QINV.findLatestAutopayActiveByDriverFeeId driverFee.id
  case invoice' of
    Just invoice -> do
      let executionRequest =
            PaymentInterface.MandateExecutionReq
              { orderId = invoice.invoiceShortId,
                amount = roundToHalf $ (fromIntegral driverFee.govtCharges) + driverFee.platformFee.fee + driverFee.platformFee.cgst + driverFee.platformFee.sgst,
                customerId = driverFee.driverId.getId,
                notificationId = notification.shortId,
                mandateId = mandateId.getId,
                executionDate
              }
      return $
        Just
          ExecutionData
            { executionRequest,
              invoice,
              driverFee,
              driverPlan
            }
    Nothing -> return Nothing

getRescheduledTime :: MonadTime m => TransporterConfig -> m UTCTime
getRescheduledTime tc = addUTCTime tc.mandateExecutionRescheduleInterval <$> getCurrentTime

data ExecutionData = ExecutionData
  { executionRequest :: PaymentInterface.MandateExecutionReq,
    invoice :: INV.Invoice,
    driverFee :: DF.DriverFee,
    driverPlan :: DriverPlan
  }

asyncExecutionCall ::
  ( MonadFlow m,
    HasShortDurationRetryCfg r c,
    EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  ExecutionData ->
  Id Merchant ->
  m ()
asyncExecutionCall ExecutionData {..} merchantId = do
  driverFeeForExecution <- QDF.findById driverFee.id
  if (driverFeeForExecution <&> (.status)) == Just PAYMENT_PENDING && (driverFeeForExecution <&> (.feeType)) == Just DF.RECURRING_EXECUTION_INVOICE
    then do
      exec <- try @_ @SomeException $ withShortRetry (APayments.createExecutionService (executionRequest, invoice.id.getId) (cast merchantId) (TPayment.mandateExecution merchantId))
      case exec of
        Left err -> do
          QINV.updateInvoiceStatusByDriverFeeIdsAndMbPaymentMode INV.INACTIVE [driverFee.id] Nothing
          QDF.updateAutoPayToManual driverFee.id
          logError ("Execution failed for driverFeeId : " <> invoice.driverFeeId.getId <> " error : " <> show err)
        Right _ -> pure ()
    else do
      QINV.updateInvoiceStatusByDriverFeeIdsAndMbPaymentMode INV.INACTIVE [driverFee.id] Nothing
