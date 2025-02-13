module SharedLogic.Allocator.Jobs.Mandate.Execution where

import qualified Control.Monad.Catch as C
import Data.List (nubBy)
import qualified Data.Map.Strict as Map
import Domain.Types.DriverFee as DF
import Domain.Types.DriverInformation as DI
import Domain.Types.DriverPlan as DP
import qualified Domain.Types.Invoice as INV
import Domain.Types.Mandate (Mandate)
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Notification as NTF
import Domain.Types.Person as P
import Domain.Types.Plan as Plan
import qualified Domain.Types.SubscriptionConfig as DSC
import Domain.Types.TransporterConfig
import Kernel.Beam.Functions as B
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
import SharedLogic.Payment
import Storage.Beam.Payment ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverPlan as QDP
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.Notification as QNTF
import qualified Storage.Queries.VendorFee as QVF
import Tools.Error
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
        serviceName = fromMaybe YATRI_SUBSCRIPTION jobData.serviceName
    merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
    merchantOpCityId <- CQMOC.getMerchantOpCityId mbMerchantOpCityId merchant Nothing
    transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    subscriptionConfig <-
      CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId serviceName
        >>= fromMaybeM (NoSubscriptionConfigForService merchantOpCityId.getId $ show serviceName)
    let limit = transporterConfig.driverFeeMandateExecutionBatchSize
    executionDate' <- getCurrentTime
    driverFees <- QDF.findDriverFeeInRangeWithOrderNotExecutedAndPendingByServiceName merchantId merchantOpCityId limit startTime endTime serviceName
    if null driverFees
      then return Complete
      else do
        let driverIdsWithPendingFee = driverFees <&> (.driverId)
        driverIdsAndDriverPlanToNotify <- driverIdAndDriverPlanTuple <$> QDP.findAllByDriverIdsPaymentModeAndServiceName driverIdsWithPendingFee AUTOPAY serviceName (Just DI.ACTIVE)
        successfulNotifications <- nubBy (\x y -> x.driverFeeId == y.driverFeeId) <$> QNTF.findAllByDriverFeeIdAndStatus (driverFees <&> (.id)) JuspayTypes.SUCCESS --- notification_success instead of success in shared kernel---
        let mapDriverFeeById_ = Map.fromList (map (\driverFee_ -> (driverFee_.id, driverFee_)) driverFees)
            mapDriverPlanByDriverId = Map.fromList driverIdsAndDriverPlanToNotify
        driverExecutionRequests <- mapMaybe identity <$> sequence (mapExecutionRequestAndInvoice mapDriverFeeById_ mapDriverPlanByDriverId executionDate' subscriptionConfig successfulNotifications)
        changeAutoPayFeesAndInvoicesForDriverFeesToManual (driverFees <&> (.id)) (driverExecutionRequests <&> (.driverFee) <&> (.id))
        QDF.updateAutopayPaymentStageByIds (Just EXECUTION_ATTEMPTING) ((.driverFee.id) <$> driverExecutionRequests)
        flip C.catchAll (\e -> C.mask_ $ logError $ "Driver fee execution for merchant id " <> merchantId.getId <> " failed. Error: " <> show e) $ do
          for_ driverExecutionRequests $ \executionData -> do
            fork ("execution for driverFeeId : " <> executionData.driverFee.id.getId) $ do
              asyncExecutionCall executionData merchantId merchantOpCityId
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
    mapExecutionRequestAndInvoice mapDriverFeeById mapDriverPlanByDriverId_ executionDate subscriptionConfig = do
      mapMaybe
        ( \notification -> do
            case mapDriverFeeById Map.!? NTF.driverFeeId notification of
              Just driverFee -> do
                let dplan = mapDriverPlanByDriverId_ Map.!? cast @P.Driver @P.Person (DF.driverId driverFee)
                buildExecutionRequestAndInvoice driverFee notification executionDate subscriptionConfig <$> dplan
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
  DSC.SubscriptionConfig ->
  (DP.DriverPlan, Id Mandate) ->
  m (Maybe ExecutionData)
buildExecutionRequestAndInvoice driverFee notification executionDate subscriptionConfig (driverPlan, mandateId) = do
  invoice' <- listToMaybe <$> QINV.findLatestAutopayActiveByDriverFeeId driverFee.id
  case invoice' of
    Just invoice -> do
      let splitEnabled = subscriptionConfig.isVendorSplitEnabled == Just True
      vendorFees' <- if splitEnabled then B.runInReplica $ QVF.findAllByDriverFeeId driverFee.id else pure []
      let vendorFees = map roundVendorFee vendorFees'

      splitSettlementDetails <- if splitEnabled then mkSplitSettlementDetails vendorFees (roundToHalf driverFee.currency (driverFee.govtCharges + driverFee.platformFee.fee + driverFee.platformFee.cgst + driverFee.platformFee.sgst)) else pure Nothing
      let executionRequest =
            PaymentInterface.MandateExecutionReq
              { orderId = invoice.invoiceShortId,
                amount = roundToHalf driverFee.currency $ driverFee.govtCharges + driverFee.platformFee.fee + driverFee.platformFee.cgst + driverFee.platformFee.sgst,
                customerId = driverFee.driverId.getId,
                notificationId = notification.shortId,
                mandateId = mandateId.getId,
                executionDate,
                splitSettlementDetails = splitSettlementDetails
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
  Id DMOC.MerchantOperatingCity ->
  m ()
asyncExecutionCall ExecutionData {..} merchantId merchantOperatingCityId = do
  driverFeeForExecution <- QDF.findById driverFee.id
  let serviceName = invoice.serviceName
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOperatingCityId serviceName
      >>= fromMaybeM (NoSubscriptionConfigForService merchantOperatingCityId.getId $ show serviceName)
  let paymentService = subscriptionConfig.paymentServiceName
  if (driverFeeForExecution <&> (.status)) == Just PAYMENT_PENDING && (driverFeeForExecution <&> (.feeType)) == Just DF.RECURRING_EXECUTION_INVOICE
    then do
      exec <- try @_ @SomeException (APayments.createExecutionService (executionRequest, invoice.id.getId) (cast merchantId) (Just $ cast merchantOperatingCityId) (TPayment.mandateExecution merchantId merchantOperatingCityId paymentService))
      case exec of
        Left err -> do
          QINV.updateInvoiceStatusByDriverFeeIdsAndMbPaymentMode INV.INACTIVE [driverFee.id] Nothing
          QDF.updateAutoPayToManual driverFee.id
          logError ("Execution failed for driverFeeId : " <> invoice.driverFeeId.getId <> " error : " <> show err)
        Right _ -> pure ()
    else do
      QINV.updateInvoiceStatusByDriverFeeIdsAndMbPaymentMode INV.INACTIVE [driverFee.id] Nothing
