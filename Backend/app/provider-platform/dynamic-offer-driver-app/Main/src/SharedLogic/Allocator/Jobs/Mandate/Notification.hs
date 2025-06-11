module SharedLogic.Allocator.Jobs.Mandate.Notification (sendPDNNotificationToDriver) where

import Control.Monad.Extra (mapMaybeM)
import qualified Data.Map as M
import qualified Data.Map.Strict as Map
import Domain.Types.DriverFee as DF
import Domain.Types.DriverInformation as DI
import Domain.Types.DriverPlan as DPlan
import qualified Domain.Types.Invoice as INV
import Domain.Types.Mandate (Mandate)
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified Domain.Types.Notification as NTF
import Domain.Types.Person as P
import Domain.Types.Plan as Plan
import qualified Domain.Types.SubscriptionConfig as DSC
import Domain.Types.TransporterConfig
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Types as PaymentInterface
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Types.Id (Id, cast)
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as APayments
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import SharedLogic.DriverFee (changeAutoPayFeesAndInvoicesForDriverFeesToManual, jobDuplicationPreventionKey, roundToHalf, setIsNotificationSchedulerRunningKey)
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverPlan as QDP
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.Notification as QNTF
import qualified Storage.Queries.Person as QP
import Tools.Error
import qualified Tools.Payment as TPayment

sendPDNNotificationToDriver ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    MonadFlow m,
    EncFlow m r,
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasKafkaProducer r
  ) =>
  Job 'SendPDNNotificationToDriver ->
  m ExecutionResult
sendPDNNotificationToDriver Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  ----- added measure duration for debugging -------
  (response, timetaken) <- measureDuration $ do
    let jobData = jobInfo.jobData
        merchantId = jobData.merchantId
        mbMerchantOpCityId = jobData.merchantOperatingCityId
        startTime = jobData.startTime
        endTime = jobData.endTime
        retryCount = fromMaybe 0 jobData.retryCount
        serviceName = fromMaybe YATRI_SUBSCRIPTION jobData.serviceName
    merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
    merchantOpCityId <- CQMOC.getMerchantOpCityId mbMerchantOpCityId merchant Nothing
    setIsNotificationSchedulerRunningKey startTime endTime merchantOpCityId serviceName True
    transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    subscriptionConfig <-
      CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId Nothing serviceName
        >>= fromMaybeM (NoSubscriptionConfigForService merchantOpCityId.getId $ show serviceName)
    let limit = transporterConfig.driverFeeMandateNotificationBatchSize
    driverFees <- QDF.findDriverFeeInRangeWithNotifcationNotSentServiceNameAndStatus merchantId merchantOpCityId limit startTime endTime retryCount DF.PAYMENT_PENDING serviceName
    if null driverFees
      then do
        if retryCount >= transporterConfig.notificationRetryCountThreshold
          then do
            setIsNotificationSchedulerRunningKey startTime endTime merchantOpCityId serviceName False
            driverFeesPostRetries <- QDF.findDriverFeeInRangeWithNotifcationNotSentServiceNameAndStatus merchantId merchantOpCityId limit startTime endTime retryCount DF.PAYMENT_PENDING serviceName
            mapM_ handleNotificationFailureAfterRetiresEnd (driverFeesPostRetries <&> (.id))
            let jobDataT :: Text = show jobData
            hashedJobData <- getHash jobDataT
            duplicationKey <- Hedis.setNxExpire (jobDuplicationPreventionKey hashedJobData "Notification") (3600 * 12) True --- 12 hours expiry time  for duplication prevention  key
            when duplicationKey $ do
              scheduleJobs transporterConfig startTime endTime merchantId merchantOpCityId serviceName
            return Complete
          else do
            let dfCalculationJobTs = 2 ^ retryCount * transporterConfig.notificationRetryTimeGap
            createJobIn @_ @'SendPDNNotificationToDriver (Just merchant.id) (Just merchantOpCityId) dfCalculationJobTs $
              SendPDNNotificationToDriverJobData
                { merchantId = merchantId,
                  merchantOperatingCityId = Just merchantOpCityId,
                  startTime = startTime,
                  endTime = endTime,
                  retryCount = Just $ retryCount + 1,
                  serviceName = Just serviceName
                }
            return Complete
      else do
        let driverIdsWithPendingFee = driverFees <&> (.driverId)
        mandateIdAndDriverIdsToNotify <- mandateIdAndDriverId <$> QDP.findAllByDriverIdsPaymentModeAndServiceName driverIdsWithPendingFee Plan.AUTOPAY serviceName (Just DI.ACTIVE)
        let driverInfoForPDNotification = mapDriverInfoForPDNNotification (Map.fromList mandateIdAndDriverIdsToNotify) driverFees
        changeAutoPayFeesAndInvoicesForDriverFeesToManual (driverFees <&> (.id)) (driverInfoForPDNotification <&> (.driverFeeId))
        driverFeeToBeNotified <-
          mapMaybeM
            ( \pdnNoticationEntity -> do
                invoice' <- listToMaybe <$> QINV.findLatestAutopayActiveByDriverFeeId pdnNoticationEntity.driverFeeId
                case invoice' of
                  Just _ -> return $ Just pdnNoticationEntity
                  Nothing -> do
                    QINV.updateInvoiceStatusByDriverFeeIdsAndMbPaymentMode INV.INACTIVE [pdnNoticationEntity.driverFeeId] Nothing
                    QDF.updateAutoPayToManual pdnNoticationEntity.driverFeeId
                    logError ("Active autopay invoice not found for driverFeeId" <> pdnNoticationEntity.driverFeeId.getId)
                    return Nothing
            )
            driverInfoForPDNotification
        QDF.updateAutopayPaymentStageAndRetryCountByIds (Just NOTIFICATION_ATTEMPTING) retryCount (map (.driverFeeId) driverFeeToBeNotified)
        for_ driverFeeToBeNotified $ \driverToNotify -> do
          fork ("Notification call for driverFeeId : " <> driverToNotify.driverFeeId.getId) $ do
            sendAsyncNotification driverToNotify merchantId merchantOpCityId subscriptionConfig
        ReSchedule <$> getRescheduledTime transporterConfig
  logWarning ("duration of job " <> show timetaken)
  return response
  where
    mandateIdAndDriverId =
      mapMaybe
        ( \dplan ->
            case DPlan.mandateId dplan of
              Just mandateId_ -> Just (dplan.driverId, mandateId_)
              Nothing -> Nothing
        )
    mapDriverInfoForPDNNotification mapMandateByDriverId =
      mapMaybe
        ( \driverFee -> mkInfoForPDNNotification driverFee <$> (mapMandateByDriverId Map.!? (cast @P.Driver @P.Person driverFee.driverId))
        )
    mkInfoForPDNNotification driverFee_ mandateId_ =
      DriverInfoForPDNotification
        { driverId = driverFee_.driverId,
          mandateId = mandateId_,
          driverFeeId = driverFee_.id,
          amount = roundToHalf driverFee_.currency $ driverFee_.govtCharges + driverFee_.platformFee.fee + driverFee_.platformFee.cgst + driverFee_.platformFee.sgst
        }

data DriverInfoForPDNotification = DriverInfoForPDNotification
  { driverId :: Id Driver,
    mandateId :: Id Mandate,
    amount :: HighPrecMoney,
    driverFeeId :: Id DF.DriverFee
  }

getRescheduledTime :: MonadTime m => TransporterConfig -> m UTCTime
getRescheduledTime tc = addUTCTime tc.mandateNotificationRescheduleInterval <$> getCurrentTime

scheduleJobs ::
  (CacheFlow m r, EsqDBFlow m r, HasField "schedulerType" r SchedulerType, JobCreatorEnv r) =>
  TransporterConfig ->
  UTCTime ->
  UTCTime ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Plan.ServiceNames ->
  m ()
scheduleJobs transporterConfig startTime endTime merchantId merchantOpCityId serviceName = do
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let dfExecutionTime = transporterConfig.driverAutoPayExecutionTime
      dfStatusCheckTime = transporterConfig.orderAndNotificationStatusCheckTime
      dfNotificationTime = transporterConfig.driverAutoPayNotificationTime
      fallBackExecutionTime = transporterConfig.driverAutoPayExecutionTimeFallBack
      fallBackOrderStatusCheckTime = transporterConfig.orderAndNotificationStatusCheckFallBackTime
  let normalFlowExecutionTime = addUTCTime (dfExecutionTime + dfNotificationTime) endTime
  let normalFlowOrderStatusTime = addUTCTime (dfStatusCheckTime + dfNotificationTime) endTime
  let dfCalculationJobTs = max (diffUTCTime normalFlowExecutionTime now) fallBackExecutionTime
  let orderAndNotificationJobTs = max (diffUTCTime normalFlowOrderStatusTime now) fallBackOrderStatusCheckTime
  createJobIn @_ @'MandateExecution (Just merchantId) (Just merchantOpCityId) dfCalculationJobTs $
    MandateExecutionInfo
      { merchantId = merchantId,
        merchantOperatingCityId = Just merchantOpCityId,
        startTime = startTime,
        endTime = endTime,
        serviceName = Just serviceName
      }
  createJobIn @_ @'OrderAndNotificationStatusUpdate (Just merchantId) (Just merchantOpCityId) orderAndNotificationJobTs $
    OrderAndNotificationStatusUpdateJobData
      { merchantId = merchantId,
        merchantOperatingCityId = Just merchantOpCityId
      }

sendAsyncNotification ::
  ( MonadFlow m,
    HasShortDurationRetryCfg r c,
    EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasKafkaProducer r
  ) =>
  DriverInfoForPDNotification ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  DSC.SubscriptionConfig ->
  m ()
sendAsyncNotification driverToNotify merchantId merchantOperatingCityId subscriptionConfig = do
  notificationId <- generateGUID
  notificationShortId <- generateShortId
  now <- getCurrentTime
  req <- mkNotificationRequest driverToNotify notificationShortId.getShortId
  driver <- QP.findById driverToNotify.driverId >>= fromMaybeM (PersonDoesNotExist driverToNotify.driverId.getId)
  QNTF.create $ buildNotificationEntity notificationId req driverToNotify.driverFeeId driverToNotify.mandateId now
  paymentServiceName <- TPayment.decidePaymentServiceForRecurring subscriptionConfig.paymentServiceName driver.id driver.merchantOperatingCityId subscriptionConfig.serviceName
  exec <- try @_ @SomeException $ withShortRetry (APayments.createNotificationService req (TPayment.mandateNotification merchantId merchantOperatingCityId paymentServiceName (Just driver.id.getId)))
  case exec of
    Left err -> do
      QINV.updateInvoiceStatusByDriverFeeIdsAndMbPaymentMode INV.INACTIVE [driverToNotify.driverFeeId] Nothing
      QDF.updateAutoPayToManual driverToNotify.driverFeeId
      QNTF.updateNotificationStatusAndResponseInfoById PaymentInterface.NOTIFICATION_FAILURE Nothing Nothing notificationId
      logError ("Notification failed for driverFeeId : " <> driverToNotify.driverFeeId.getId <> " error : " <> show err)
    Right res -> do
      QNTF.updateNotificationResponseById notificationId res
  where
    buildNotificationEntity id_ req driverFeeId mandateId now =
      NTF.Notification
        { id = id_,
          shortId = req.notificationId,
          sourceAmount = req.amount,
          mandateId = mandateId,
          driverFeeId = driverFeeId,
          juspayProvidedId = "Unknown",
          txnDate = now,
          providerName = Nothing,
          notificationType = Nothing,
          description = req.description,
          status = PaymentInterface.NOTIFICATION_CREATED,
          merchantOperatingCityId = merchantOperatingCityId,
          dateCreated = now,
          lastUpdated = now,
          lastStatusCheckedAt = Nothing,
          responseCode = Nothing,
          responseMessage = Nothing,
          createdAt = now,
          updatedAt = now,
          merchantId = Just merchantId
        }
    mkNotificationRequest driverInfoForPDN shortId = do
      now <- getCurrentTime
      return
        PaymentInterface.MandateNotificationReq
          { amount = driverInfoForPDN.amount,
            txnDate = addUTCTime (3600 * 24) now,
            mandateId = driverInfoForPDN.mandateId.getId, --- not sure regarding this m
            notificationId = shortId,
            description = "Driver fee mandate notification"
          }

handleNotificationFailureAfterRetiresEnd :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DF.DriverFee -> m ()
handleNotificationFailureAfterRetiresEnd driverFeeId = do
  QINV.updateInvoiceStatusByDriverFeeIdsAndMbPaymentMode INV.INACTIVE [driverFeeId] Nothing
  QDF.updateAutoPayToManual driverFeeId
  QDF.updateAutopayPaymentStageByIds (Just NOTIFICATION_ATTEMPTING) [driverFeeId]
