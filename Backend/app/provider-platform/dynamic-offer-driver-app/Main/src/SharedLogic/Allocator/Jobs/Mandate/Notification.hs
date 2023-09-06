module SharedLogic.Allocator.Jobs.Mandate.Notification (sendPDNNotificationToDriver) where

import qualified Data.Map.Strict as Map
import Domain.Types.DriverFee as DF
import Domain.Types.DriverInformation as DI
import Domain.Types.DriverPlan as DPlan
import Domain.Types.Mandate (Mandate)
import Domain.Types.Merchant.TransporterConfig
import qualified Domain.Types.Notification as NTF
import Domain.Types.Person as P
import Domain.Types.Plan as Plan
import qualified Kernel.External.Payment.Interface.Types as PaymentInterface
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Error
import Kernel.Types.Id (Id, cast)
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as APayments
import Lib.Scheduler
import SharedLogic.Allocator
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverPlan as QDP
import qualified Storage.Queries.Notification as QNTF
import qualified Tools.Payment as TPayment

sendPDNNotificationToDriver ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    MonadFlow m,
    EncFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  Job 'SendPDNNotificationToDriver ->
  m ExecutionResult
sendPDNNotificationToDriver Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  transporterConfig <- SCT.findByMerchantId jobData.merchantId >>= fromMaybeM (TransporterConfigNotFound jobData.merchantId.getId)
  let startTime = jobData.startTime
      endTime = jobData.endTime
      limit = transporterConfig.driverFeeMandateNotificationBatchSize
  driverFees <- QDF.findDriverFeeInRangeWithNotifcationNotSentAndStatus limit startTime endTime DF.PAYMENT_PENDING
  if null driverFees
    then return Complete --- TO DO :- here we will tigger execution scheduler ---
    else do
      let driverIdsWithPendingFee = driverFees <&> (.driverId)
      activeSubscribedDrivers <- QDI.findAllSubscribedByAutoPayStatusAndMerchantIdInDriverIds jobData.merchantId (Just DI.ACTIVE) driverIdsWithPendingFee True
      mandateIdAndDriverIdsToNotify <- mandateIdAndDriverId <$> QDP.findAllByDriverIdsAndPaymentMode (DI.driverId <$> activeSubscribedDrivers) Plan.AUTOPAY
      let driverInfoForPDNotification = mapDriverInfoForPDNNotification (Map.fromList mandateIdAndDriverIdsToNotify) driverFees
      now <- getCurrentTime
      for_ driverInfoForPDNotification $ \driverToNotify -> do
        notificationId <- generateGUID
        notificationShortId <- generateShortId
        req <- mkNotificationRequest driverToNotify notificationShortId.getShortId
        exec <- try @_ @SomeException $ withShortRetry (APayments.createNotificationService req (TPayment.mandateNotification jobData.merchantId))
        case exec of
          Left _ -> throwError (InternalError $ "Notification failed for driverFeeId" <> driverToNotify.driverFeeId.getId)
          Right res -> QNTF.create $ buildNotificationEntity res notificationId driverToNotify.driverFeeId driverToNotify.mandateId now
      ReSchedule <$> getRescheduledTime transporterConfig
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
    mkInfoForPDNNotification driverFee_ mandate_Id =
      DriverInfoForPDNotification
        { driverId = driverFee_.driverId,
          mandateId = mandate_Id,
          driverFeeId = driverFee_.id,
          amount = fromIntegral driverFee_.govtCharges + fromIntegral driverFee_.platformFee.fee + driverFee_.platformFee.cgst + driverFee_.platformFee.sgst
        }
    mkNotificationRequest driverInfoForPDN shortId = do
      now <- getCurrentTime
      return
        PaymentInterface.MandateNotificationReq
          { amount = driverInfoForPDN.amount,
            txnDate = now,
            mandateId = driverInfoForPDN.mandateId.getId, --- not sure regarding this m
            notificationId = shortId,
            description = "" --- to be decided ---
          }
    buildNotificationEntity response id_ driverFeeId mandateId now =
      NTF.Notification
        { id = id_,
          shortId = response.notificationId,
          sourceAmount = response.sourceInfo.sourceAmount,
          mandateId = mandateId,
          driverFeeId = driverFeeId,
          juspayProvidedId = response.juspayProvidedId,
          txnDate = response.sourceInfo.txnDate,
          providerName = response.providerName,
          notificationType = response.notificationType,
          description = response.description,
          status = response.status,
          dateCreated = response.dateCreated,
          lastUpdated = response.lastUpdated,
          createdAt = now,
          updatedAt = now
        }

data DriverInfoForPDNotification = DriverInfoForPDNotification
  { driverId :: Id Driver,
    mandateId :: Id Mandate,
    amount :: HighPrecMoney,
    driverFeeId :: Id DF.DriverFee
  }

getRescheduledTime :: MonadTime m => TransporterConfig -> m UTCTime
getRescheduledTime tc = addUTCTime tc.mandateNotificationRescheduleInterval <$> getCurrentTime
