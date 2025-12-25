{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.PaymentNudge
  ( sendSwitchPlanNudge,
    notifyPaymentFailure,
    notifyMandatePaused,
    notifyMandateCancelled,
    notifyPlanActivatedForDay,
  )
where

import Data.Ord
import qualified Data.Text as T
import qualified Domain.Action.Dashboard.Common as Common
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverPlan as DPlan
import qualified Domain.Types.Overlay as DOverlay
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import qualified Domain.Types.TransporterConfig as TC
import Kernel.Beam.Functions as B
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Payment.Interface.Types as Payments
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverFee (roundToHalf)
import qualified SharedLogic.Payment as SPayment
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.CachedQueries.PlanExtra as CQP
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QDP
import Tools.Error
import Tools.Notifications (mkOverlayReq, sendOverlay)

templateText :: Text -> Text
templateText txt = "{#" <> txt <> "#}"

switchPlanBudgeKey :: Text
switchPlanBudgeKey = "SWITCH_PLAN"

autopayPaymentFailedNudgeKey :: Text
autopayPaymentFailedNudgeKey = "PAYMENT_FAILED_AUTOPAY"

maunalPaymentFailedNudgeKey :: Text
maunalPaymentFailedNudgeKey = "PAYMENT_FAILED_MANUAL"

mandatePausedKey :: Text
mandatePausedKey = "MANDATE_PAUSED"

mandateCancelledKey :: Text
mandateCancelledKey = "MANDATE_CANCELLED"

planActivatedKey :: Text
planActivatedKey = "PLAN_ACTIVATED_FOR_DAY"

data PlanAmountEntity = PlanAmountEntity
  { finalAmount :: HighPrecMoney,
    planId :: Text
  }

sendSwitchPlanNudge ::
  ( EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    HasKafkaProducer r
  ) =>
  TC.TransporterConfig ->
  DI.DriverInformation ->
  Maybe Plan ->
  Maybe DPlan.DriverPlan ->
  Int ->
  ServiceNames ->
  m ()
sendSwitchPlanNudge transporterConfig driverInfo mbCurrPlan mbDriverPlan numRides serviceName = do
  whenJust mbCurrPlan $ \currPlan -> do
    driver <- QDP.findById (cast driverInfo.driverId) >>= fromMaybeM (PersonNotFound driverInfo.driverId.getId)
    let isFleetOwner = Common.checkFleetOwnerRole driver.role
    let mbIsFleetOwnerPlan = if isFleetOwner then Just True else Nothing
    if numRides == currPlan.freeRideCount + 1
      then notifyPlanActivatedForDay driver
      else case currPlan.planBaseAmount of
        PERRIDE_BASE amount -> do
          let currentTotal = fromIntegral numRides * amount
          availablePlans <- filterM (checkPlanEligible currPlan) =<< (CQP.findByMerchantOpCityIdAndPaymentModeWithServiceName transporterConfig.merchantOperatingCityId currPlan.paymentMode serviceName) (Just False) mbIsFleetOwnerPlan
          offeredAmountsEntity <- getOfferedAmount currentTotal driver `mapM` availablePlans

          unless (null offeredAmountsEntity) do
            let bestAmountEntity = minimumBy (comparing (.finalAmount)) offeredAmountsEntity
            when (currentTotal > bestAmountEntity.finalAmount) $
              switchPlanNudge driver numRides (currPlan.maxAmount - bestAmountEntity.finalAmount) bestAmountEntity.planId
        _ -> return ()
  where
    checkPlanEligible currPlan ePlan = return (ePlan.paymentMode == currPlan.paymentMode && ePlan.planBaseAmount /= currPlan.planBaseAmount)

    getOfferedAmount currentTotal driver plan = do
      now <- getCurrentTime
      let amount =
            case plan.planBaseAmount of
              DAILY_BASE x -> x
              PERRIDE_BASE x -> x * fromIntegral numRides
              _ -> currentTotal
      let mbMandateSetupDate = mbDriverPlan >>= (.mandateSetupDate)
      let mandateSetupDate = maybe now (\date -> if driverInfo.autoPayStatus == Just DI.ACTIVE then date else now) mbMandateSetupDate
      offersResp <- SPayment.offerListCache transporterConfig.merchantId driverInfo.driverId driver.merchantOperatingCityId plan.serviceName =<< makeOfferReq mandateSetupDate plan.paymentMode plan driver
      if null offersResp.offerResp
        then return (mkOfferedAmountsEntity amount plan.id)
        else do
          let bestOffer = minimumBy (comparing (.finalOrderAmount)) offersResp.offerResp
          return (mkOfferedAmountsEntity bestOffer.finalOrderAmount plan.id)

    mkOfferedAmountsEntity amount planId =
      PlanAmountEntity
        { finalAmount = amount,
          planId = planId.getId
        }

    makeOfferReq date paymentMode_ plan driver = do
      now <- getCurrentTime
      let offerOrder = Payment.OfferOrder {orderId = Nothing, amount = plan.maxAmount, currency = transporterConfig.currency}
          customerReq = Payment.OfferCustomer {customerId = driver.id.getId, email = driver.email, mobile = Nothing}
      return
        Payment.OfferListReq
          { order = offerOrder,
            customer = Just customerReq,
            planId = plan.id.getId,
            registrationDate = addUTCTime (fromIntegral transporterConfig.timeDiffFromUtc) date,
            dutyDate = addUTCTime (fromIntegral transporterConfig.timeDiffFromUtc) now,
            paymentMode = show paymentMode_,
            numOfRides = if paymentMode_ == AUTOPAY then 0 else -1,
            offerListingMetric = if transporterConfig.enableUdfForOffers then Just Payments.IS_VISIBLE else Nothing
          }

switchPlanNudge :: (CacheFlow m r, EsqDBFlow m r) => DP.Person -> Int -> HighPrecMoney -> Text -> m ()
switchPlanNudge driver numOfRides saveUpto planId = do
  mOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory driver.merchantOperatingCityId switchPlanBudgeKey (fromMaybe ENGLISH driver.language) Nothing Nothing Nothing
  whenJust mOverlay $ \overlay -> do
    let description =
          T.replace (templateText "numberOfRides") (show numOfRides)
            . T.replace (templateText "saveUpto") (show saveUpto)
            <$> overlay.description
    let endPoint = T.replace (templateText "planId") planId <$> overlay.endPoint
    let overlay' = overlay {DOverlay.description = description, DOverlay.endPoint = endPoint}
    sendOverlay driver.merchantOperatingCityId driver $ mkOverlayReq overlay'

notifyPaymentFailure ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  PaymentMode ->
  Maybe Text ->
  ServiceNames ->
  m ()
notifyPaymentFailure driverId paymentMode mbBankErrorCode serviceName = do
  driver <- B.runInReplica $ QDP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  dueDriverFees <- B.runInReplica $ QDF.findAllFeeByTypeServiceStatusAndDriver serviceName (cast driverId) [DF.RECURRING_INVOICE, DF.RECURRING_EXECUTION_INVOICE] [DF.PAYMENT_PENDING, DF.PAYMENT_OVERDUE]
  let totalDues = sum $ map (\dueInvoice -> roundToHalf dueInvoice.currency (dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) dueDriverFees

  let pnKey = if paymentMode == AUTOPAY then autopayPaymentFailedNudgeKey else maunalPaymentFailedNudgeKey
  mOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory driver.merchantOperatingCityId pnKey (fromMaybe ENGLISH driver.language) mbBankErrorCode Nothing Nothing
  whenJust mOverlay $ \overlay -> do
    let description = T.replace (templateText "dueAmount") (show totalDues) <$> overlay.description
    let okButtonText = T.replace (templateText "dueAmount") (show totalDues) <$> overlay.okButtonText
    let overlay' = overlay {DOverlay.description = description, DOverlay.okButtonText = okButtonText}
    sendOverlay driver.merchantOperatingCityId driver $ mkOverlayReq overlay'

notifyMandatePaused :: (CacheFlow m r, EsqDBFlow m r) => DP.Person -> m ()
notifyMandatePaused driver = do
  let pnKey = mandatePausedKey
  mOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory driver.merchantOperatingCityId pnKey (fromMaybe ENGLISH driver.language) Nothing Nothing Nothing
  whenJust mOverlay $ \overlay -> do
    sendOverlay driver.merchantOperatingCityId driver $ mkOverlayReq overlay

notifyMandateCancelled :: (CacheFlow m r, EsqDBFlow m r) => DP.Person -> m ()
notifyMandateCancelled driver = do
  let pnKey = mandateCancelledKey
  driverInfo <- QDI.findById driver.id >>= fromMaybeM DriverInfoNotFound
  whenJust driverInfo.autoPayStatus $ \autoPayStatus -> do
    when (autoPayStatus /= DI.PAUSED_PSP) $ do
      mOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory driver.merchantOperatingCityId pnKey (fromMaybe ENGLISH driver.language) Nothing Nothing Nothing
      whenJust mOverlay $ \overlay -> do
        sendOverlay driver.merchantOperatingCityId driver $ mkOverlayReq overlay

notifyPlanActivatedForDay :: (CacheFlow m r, EsqDBFlow m r) => DP.Person -> m ()
notifyPlanActivatedForDay driver = do
  let pnKey = planActivatedKey
  mOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory driver.merchantOperatingCityId pnKey (fromMaybe ENGLISH driver.language) Nothing Nothing Nothing
  whenJust mOverlay $ \overlay -> do
    sendOverlay driver.merchantOperatingCityId driver $ mkOverlayReq overlay
