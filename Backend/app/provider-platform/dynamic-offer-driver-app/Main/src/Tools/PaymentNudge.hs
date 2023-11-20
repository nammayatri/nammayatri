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
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverPlan as DPlan
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.TransporterConfig as TC
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import Kernel.Beam.Functions as B
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Payment.Interface.Types as Payments
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Payment as SPayment
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.CachedQueries.Plan as CQP
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.Person as QDP
import Tools.Notifications (sendOverlay)

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

roundToHalf :: HighPrecMoney -> HighPrecMoney
roundToHalf x = fromInteger (round (x * 2)) / 2

data PlanAmountEntity = PlanAmountEntity
  { finalAmount :: HighPrecMoney,
    planId :: Text
  }

sendSwitchPlanNudge ::
  ( EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  TC.TransporterConfig ->
  DI.DriverInformation ->
  Maybe Plan ->
  Maybe DPlan.DriverPlan ->
  Int ->
  m ()
sendSwitchPlanNudge transporterConfig driverInfo mbCurrPlan mbDriverPlan numRides = do
  whenJust mbCurrPlan $ \currPlan -> do
    driver <- QDP.findById (cast driverInfo.driverId) >>= fromMaybeM (PersonNotFound driverInfo.driverId.getId)
    if numRides == currPlan.freeRideCount + 1
      then notifyPlanActivatedForDay driver.id driver.merchantId driver.deviceToken driver.language
      else case currPlan.planBaseAmount of
        PERRIDE_BASE amount -> do
          let currentTotal = fromIntegral numRides * amount
          availablePlans <- filterM (checkPlanEligible currPlan) =<< (CQP.findByMerchantIdAndPaymentMode transporterConfig.merchantId currPlan.paymentMode)
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
      offersResp <- SPayment.offerListCache transporterConfig.merchantId driver.merchantOperatingCityId =<< makeOfferReq mandateSetupDate plan.paymentMode plan driver
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
      let offerOrder = Payment.OfferOrder {orderId = Nothing, amount = plan.maxAmount, currency = Payment.INR}
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
  mOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdf driver.merchantOperatingCityId switchPlanBudgeKey (fromMaybe ENGLISH driver.language) Nothing
  whenJust mOverlay $ \overlay -> do
    let description =
          T.replace (templateText "numberOfRides") (show numOfRides)
            . T.replace (templateText "saveUpto") (show saveUpto)
            <$> overlay.description
    let endPoint = T.replace (templateText "planId") planId <$> overlay.endPoint
    sendOverlay driver.merchantOperatingCityId driver.id driver.deviceToken overlay.title description overlay.imageUrl overlay.okButtonText overlay.cancelButtonText overlay.actions overlay.link endPoint overlay.method overlay.reqBody overlay.delay overlay.contactSupportNumber overlay.toastMessage overlay.secondaryActions overlay.socialMediaLinks

notifyPaymentFailure :: (CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> PaymentMode -> Maybe Text -> m ()
notifyPaymentFailure driverId paymentMode mbBankErrorCode = do
  driver <- B.runInReplica $ QDP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  dueDriverFees <- B.runInReplica $ QDF.findAllPendingAndDueDriverFeeByDriverId (cast driverId)
  let totalDues = sum $ map (\dueInvoice -> roundToHalf (fromIntegral dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) dueDriverFees

  let pnKey = if paymentMode == AUTOPAY then autopayPaymentFailedNudgeKey else maunalPaymentFailedNudgeKey
  mOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdf driver.merchantOperatingCityId pnKey (fromMaybe ENGLISH driver.language) mbBankErrorCode
  whenJust mOverlay $ \overlay -> do
    let description = T.replace (templateText "dueAmount") (show totalDues) <$> overlay.description
    let okButtonText = T.replace (templateText "dueAmount") (show totalDues) <$> overlay.okButtonText
    logDebug $ show driverId <> ":updated Value of description - " <> show description
    logDebug $ show driverId <> ":updated Value of okButtonText - " <> show okButtonText
    sendOverlay driver.merchantOperatingCityId driver.id driver.deviceToken overlay.title description overlay.imageUrl okButtonText overlay.cancelButtonText overlay.actions overlay.link overlay.endPoint overlay.method overlay.reqBody overlay.delay overlay.contactSupportNumber overlay.toastMessage overlay.secondaryActions overlay.socialMediaLinks

notifyMandatePaused :: (CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> Id DM.Merchant -> Maybe FCM.FCMRecipientToken -> Maybe Language -> m ()
notifyMandatePaused driverId _merchantId deviceToken language = do
  let pnKey = mandatePausedKey
  driver <- B.runInReplica $ QDP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  mOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdf driver.merchantOperatingCityId pnKey (fromMaybe ENGLISH language) Nothing
  whenJust mOverlay $ \overlay -> do
    sendOverlay driver.merchantOperatingCityId driverId deviceToken overlay.title overlay.description overlay.imageUrl overlay.okButtonText overlay.cancelButtonText overlay.actions overlay.link overlay.endPoint overlay.method overlay.reqBody overlay.delay overlay.contactSupportNumber overlay.toastMessage overlay.secondaryActions overlay.socialMediaLinks

notifyMandateCancelled :: (CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> Id DM.Merchant -> Maybe FCM.FCMRecipientToken -> Maybe Language -> m ()
notifyMandateCancelled driverId _merchantId deviceToken language = do
  let pnKey = mandateCancelledKey
  driver <- B.runInReplica $ QDP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  mOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdf driver.merchantOperatingCityId pnKey (fromMaybe ENGLISH language) Nothing
  whenJust mOverlay $ \overlay -> do
    sendOverlay driver.merchantOperatingCityId driverId deviceToken overlay.title overlay.description overlay.imageUrl overlay.okButtonText overlay.cancelButtonText overlay.actions overlay.link overlay.endPoint overlay.method overlay.reqBody overlay.delay overlay.contactSupportNumber overlay.toastMessage overlay.secondaryActions overlay.socialMediaLinks

notifyPlanActivatedForDay :: (CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> Id DM.Merchant -> Maybe FCM.FCMRecipientToken -> Maybe Language -> m ()
notifyPlanActivatedForDay driverId _merchantId deviceToken language = do
  let pnKey = planActivatedKey
  driver <- B.runInReplica $ QDP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  mOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdf driver.merchantOperatingCityId pnKey (fromMaybe ENGLISH language) Nothing
  whenJust mOverlay $ \overlay -> do
    sendOverlay driver.merchantOperatingCityId driverId deviceToken overlay.title overlay.description overlay.imageUrl overlay.okButtonText overlay.cancelButtonText overlay.actions overlay.link overlay.endPoint overlay.method overlay.reqBody overlay.delay overlay.contactSupportNumber overlay.toastMessage overlay.secondaryActions overlay.socialMediaLinks
