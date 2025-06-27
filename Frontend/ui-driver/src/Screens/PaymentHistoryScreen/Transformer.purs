{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PaymentHistoryScreen.Transformer where

import Prelude

import Domain.Payments
import Data.Array (length, mapWithIndex, (!!), filter)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Engineering.Helpers.Commons (convertUTCtoISC)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Types (GradientConfig)
import Screens.SubscriptionScreen.Transformer (decodeOfferPlan, getFeeBreakup, getPromoConfig)
import Screens.Types (PromoConfig)
import Screens.Types as ST
import Services.API (FeeType(..), OfferEntity(..))
import Services.API as API
import Data.Int as INT
import Data.Number as NUM
import Styles.Colors as Color
import Engineering.Helpers.Utils (getFixedTwoDecimals)

buildTransactionDetails :: API.HistoryEntryDetailsEntityV2Resp -> Array GradientConfig -> Boolean -> String -> ST.TransactionInfo
buildTransactionDetails (API.HistoryEntryDetailsEntityV2Resp resp) gradientConfig showFeeBreakup gstPercentage =
    let filteredDriverFees = if (resp.feeType == AUTOPAY_REGISTRATION && length resp.driverFeeInfo > 1)  then filter (\ (API.DriverFeeInfoEntity driverFee) -> driverFee.driverFeeAmount > 0.0) resp.driverFeeInfo else resp.driverFeeInfo
        (API.DriverFeeInfoEntity driverFee') = case (filteredDriverFees !! 0) of
                                                  Just (API.DriverFeeInfoEntity driverFee) -> (API.DriverFeeInfoEntity driverFee)
                                                  Nothing -> dummyDriverFee
        statusTime = if resp.feeType == AUTOPAY_PAYMENT then resp.executionAt else resp.createdAt
        boothCharges specialZoneRideCount totalSpecialZoneCharges = case specialZoneRideCount,totalSpecialZoneCharges of
                      Just count, Just charges | count /= 0 && charges /= 0.0 -> Just $ show count <> " " <> getString (if count > 1 then RIDES else RIDE) <> " x €" <> getFixedTwoDecimals (charges / INT.toNumber count) <> " " <> getString GST_INCLUDE
                      _, _ -> Nothing
        fareBreakup fee = getFeeBreakup fee.maxRidesEligibleForCharge fee.coinDiscountAmount (max 0.0 (fee.driverFeeAmount - (fromMaybe 0.0 fee.totalSpecialZoneCharges))) fee.totalRides
        autoPaySpecificKeys = do
          let offerAndPlanDetails = fromMaybe "" driverFee'.offerAndPlanDetails
              planOfferData = decodeOfferPlan $ offerAndPlanDetails
              rideNumberPrefix = if driverFee'.totalRides < 10 then "0" else ""
          case (length filteredDriverFees == 1) of
              true -> [
                  {
                    key : "TRIP_DATE",
                    title : getString TRIP_DATE,
                    val : if resp.feeType == AUTOPAY_REGISTRATION then "" else (convertUTCtoISC driverFee'.rideTakenOn "Do MMM YYYY")
                  },
                  {
                    key : "PLAN",
                    title : getString PLAN,
                    val : planOfferData.plan
                  },
                  {
                    key : "NUMBER_OF_RIDES",
                    title : getString NUMBER_OF_RIDES,
                    val : if resp.feeType == AUTOPAY_REGISTRATION then "" else rideNumberPrefix <> show (driverFee'.totalRides + fromMaybe 0 driverFee'.specialZoneRideCount)
                  },
                  {
                    key : "YOUR_EARNINGS",
                    title : getString YOUR_EARNINGS,
                    val : if resp.feeType == AUTOPAY_REGISTRATION then "" else "€" <> getFixedTwoDecimals driverFee'.totalEarnings
                  },
                  {
                    key : "FEE_BREAKUP",
                    title : getString FEE_BREAKUP,
                    val : if (resp.feeType == AUTOPAY_REGISTRATION || not showFeeBreakup) then "" else fareBreakup driverFee'
                  },
                  {
                    key : "BOOTH_CHARGES",
                    title : getString BOOTH_CHARGES,
                    val : fromMaybe "" $ boothCharges driverFee'.specialZoneRideCount driverFee'.totalSpecialZoneCharges
                  },
                  {
                    key : "OFFER",
                    title : getString OFFER,
                    val : planOfferData.offer
                  },
                  {
                    key : "COIN_DISCOUNT",
                    title : "Discount",
                    val : getFixedTwoDecimals $ case driverFee'.isCoinCleared of
                                                true -> driverFee'.driverFeeAmount
                                                false -> fromMaybe 0.0 driverFee'.coinDiscountAmount
                  },
                  {
                    key : "GST",
                    title : getString $ GST_WITH_PERCENTAGE $ maybe gstPercentage (\percentage -> show $ 100.0 * percentage) driverFee'.gstPercentage,
                    val : case driverFee'.gst of
                            Just gst -> if gst > 0.0 then getFixedTwoDecimals gst else ""
                            Nothing -> ""
                  }
                ]
              false -> []

        transactionDetails = {
            notificationStatus : driverFee'.autoPayStage,
            paymentStatus : if resp.feeType == AUTOPAY_PAYMENT then getAutoPayPaymentStatus driverFee'.autoPayStage else getInvoiceStatus driverFee'.paymentStatus,
            statusTime : case statusTime of
                            Just time -> convertUTCtoISC time "Do MMM YYYY, h:mm A"
                            Nothing -> "",
            isSplit : (length filteredDriverFees == 1) && driverFee'.isSplit,
            isAutoPayFailed : (length filteredDriverFees == 1) && isJust driverFee'.autoPayStage && resp.feeType == MANUAL_PAYMENT,
            feeType : resp.feeType,
            numOfDriverFee : length filteredDriverFees,
            isCoinCleared : driverFee'.isCoinCleared,
            isCoinDiscountApplied : isJust driverFee'.coinDiscountAmount &&  driverFee'.coinDiscountAmount /= Just 0.0,
            details : [
              {
                key : "TXN_ID",
                title : getString TXN_ID,
                val : resp.invoiceId
              },
              {
                key : "AMOUNT_PAID",
                title : getString TOTAL_AMOUNT,
                val : "€" <> getFixedTwoDecimals resp.amount
              },
              { key : "PAYMENT_MODE",
                title : getString PAYMENT_MODE,
                val : if resp.feeType == API.MANUAL_PAYMENT then "UPI" else "UPI AutoPay"
              }
            ] <> autoPaySpecificKeys,
            manualSpecificDetails : do
                    case (length filteredDriverFees /= 1) of
                        true -> mapWithIndex (\ ind (API.DriverFeeInfoEntity driverFee) ->  do
                            let offerAndPlanDetails = fromMaybe "" driverFee.offerAndPlanDetails
                                planOfferData = decodeOfferPlan offerAndPlanDetails
                                autoPayStageData = getAutoPayStageData driverFee.autoPayStage driverFee.isCoinCleared
                                noOfRides = driverFee.totalRides + fromMaybe 0 driverFee.specialZoneRideCount
                            {
                                date : convertUTCtoISC driverFee.rideTakenOn "Do MMM YYYY",
                                planType : planOfferData.plan,
                                offerApplied : (getPromoConfig [OfferEntity{title : Just planOfferData.offer, description : Nothing, tnc : Nothing, offerId : "", gradient : Nothing}] gradientConfig) !! 0,
                                noOfRides : noOfRides,
                                totalEarningsOfDay : driverFee.totalEarnings,
                                dueAmount : driverFee.driverFeeAmount,
                                fareBreakup : fareBreakup driverFee,
                                expanded : false,
                                isAutoPayFailed : isJust driverFee.autoPayStage && resp.feeType == MANUAL_PAYMENT,
                                isSplitPayment : driverFee.isSplit,
                                id : show ind,
                                isDue : false,
                                scheduledAt : if (resp.feeType == AUTOPAY_REGISTRATION && isJust  resp.executionAt) then Just (convertUTCtoISC (fromMaybe "" resp.executionAt) "Do MMM YYYY, h:mm A") else Nothing,
                                paymentMode : resp.feeType,
                                paymentStatus :  if resp.feeType == AUTOPAY_REGISTRATION then Just (autoPayStageData.stage) else Nothing,
                                boothCharges : boothCharges driverFee.specialZoneRideCount driverFee.totalSpecialZoneCharges,
                                amountPaidByYatriCoins : case driverFee.isCoinCleared of
                                                            true -> Just driverFee.driverFeeAmount
                                                            false -> driverFee.coinDiscountAmount
                            }
                            ) filteredDriverFees
                        false -> []
        }
        in transactionDetails

getAutoPayPaymentStatus :: Maybe API.AutopayPaymentStage -> PaymentStatus
getAutoPayPaymentStatus status' = do
  let status = fromMaybe API.NOTIFICATION_SCHEDULED status'
  case status of
    API.NOTIFICATION_SCHEDULED -> Scheduled
    API.NOTIFICATION_ATTEMPTING -> Scheduled
    API.EXECUTION_SCHEDULED -> Scheduled
    API.EXECUTION_ATTEMPTING -> Pending
    API.EXECUTION_SUCCESS -> Success
    _ -> Failed

getAutoPayStageData :: Maybe API.AutopayPaymentStage -> Boolean -> {stage :: String, statusTimeDesc :: String}
getAutoPayStageData stage isCoinCleared =
  let status = fromMaybe API.NOTIFICATION_SCHEDULED stage
  in
  case status, isCoinCleared of
    _ , true -> { stage : getString PAYMENT_SUCCESSFUL, statusTimeDesc : getString TRANSACTION_DEBITED_ON }
    API.NOTIFICATION_SCHEDULED, _ -> { stage : getString NOTIFICATION_SCHEDULED, statusTimeDesc : getString SCHEDULED_AT }
    API.NOTIFICATION_ATTEMPTING, _ -> { stage : getString NOTIFICATION_ATTEMPTING, statusTimeDesc : getString SCHEDULED_AT }
    API.EXECUTION_SCHEDULED, _ -> { stage : getString EXECUTION_SCHEDULED, statusTimeDesc : getString SCHEDULED_AT }
    API.EXECUTION_ATTEMPTING, _ -> { stage : getString EXECUTION_ATTEMPTING, statusTimeDesc : getString SCHEDULED_AT }
    API.EXECUTION_SUCCESS, _ -> { stage : getString EXECUTION_SUCCESS, statusTimeDesc : getString TRANSACTION_DEBITED_ON }
    API.NOTIFICATION_FAILED, _ -> { stage : getString NOTIFICATION_FAILED, statusTimeDesc : getString TRANSACTION_ATTEMPTED_ON }
    API.EXECUTION_FAILED, _ -> { stage : getString EXECUTION_FAILED, statusTimeDesc : getString TRANSACTION_ATTEMPTED_ON }
    _, _ -> { stage : getString EXECUTION_ATTEMPTING, statusTimeDesc : getString TRANSACTION_ATTEMPTED_ON }

getInvoiceStatus :: Maybe API.InvoiceStatus -> PaymentStatus
getInvoiceStatus status' = do
  let status = fromMaybe API.ACTIVE_INVOICE status'
  case status of
    API.ACTIVE_INVOICE -> Pending
    API.SUCCESS -> Success
    API.FAILED -> Failed
    API.EXPIRED -> Failed
    API.INACTIVE -> Failed
    API.CLEARED_BY_YATRI_COINS -> Success

dummyDriverFee :: API.DriverFeeInfoEntity
dummyDriverFee =
  API.DriverFeeInfoEntity {
      autoPayStage : Just API.NOTIFICATION_SCHEDULED, -- AutopayPaymentStage NOTIFICATION_SCHEDULED | NOTIFICATION_ATTEMPTING | EXECUTION_SCHEDULED | EXECUTION_ATTEMPTING | EXECUTION_SUCCESS
      paymentStatus : Just API.ACTIVE_INVOICE, --InvoiceStatus ACTIVE_INVOICE (Pending) | SUCCESS | FAILED | EXPIRED | INACTIVE | CLEARED_BY_YATRI_COINS
      totalEarnings : 0.0,
      totalRides : 0,
      planAmount : 0.0,
      isSplit : false,
      offerAndPlanDetails : Just "",
      rideTakenOn : "",
      driverFeeAmount : 0.0,
      specialZoneRideCount : Nothing,
      totalSpecialZoneCharges : Nothing,
      maxRidesEligibleForCharge : Nothing,
      isCoinCleared : false,
      coinDiscountAmount : Nothing,
      gst : Nothing,
      gstPercentage : Nothing
  }

coinsOfferConfig :: String -> PromoConfig
coinsOfferConfig amount =
    {
    title : Just $ amount <> " " <> getString DISCOUNT_POINTS_SMALL,
    isGradient : true,
    gradient : [Color.yellowCoinGradient1, Color.yellowCoinGradient2],
    hasImage : false,
    imageURL : "",
    offerDescription : Nothing,
    addedFromUI : true,
    isPaidByYatriCoins : true
    }
