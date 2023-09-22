module Screens.PaymentHistoryScreen.Transformer where

import Prelude

import Common.Types.App (PaymentStatus(..), PaymentStatus(..))
import Data.Array (length, mapWithIndex, (!!))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Engineering.Helpers.Commons (convertUTCtoISC)
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.SubscriptionScreen.Transformer (decodeOfferPlan, getPromoConfig)
import Screens.Types (PromoConfig)
import Screens.Types as ST
import Services.API (FeeType(..), OfferEntity(..))
import Services.API as API

buildTransactionDetails :: API.HistoryEntryDetailsEntityV2Resp -> ST.TransactionInfo
buildTransactionDetails (API.HistoryEntryDetailsEntityV2Resp resp) =
    let (API.DriverFeeInfoEntity driverFee') = case (resp.driverFeeInfo !! 0) of
                                                  Just (API.DriverFeeInfoEntity driverFee) -> (API.DriverFeeInfoEntity driverFee)
                                                  Nothing -> dummyDriverFee
        autoPaySpecificKeys = do
          let planOfferData = decodeOfferPlan $ fromMaybe "" driverFee'.offerAndPlanDetails
          case (length resp.driverFeeInfo == 1) of
              true -> [
                  {
                    key : "TRIP_DATE",
                    title : getString TRIP_DATE,
                    val : if resp.feeType == AUTOPAY_REGISTRATION then "" else (convertUTCtoISC driverFee'.rideTakenOn "Do MMM, YYYY")
                  },
                  {
                    key : "PLAN",
                    title : getString PLAN,
                    val : planOfferData.plan
                  },
                  {
                    key : "NUMBER_OF_RIDES",
                    title : getString NUMBER_OF_RIDES,
                    val : if resp.feeType == AUTOPAY_REGISTRATION then "" else show driverFee'.totalRides
                  },
                  {key : "YOUR_EARNINGS",
                  title : getString YOUR_EARNINGS,
                  val : if resp.feeType == AUTOPAY_REGISTRATION then "" else "₹" <> show driverFee'.totalEarnings},
                --   {key : "FEE_BREAKUP",  -- TO BE ADDED
                --   title : getString FEE_BREAKUP,
                --   val : ""<> getString GST_INCLUDE},
                  {key : "OFFER",
                  title : getString OFFER,
                  val : planOfferData.offer}
                ]
              false -> []
 
        transactionDetails = {
            notificationStatus : driverFee'.autoPayStage,
            paymentStatus : if resp.feeType == AUTOPAY_PAYMENT then getAutoPayPaymentStatus driverFee'.autoPayStage else getInvoiceStatus driverFee'.paymentStatus,
            statusTime : case resp.executionAt of 
                            Just time -> convertUTCtoISC time "Do MMM YYYY, h:mm A"
                            Nothing -> "",
            isSplit : false,--(length resp.driverFeeInfo == 1) && driverFee'.isSplit, // NEED TO FIX LATER
            isAutoPayFailed : (length resp.driverFeeInfo == 1) && isJust driverFee'.autoPayStage && resp.feeType == MANUAL_PAYMENT,
            feeType : resp.feeType,
            details : [
              {
                key : "TXN_ID",
                title : getString TXN_ID,
                val : resp.invoiceId
              },
              {
                key : "AMOUNT_PAID",
                title : getString AMOUNT_PAID,
                val : "₹" <> show driverFee'.driverFeeAmount
              },
              { key : "PAYMENT_MODE",
                title : getString PAYMENT_MODE,
                val : if resp.feeType == API.MANUAL_PAYMENT then "UPI" else "UPI AutoPay"
              }
            ] <> autoPaySpecificKeys,
            manualSpecificDetails : do
                    case (length resp.driverFeeInfo /= 1) of
                        true -> mapWithIndex (\ ind (API.DriverFeeInfoEntity driverFee) ->  do
                            let planOfferData = decodeOfferPlan $ fromMaybe "" driverFee.offerAndPlanDetails
                            {
                                date : convertUTCtoISC driverFee.rideTakenOn "Do MMM, YYYY",
                                planType : planOfferData.plan,
                                offerApplied : (getPromoConfig [OfferEntity{title : Just planOfferData.offer, description : Nothing, tnc : Nothing}]) !! 0,
                                noOfRides : driverFee.totalRides,
                                totalEarningsOfDay : driverFee.totalEarnings,
                                dueAmount : driverFee.driverFeeAmount,
                                fareBreakup : (show driverFee.planAmount),
                                expanded : false,
                                isAutoPayFailed : isJust driverFee.autoPayStage && resp.feeType == MANUAL_PAYMENT,
                                isSplitPayment : false,--driverFee.isSplit, //NEED TO FIX LATER
                                id : show ind,
                                scheduledAt : if (resp.feeType == AUTOPAY_REGISTRATION && isJust  resp.executionAt) then Just (convertUTCtoISC (fromMaybe "" resp.executionAt) "Do MMM YYYY, h:mm A") else Nothing,
                                paymentMode : resp.feeType,
                                paymentStatus :  if resp.feeType == AUTOPAY_REGISTRATION then Just (getAutoPayStageData driverFee.autoPayStage) else Nothing
                            }
                            ) resp.driverFeeInfo
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

getAutoPayStageData :: Maybe API.AutopayPaymentStage -> String
getAutoPayStageData stage = 
  let status = fromMaybe API.NOTIFICATION_SCHEDULED stage
  in
  case status of 
    API.NOTIFICATION_SCHEDULED -> getString NOTIFICATION_SCHEDULED
    API.NOTIFICATION_ATTEMPTING -> getString NOTIFICATION_ATTEMPTING
    API.EXECUTION_SCHEDULED -> getString EXECUTION_SCHEDULED
    API.EXECUTION_ATTEMPTING -> getString EXECUTION_ATTEMPTING
    API.EXECUTION_SUCCESS -> getString EXECUTION_SUCCESS

getInvoiceStatus :: Maybe API.InvoiceStatus -> PaymentStatus
getInvoiceStatus status' = do
  let status = fromMaybe API.ACTIVE_INVOICE status'
  case status of 
    API.ACTIVE_INVOICE -> Pending
    API.SUCCESS -> Success
    API.FAILED -> Failed
    API.EXPIRED -> Failed
    API.INACTIVE -> Failed

dummyDriverFee :: API.DriverFeeInfoEntity
dummyDriverFee = 
  API.DriverFeeInfoEntity {
      autoPayStage : Just API.NOTIFICATION_SCHEDULED, -- AutopayPaymentStage NOTIFICATION_SCHEDULED | NOTIFICATION_ATTEMPTING | EXECUTION_SCHEDULED | EXECUTION_ATTEMPTING | EXECUTION_SUCCESS
      paymentStatus : Just API.ACTIVE_INVOICE, --InvoiceStatus ACTIVE_INVOICE (Pending) | SUCCESS | FAILED | EXPIRED | INACTIVE
      totalEarnings : 0.0,
      totalRides : 0,
      planAmount : 0.0,
      isSplit : false,
      offerAndPlanDetails : Just "",
      rideTakenOn : "",
      driverFeeAmount : 0.0
  }

