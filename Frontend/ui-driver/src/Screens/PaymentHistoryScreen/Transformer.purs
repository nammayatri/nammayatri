module Screens.PaymentHistoryScreen.Transformer where

import Prelude

import Common.Types.App (PaymentStatus(..))
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.Types as ST
import Services.API (FeeType(..))
import Services.API as API

buildTransactionDetails :: API.HistoryEntryDetailsEntityV2Resp -> ST.TransactionInfo
buildTransactionDetails (API.HistoryEntryDetailsEntityV2Resp resp) =
    let (API.DriverFeeInfoEntity driverFee') = case (resp.driverFeeInfo !! 0) of
                                                  Just (API.DriverFeeInfoEntity driverFee) -> (API.DriverFeeInfoEntity driverFee)
                                                  Nothing -> dummyDriverFee
        autoPaySpecificKeys = do
          case (length resp.driverFeeInfo == 1) of
              true -> [
                  {
                    key : "TRIP_DATE",
                    title : getString TRIP_DATE,
                    val : "trip date"--TO BE FIXED(convertUTCtoISC driverFee.ridesTakenDate "Do MMM, YYYY")
                  },
                  {
                    key : "PLAN",
                    title : getString PLAN,
                    val : "details.invoiceId"
                  },
                  {
                    key : "NUMBER_OF_RIDES",
                    title : getString NUMBER_OF_RIDES,
                    val : show driverFee'.totalRides
                  },
                  {key : "YOUR_EARNINGS",
                  title : getString YOUR_EARNINGS,
                  val : "₹" <> show driverFee'.totalEarnings},
                  {key : "FEE_BREAKUP",
                  title : getString FEE_BREAKUP,
                  val : "details.invoiceId"},
                  {key : "OFFER",
                  title : getString OFFER,
                  val : "details.invoiceId"}
                ]
              false -> []
        manualDetails = map (\(API.DriverFeeInfoEntity driverFee) -> {
                                    tripDate : "",
                                    plan : "",
                                    amount : driverFee.planAmount
                                  }) resp.driverFeeInfo
 
        transactionDetails = {
            notificationStatus : driverFee'.autoPayStage,
            paymentStatus : if resp.feeType == AUTOPAY_PAYMENT then getAutoPayPaymentStatus driverFee'.autoPayStage else getInvoiceStatus driverFee'.paymentStatus,
            statusTime : fromMaybe "" resp.executionAt,
            details : [
              {
                key : "TXN_ID",
                title : getString TXN_ID,
                val : resp.invoiceId
              },
              {
                key : "AMOUNT_PAID",
                title : getString AMOUNT_PAID,
                val : "₹" <> show resp.amount
              },
              { key : "PAYMENT_MODE",
                title : getString PAYMENT_MODE,
                val : if resp.feeType == API.MANUAL_PAYMENT then "UPI" else "UPI AutoPay"
              }
            ] <> autoPaySpecificKeys,
            manualSpecificDetails : map (\(API.DriverFeeInfoEntity driverFee) ->  {
                                        date : fromMaybe "" resp.createdAt,
                                        planType : fromMaybe "" driverFee.offerAndPlanDetails,
                                        offerApplied : {
                                                        title : Just "Freedom Offer: 76% off APPLIED",
                                                        offerDescription : Nothing,
                                                        isGradient : true,
                                                        gradient : ["#FFE7C2", "#FFFFFF", "#DDFFEB"],
                                                        hasImage : true,
                                                        imageURL : "ny_ic_discount,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_discount.png",
                                                        addedFromUI : true
                                                        },
                                        noOfRides : driverFee.totalRides,
                                        totalEarningsOfDay : driverFee.totalEarnings,
                                        dueAmount : driverFee.planAmount,
                                        fareBreakup : show driverFee.planAmount,
                                        expanded : false,
                                        isAutoPayFailed : driverFee.autoPayStage == Just API.EXECUTION_ATTEMPTING && resp.feeType /= AUTOPAY_PAYMENT,
                                        isSplitPayment : driverFee.isSplit
                                      }
                                       ) resp.driverFeeInfo
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
      offerAndPlanDetails : Just ""
  }