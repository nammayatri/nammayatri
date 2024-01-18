{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Transformer.OnUpdate
  ( buildOnUpdateReqV2,
  )
where

import Beckn.OnDemand.Utils.OnUpdate as Utils
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType as Event
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as CU
import qualified Data.List as List
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.OnUpdate as OU
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.DecimalValue (DecimalValue)
import Kernel.Utils.Common
import SharedLogic.FareCalculator (mkBreakupList)
import Tools.Error

data DriverInfo = DriverInfo
  { mobileNumber :: Text,
    name :: Text,
    tags :: Maybe [Spec.TagGroup]
  }

mkFulFillment ::
  (MonadFlow m, EncFlow m r) =>
  Maybe SP.Person ->
  DRide.Ride ->
  DBooking.Booking ->
  Maybe DVeh.Vehicle ->
  Maybe Text ->
  Maybe [Spec.TagGroup] ->
  Event.OnUpdateEventType ->
  m Spec.Fulfillment
mkFulFillment mbDriver ride booking mbVehicle mbImage mbTags event = do
  mbDInfo <- driverInfo
  pure $
    Spec.Fulfillment
      { fulfillmentId = Just ride.id.getId,
        fulfillmentStops = Utils.mkStops booking ride.otp,
        fulfillmentType = Just $ Utils.mkFulfillmentType booking.bookingType,
        fulfillmentAgent =
          Just $
            Spec.Agent
              { agentContact =
                  mbDInfo >>= \dInfo ->
                    Just $
                      Spec.Contact
                        { contactPhone = Just dInfo.mobileNumber
                        },
                agentPerson =
                  Just $
                    Spec.Person
                      { personId = Nothing,
                        personImage =
                          Just $
                            Spec.Image
                              { imageHeight = Nothing,
                                imageSizeType = Nothing,
                                imageUrl = mbImage,
                                imageWidth = Nothing
                              },
                        personName = mbDInfo >>= Just . (.name),
                        personTags = mbDInfo >>= (.tags)
                      }
              },
        fulfillmentVehicle =
          mbVehicle >>= \vehicle ->
            Just $
              Spec.Vehicle
                { vehicleColor = Just vehicle.color,
                  vehicleModel = Just vehicle.model,
                  vehicleRegistration = Just vehicle.registrationNo,
                  vehicleVariant = Utils.showVariant vehicle.variant,
                  vehicleMake = Nothing,
                  vehicleCategory = Nothing
                },
        fulfillmentCustomer = Nothing,
        fulfillmentState =
          Just $
            Spec.FulfillmentState
              { fulfillmentStateDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show event,
                        descriptorName = Nothing,
                        descriptorShortDesc = Nothing
                      }
              },
        fulfillmentTags = mbTags
      }
  where
    driverInfo = forM mbDriver $ \driver -> do
      dPhoneNum <- SP.getPersonNumber driver >>= fromMaybeM (InternalError "Driver mobile number is not present in OnUpdateBuildReq.")
      dName <- SP.getPersonFullName driver & fromMaybeM (PersonFieldNotPresent "firstName")
      let dTags = Utils.mkRideAssignedPersonTags driver
      pure $
        DriverInfo
          { mobileNumber = dPhoneNum,
            name = dName,
            tags = dTags
          }

buildOnUpdateReqV2 ::
  (MonadFlow m, EncFlow m r) =>
  Context.Action ->
  Context.Domain ->
  Text ->
  Text ->
  BaseUrl ->
  Context.City ->
  Context.Country ->
  OU.OnUpdateBuildReq ->
  m Spec.OnUpdateReq
buildOnUpdateReqV2 action domain messageId bppSubscriberId bppUri city country = \case
  OU.RideAssignedBuildReq OU.DRideAssignedReq {..} -> do
    context <- CU.buildContextV2 action domain messageId (Just booking.transactionId) booking.bapId booking.bapUri (Just bppSubscriberId) (Just bppUri) city country
    fulfillment <- mkFulFillment (Just driver) ride booking (Just vehicle) image Nothing Event.RIDE_ASSIGNED
    pure $
      Spec.OnUpdateReq
        { onUpdateReqError = Nothing,
          onUpdateReqContext = context,
          onUpdateReqMessage =
            Just $
              Spec.ConfirmReqMessage
                { confirmReqMessageOrder =
                    Spec.Order
                      { orderId = Just booking.id.getId,
                        orderFulfillments = Just [fulfillment],
                        orderBilling = Nothing,
                        orderCancellationTerms = Nothing,
                        orderItems = Nothing,
                        orderPayments = Nothing,
                        orderProvider = Nothing,
                        orderQuote = Nothing,
                        orderStatus = Just "ACTIVE"
                      }
                }
        }
  OU.RideStartedBuildReq OU.DRideStartedReq {..} -> do
    context <- CU.buildContextV2 action domain messageId (Just booking.transactionId) booking.bapId booking.bapUri (Just bppSubscriberId) (Just bppUri) city country
    fulfillment <- mkFulFillment (Just driver) ride booking (Just vehicle) Nothing Nothing Event.RIDE_STARTED
    pure $
      Spec.OnUpdateReq
        { onUpdateReqError = Nothing,
          onUpdateReqContext = context,
          onUpdateReqMessage =
            Just $
              Spec.ConfirmReqMessage
                { confirmReqMessageOrder =
                    Spec.Order
                      { orderId = Just booking.id.getId,
                        orderFulfillments = Just [fulfillment],
                        orderBilling = Nothing,
                        orderCancellationTerms = Nothing,
                        orderItems = Nothing,
                        orderPayments = Nothing,
                        orderProvider = Nothing,
                        orderQuote = Nothing,
                        orderStatus = Nothing
                      }
                }
        }
  OU.RideCompletedBuildReq OU.DRideCompletedReq {..} -> do
    context <- CU.buildContextV2 action domain messageId (Just booking.transactionId) booking.bapId booking.bapUri (Just bppSubscriberId) (Just bppUri) city country
    rideDistanceDetailsTags <- Utils.mkRideDistanceDetailsTags ride
    fulfillment <- mkFulFillment (Just driver) ride booking (Just vehicle) Nothing rideDistanceDetailsTags Event.RIDE_COMPLETED
    fare' :: DecimalValue <- realToFrac <$> ride.fare & fromMaybeM (InternalError "Ride fare is not present in RideCompletedReq ride.")
    let fare = show fare'
    let currency = "INR"
        breakup =
          mkBreakupList (mkPrice currency) mkBreakupItem fareParams
            & filter (filterRequiredBreakups $ DFParams.getFareParametersType fareParams)
        price =
          Spec.Price
            { priceComputedValue = Just fare,
              priceCurrency = Just currency,
              priceMaximumValue = Nothing,
              priceMinimumValue = Nothing,
              priceOfferedValue = Nothing,
              priceValue = Just fare
            }
    pure $
      Spec.OnUpdateReq
        { onUpdateReqError = Nothing,
          onUpdateReqContext = context,
          onUpdateReqMessage =
            Just $
              Spec.ConfirmReqMessage
                { confirmReqMessageOrder =
                    Spec.Order
                      { orderId = Just booking.id.getId,
                        orderFulfillments = Just [fulfillment],
                        orderBilling = Nothing,
                        orderCancellationTerms = Nothing,
                        orderItems = Nothing,
                        orderPayments =
                          Just . List.singleton $
                            Spec.Payment
                              { paymentCollectedBy = Just $ Utils.showPaymentCollectedBy paymentMethodInfo,
                                paymentId = Nothing,
                                paymentParams =
                                  Just $
                                    Spec.PaymentParams
                                      { paymentParamsAmount = Nothing,
                                        paymentParamsBankAccountNumber = Nothing,
                                        paymentParamsBankCode = Nothing,
                                        paymentParamsCurrency = Just currency,
                                        paymentParamsVirtualPaymentAddress = Nothing
                                      },
                                paymentStatus = Nothing,
                                paymentTags = Nothing,
                                paymentType = Just $ Utils.mkRideCompletedPaymentType paymentMethodInfo
                              },
                        orderProvider = Nothing,
                        orderQuote =
                          Just $
                            Spec.Quotation
                              { quotationBreakup = Just breakup,
                                quotationPrice = Just price,
                                quotationTtl = Nothing
                              },
                        orderStatus = Nothing
                      }
                }
        }
    where
      mkPrice currency val =
        Spec.Price
          { priceComputedValue = Nothing,
            priceCurrency = Just currency,
            priceMaximumValue = Nothing,
            priceMinimumValue = Nothing,
            priceOfferedValue = Nothing,
            priceValue = Just . (show :: DecimalValue -> Text) $ fromIntegral val
          }

      mkBreakupItem :: Text -> Spec.Price -> Spec.QuotationBreakupInner
      mkBreakupItem title price =
        Spec.QuotationBreakupInner
          { quotationBreakupInnerTitle = Just title,
            quotationBreakupInnerPrice = Just price
          }

      filterRequiredBreakups fParamsType breakup = do
        case fParamsType of
          DFParams.Progressive ->
            breakup.quotationBreakupInnerTitle == Just "BASE_FARE"
              || breakup.quotationBreakupInnerTitle == Just "SERVICE_CHARGE"
              || breakup.quotationBreakupInnerTitle == Just "DEAD_KILOMETER_FARE"
              || breakup.quotationBreakupInnerTitle == Just "EXTRA_DISTANCE_FARE"
              || breakup.quotationBreakupInnerTitle == Just "DRIVER_SELECTED_FARE"
              || breakup.quotationBreakupInnerTitle == Just "CUSTOMER_SELECTED_FARE"
              || breakup.quotationBreakupInnerTitle == Just "TOTAL_FARE"
              || breakup.quotationBreakupInnerTitle == Just "WAITING_OR_PICKUP_CHARGES"
              || breakup.quotationBreakupInnerTitle == Just "EXTRA_TIME_FARE"
              || breakup.quotationBreakupInnerTitle == Just "CUSTOMER_CANCELLATION_DUES"
          DFParams.Slab ->
            breakup.quotationBreakupInnerTitle == Just "BASE_FARE"
              || breakup.quotationBreakupInnerTitle == Just "SERVICE_CHARGE"
              || breakup.quotationBreakupInnerTitle == Just "WAITING_OR_PICKUP_CHARGES"
              || breakup.quotationBreakupInnerTitle == Just "PLATFORM_FEE"
              || breakup.quotationBreakupInnerTitle == Just "SGST"
              || breakup.quotationBreakupInnerTitle == Just "CGST"
              || breakup.quotationBreakupInnerTitle == Just "FIXED_GOVERNMENT_RATE"
              || breakup.quotationBreakupInnerTitle == Just "TOTAL_FARE"
              || breakup.quotationBreakupInnerTitle == Just "CUSTOMER_SELECTED_FARE"
              || breakup.quotationBreakupInnerTitle == Just "NIGHT_SHIFT_CHARGE"
              || breakup.quotationBreakupInnerTitle == Just "EXTRA_TIME_FARE"
              || breakup.quotationBreakupInnerTitle == Just "CUSTOMER_CANCELLATION_DUES"
  OU.BookingCancelledBuildReq OU.DBookingCancelledReq {..} -> do
    context <- CU.buildContextV2 action domain messageId (Just booking.transactionId) booking.bapId booking.bapUri (Just bppSubscriberId) (Just bppUri) city country
    pure $
      Spec.OnUpdateReq
        { onUpdateReqError = Nothing,
          onUpdateReqContext = context,
          onUpdateReqMessage =
            Just $
              Spec.ConfirmReqMessage
                { confirmReqMessageOrder =
                    Spec.Order
                      { orderId = Just booking.id.getId,
                        orderStatus = Just "CANCELLED",
                        orderFulfillments =
                          Just . List.singleton $
                            Spec.Fulfillment
                              { fulfillmentState =
                                  Just $
                                    Spec.FulfillmentState
                                      { fulfillmentStateDescriptor =
                                          Just $
                                            Spec.Descriptor
                                              { descriptorCode = Just $ show Event.RIDE_BOOKING_CANCELLED,
                                                descriptorName = Nothing,
                                                descriptorShortDesc = Nothing
                                              }
                                      },
                                fulfillmentId = Nothing,
                                fulfillmentStops = Nothing,
                                fulfillmentType = Nothing,
                                fulfillmentAgent = Nothing,
                                fulfillmentCustomer = Nothing,
                                fulfillmentTags = Nothing,
                                fulfillmentVehicle = Nothing
                              },
                        orderBilling = Nothing,
                        orderCancellationTerms = Nothing,
                        orderItems = Nothing,
                        orderPayments = Nothing,
                        orderProvider = Nothing,
                        orderQuote = Nothing
                      }
                }
        }
  OU.DriverArrivedBuildReq OU.DDriverArrivedReq {..} -> do
    context <- CU.buildContextV2 action domain messageId (Just booking.transactionId) booking.bapId booking.bapUri (Just bppSubscriberId) (Just bppUri) city country
    let driverArrivedInfoTags = Utils.mkDriverArrivedInfoTags arrivalTime
    fulfillment <- mkFulFillment (Just driver) ride booking (Just vehicle) Nothing driverArrivedInfoTags Event.DRIVER_ARRIVED
    pure $
      Spec.OnUpdateReq
        { onUpdateReqError = Nothing,
          onUpdateReqContext = context,
          onUpdateReqMessage =
            Just $
              Spec.ConfirmReqMessage
                { confirmReqMessageOrder =
                    Spec.Order
                      { orderId = Just ride.bookingId.getId,
                        orderFulfillments = Just [fulfillment],
                        orderBilling = Nothing,
                        orderCancellationTerms = Nothing,
                        orderItems = Nothing,
                        orderPayments = Nothing,
                        orderProvider = Nothing,
                        orderQuote = Nothing,
                        orderStatus = Nothing
                      }
                }
        }
  OU.EstimateRepetitionBuildReq OU.DEstimateRepetitionReq {..} -> do
    context <- CU.buildContextV2 action domain messageId (Just booking.transactionId) booking.bapId booking.bapUri (Just bppSubscriberId) (Just bppUri) city country
    let previousCancellationReasonsTags = Utils.mkPreviousCancellationReasonsTags cancellationSource
    fulfillment <- mkFulFillment Nothing ride booking Nothing Nothing previousCancellationReasonsTags Event.ESTIMATE_REPETITION
    pure $
      Spec.OnUpdateReq
        { onUpdateReqError = Nothing,
          onUpdateReqContext = context,
          onUpdateReqMessage =
            Just $
              Spec.ConfirmReqMessage
                { confirmReqMessageOrder =
                    Spec.Order
                      { orderId = Just booking.id.getId,
                        orderFulfillments = Just [fulfillment],
                        orderItems =
                          Just . List.singleton $
                            Spec.Item
                              { itemId = Just estimateId.getId,
                                itemDescriptor = Nothing,
                                itemFulfillmentIds = Nothing,
                                itemLocationIds = Nothing,
                                itemPaymentIds = Nothing,
                                itemPrice = Nothing,
                                itemTags = Nothing
                              },
                        orderBilling = Nothing,
                        orderCancellationTerms = Nothing,
                        orderPayments = Nothing,
                        orderProvider = Nothing,
                        orderQuote = Nothing,
                        orderStatus = Nothing
                      }
                }
        }
  OU.NewMessageBuildReq OU.DNewMessageReq {..} -> do
    context <- CU.buildContextV2 action domain messageId (Just booking.transactionId) booking.bapId booking.bapUri (Just bppSubscriberId) (Just bppUri) city country
    let newMessageTags = Utils.mkNewMessageTags message
    fulfillment <- mkFulFillment (Just driver) ride booking (Just vehicle) Nothing newMessageTags Event.NEW_MESSAGE
    pure $
      Spec.OnUpdateReq
        { onUpdateReqError = Nothing,
          onUpdateReqContext = context,
          onUpdateReqMessage =
            Just $
              Spec.ConfirmReqMessage
                { confirmReqMessageOrder =
                    Spec.Order
                      { orderId = Just ride.bookingId.getId,
                        orderFulfillments = Just [fulfillment],
                        orderBilling = Nothing,
                        orderCancellationTerms = Nothing,
                        orderItems = Nothing,
                        orderPayments = Nothing,
                        orderProvider = Nothing,
                        orderQuote = Nothing,
                        orderStatus = Nothing
                      }
                }
        }
  OU.SafetyAlertBuildReq OU.DSafetyAlertReq {..} -> do
    context <- CU.buildContextV2 action domain messageId (Just booking.transactionId) booking.bapId booking.bapUri (Just bppSubscriberId) (Just bppUri) city country
    let code = "safety_alert" -- TODO :: Fix this when rebasing, JAYPAL
    let safetyAlertTags = Utils.mkSafetyAlertTags reason code
    fulfillment <- mkFulFillment Nothing ride booking Nothing Nothing safetyAlertTags Event.SAFETY_ALERT
    pure $
      Spec.OnUpdateReq
        { onUpdateReqError = Nothing,
          onUpdateReqContext = context,
          onUpdateReqMessage =
            Just $
              Spec.ConfirmReqMessage
                { confirmReqMessageOrder =
                    Spec.Order
                      { orderId = Just ride.bookingId.getId,
                        orderFulfillments = Just [fulfillment],
                        orderBilling = Nothing,
                        orderCancellationTerms = Nothing,
                        orderItems = Nothing,
                        orderPayments = Nothing,
                        orderProvider = Nothing,
                        orderQuote = Nothing,
                        orderStatus = Nothing
                      }
                }
        }
