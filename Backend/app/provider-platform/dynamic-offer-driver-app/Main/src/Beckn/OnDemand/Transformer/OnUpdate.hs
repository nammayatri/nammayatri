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

import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.OnDemand.Utils.OnUpdate as Utils
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType as Event
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as CU
import qualified Data.List as List
import qualified Domain.Types.OnUpdate as OU
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common

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
    fulfillment <- Utils.mkFulFillmentV2 (Just driver) ride booking (Just vehicle) image Nothing isDriverBirthDay isFreeRide (Just $ show Event.RIDE_ASSIGNED)
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
                        orderCancellation = Nothing,
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
    fulfillment <- Utils.mkFulFillmentV2 (Just driver) ride booking (Just vehicle) Nothing Nothing False False (Just $ show Event.RIDE_STARTED)
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
                        orderCancellation = Nothing,
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
    distanceTagGroup <- Utils.mkDistanceTagGroup ride
    fulfillment <- Utils.mkFulFillmentV2 (Just driver) ride booking (Just vehicle) Nothing distanceTagGroup False False (Just $ show Event.RIDE_COMPLETED)
    quote <- Utils.mkRideCompletedQuote ride fareParams
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
                        orderQuote = Just quote,
                        orderPayments = Just $ Utils.mkRideCompletedPayment paymentMethodInfo paymentUrl,
                        orderFulfillments = Just [fulfillment],
                        orderBilling = Nothing,
                        orderCancellation = Nothing,
                        orderCancellationTerms = Nothing,
                        orderItems = Nothing,
                        orderProvider = Nothing,
                        orderStatus = Nothing
                      }
                }
        }
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
                        orderCancellation =
                          Just $
                            Spec.Cancellation
                              { cancellationCancelledBy = Just . show $ Utils.castCancellationSource cancellationSource
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
    fulfillment <- Utils.mkFulFillmentV2 (Just driver) ride booking (Just vehicle) Nothing driverArrivedInfoTags False False (Just $ show Event.DRIVER_ARRIVED)
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
                        orderCancellation = Nothing,
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
    fulfillment <- Utils.mkFulFillmentV2 Nothing ride booking Nothing Nothing previousCancellationReasonsTags False False (Just $ show Event.ESTIMATE_REPETITION)
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
                        orderCancellation = Nothing,
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
    fulfillment <- Utils.mkFulFillmentV2 (Just driver) ride booking (Just vehicle) Nothing newMessageTags False False (Just $ show Event.NEW_MESSAGE)
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
                        orderCancellation = Nothing,
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
    let safetyAlertTags = Utils.mkSafetyAlertTags reason code
    fulfillment <- Utils.mkFulFillmentV2 Nothing ride booking Nothing Nothing safetyAlertTags False False (Just $ show Event.SAFETY_ALERT)
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
                        orderCancellation = Nothing,
                        orderCancellationTerms = Nothing,
                        orderItems = Nothing,
                        orderPayments = Nothing,
                        orderProvider = Nothing,
                        orderQuote = Nothing,
                        orderStatus = Nothing
                      }
                }
        }
  OU.StopArrivedBuildReq OU.DStopArrivedBuildReq {..} -> do
    context <- CU.buildContextV2 action domain messageId (Just booking.transactionId) booking.bapId booking.bapUri (Just bppSubscriberId) (Just bppUri) city country
    fulfillment <- Utils.mkFulFillmentV2 Nothing ride booking Nothing Nothing Nothing False False (Just $ show Event.STOP_ARRIVED)
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
                        orderCancellation = Nothing,
                        orderCancellationTerms = Nothing,
                        orderItems = Nothing,
                        orderPayments = Nothing,
                        orderProvider = Nothing,
                        orderQuote = Nothing,
                        orderStatus = Nothing
                      }
                }
        }
