{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnUpdate
  ( buildOnUpdateMessage,
    buildOnUpdateMessageV2,
    module Reexport,
  )
where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Transformer.OnUpdate as TFOU
import qualified Beckn.OnDemand.Utils.Common as BUtils
import qualified Beckn.OnDemand.Utils.OnUpdate as Utils
import qualified Beckn.Types.Core.Taxi.Common.FulfillmentInfo as RideFulfillment
import qualified Beckn.Types.Core.Taxi.Common.Tags as Tags
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingCancelledEvent as BookingCancelledOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.DriverArrivedEvent as DriverArrivedOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.EstimateRepetitionEvent as EstimateRepetitionOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.NewMessageEvent as NewMessageOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent as RideAssignedOU
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent as OnUpdate
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent as RideCompletedOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideStartedEvent as RideStartedOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.SafetyAlertEvent as SafetyAlertDU
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.Merchant as DM
import Domain.Types.OnUpdate as Reexport
import qualified Domain.Types.Person as SP
import Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as SVeh
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FareCalculator
import Tools.Error

mkFullfillment ::
  (EsqDBFlow m r, EncFlow m r) =>
  Maybe SP.Person ->
  DRide.Ride ->
  DRB.Booking ->
  Maybe SVeh.Vehicle ->
  Maybe Text ->
  Maybe Tags.TagGroups ->
  m RideFulfillment.FulfillmentInfo
mkFullfillment mbDriver ride booking mbVehicle mbImage tags = do
  agent <-
    forM mbDriver $ \driver -> do
      let agentTags =
            [ Tags.TagGroup
                { display = False,
                  code = "driver_details",
                  name = "Driver Details",
                  list =
                    [ Tags.Tag (Just False) (Just "registered_at") (Just "Registered At") (Just $ show driver.createdAt),
                      Tags.Tag (Just False) (Just "rating") (Just "rating") (show <$> driver.rating)
                    ]
                }
            ]
      mobileNumber <- SP.getPersonNumber driver >>= fromMaybeM (InternalError "Driver mobile number is not present.")
      name <- SP.getPersonFullName driver & fromMaybeM (PersonFieldNotPresent "firstName")
      pure $
        RideAssignedOU.Agent
          { name = name,
            rateable = True,
            phone = Just mobileNumber,
            image = mbImage,
            tags = Just $ Tags.TG agentTags
          }
  let veh =
        mbVehicle <&> \vehicle ->
          RideAssignedOU.Vehicle
            { model = vehicle.model,
              variant = show vehicle.variant,
              color = vehicle.color,
              registration = vehicle.registrationNo
            }
  let authorization =
        RideAssignedOU.Authorization
          { _type = "OTP",
            token = ride.otp
          }
  pure $
    RideAssignedOU.FulfillmentInfo
      { id = ride.id.getId,
        start =
          RideAssignedOU.StartInfo
            { authorization =
                case booking.bookingType of
                  DRB.SpecialZoneBooking -> Just authorization
                  DRB.NormalBooking -> Just authorization, -- TODO :: Remove authorization for NormalBooking once Customer side code is decoupled.
              location =
                RideAssignedOU.Location
                  { gps = RideAssignedOU.Gps {lat = booking.fromLocation.lat, lon = booking.fromLocation.lon}
                  }
            },
        end =
          RideAssignedOU.EndInfo
            { location =
                RideAssignedOU.Location
                  { gps = RideAssignedOU.Gps {lat = booking.toLocation.lat, lon = booking.toLocation.lon} -- assuming locations will always be in correct order in list
                  }
            },
        agent,
        _type = if booking.bookingType == DRB.NormalBooking then RideAssignedOU.RIDE else RideAssignedOU.RIDE_OTP,
        vehicle = veh,
        ..
      }

buildOnUpdateMessage ::
  (EsqDBFlow m r, EncFlow m r) =>
  OnUpdateBuildReq ->
  m OnUpdate.OnUpdateMessage
buildOnUpdateMessage (RideAssignedBuildReq DRideAssignedReq {..}) = do
  fulfillment <- mkFullfillment (Just driver) ride booking (Just vehicle) image Nothing
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.RideAssigned $
            RideAssignedOU.RideAssignedEvent
              { id = booking.id.getId,
                state = "ACTIVE",
                ..
              },
        -- JAYPAL, TODO check where to place update_target
        update_target = "order.fufillment.state.code, order.fulfillment.agent, order.fulfillment.vehicle" <> ", order.fulfillment.start.authorization" -- TODO :: Remove authorization for NormalBooking once Customer side code is decoupled.
      }
buildOnUpdateMessage (RideStartedBuildReq DRideStartedReq {..}) = do
  fulfillment <- mkFullfillment (Just driver) ride booking (Just vehicle) Nothing Nothing
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.RideStarted $
            RideStartedOU.RideStartedEvent
              { id = booking.id.getId,
                ..
              },
        update_target = "order.fufillment.state.code"
      }
buildOnUpdateMessage (RideCompletedBuildReq req@DRideCompletedReq {}) = do
  chargeableDistance :: HighPrecMeters <-
    realToFrac <$> req.ride.chargeableDistance
      & fromMaybeM (InternalError "Ride chargeable distance is not present.")
  let traveledDistance :: HighPrecMeters = req.ride.traveledDistance
  let tagGroups =
        [ Tags.TagGroup
            { display = False,
              code = "ride_distance_details",
              name = "Ride Distance Details",
              list =
                [ Tags.Tag (Just False) (Just "chargeable_distance") (Just "Chargeable Distance") (Just $ show chargeableDistance),
                  Tags.Tag (Just False) (Just "traveled_distance") (Just "Traveled Distance") (Just $ show traveledDistance)
                ]
            }
        ]
  fulfillment <- mkFullfillment (Just req.driver) req.ride req.booking (Just req.vehicle) Nothing (Just $ Tags.TG tagGroups)
  fare <- realToFrac <$> req.ride.fare & fromMaybeM (InternalError "Ride fare is not present.")
  let currency = "INR"
      price =
        RideCompletedOU.QuotePrice
          { currency,
            value = fare,
            computed_value = fare
          }
      breakup =
        mkBreakupList (OnUpdate.BreakupPrice currency . fromIntegral) OnUpdate.BreakupItem req.fareParams
          & filter (filterRequiredBreakups $ DFParams.getFareParametersType req.fareParams) -- TODO: Remove after roll out
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.RideCompleted
            RideCompletedOU.RideCompletedEvent
              { id = req.booking.id.getId,
                quote =
                  RideCompletedOU.RideCompletedQuote
                    { price,
                      breakup
                    },
                payment =
                  Just
                    OnUpdate.Payment
                      { _type = maybe OnUpdate.ON_FULFILLMENT (Common.castDPaymentType . (.paymentType)) req.paymentMethodInfo,
                        params =
                          OnUpdate.PaymentParams
                            { collected_by = maybe OnUpdate.BPP (Common.castDPaymentCollector . (.collectedBy)) req.paymentMethodInfo,
                              instrument = Nothing,
                              currency = "INR",
                              amount = Nothing
                            },
                        uri = req.paymentUrl
                      },
                fulfillment = fulfillment
              },
        update_target = "order.payment, order.quote, order.fulfillment.tags, order.fulfillment.state.tags"
      }
  where
    filterRequiredBreakups fParamsType breakup = do
      case fParamsType of
        DFParams.Progressive ->
          breakup.title == "BASE_FARE"
            || breakup.title == "SERVICE_CHARGE"
            || breakup.title == "DEAD_KILOMETER_FARE"
            || breakup.title == "EXTRA_DISTANCE_FARE"
            || breakup.title == "DRIVER_SELECTED_FARE"
            || breakup.title == "CUSTOMER_SELECTED_FARE"
            || breakup.title == "TOTAL_FARE"
            || breakup.title == "WAITING_OR_PICKUP_CHARGES"
            || breakup.title == "EXTRA_TIME_FARE"
            || breakup.title == "CUSTOMER_CANCELLATION_DUES"
        DFParams.Slab ->
          breakup.title == "BASE_FARE"
            || breakup.title == "SERVICE_CHARGE"
            || breakup.title == "WAITING_OR_PICKUP_CHARGES"
            || breakup.title == "PLATFORM_FEE"
            || breakup.title == "SGST"
            || breakup.title == "CGST"
            || breakup.title == "FIXED_GOVERNMENT_RATE"
            || breakup.title == "TOTAL_FARE"
            || breakup.title == "CUSTOMER_SELECTED_FARE"
            || breakup.title == "NIGHT_SHIFT_CHARGE"
            || breakup.title == "EXTRA_TIME_FARE"
            || breakup.title == "CUSTOMER_CANCELLATION_DUES"
buildOnUpdateMessage (BookingCancelledBuildReq DBookingCancelledReq {..}) = do
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.BookingCancelled $
            BookingCancelledOU.BookingCancelledEvent
              { id = booking.id.getId,
                state = "CANCELLED"
                -- cancellation_reason = castCancellationSource cancellationSource -- TODO :: Handle this event using on_cancel. JAYPAL
              },
        update_target = "state,fufillment.state.code"
      }
buildOnUpdateMessage (DriverArrivedBuildReq DDriverArrivedReq {..}) = do
  let tagGroups =
        [ Tags.TagGroup
            { display = False,
              code = "driver_arrived_info",
              name = "Driver Arrived Info",
              list = [Tags.Tag (Just False) (Just "arrival_time") (Just "Chargeable Distance") (show <$> arrivalTime) | isJust arrivalTime]
            }
        ]
  fulfillment <- mkFullfillment (Just driver) ride booking (Just vehicle) Nothing (Just $ Tags.TG tagGroups)
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.DriverArrived $
            DriverArrivedOU.DriverArrivedEvent
              { id = ride.bookingId.getId,
                fulfillment
              },
        update_target = "order.fufillment.state.code, order.fulfillment.tags"
      }
buildOnUpdateMessage (EstimateRepetitionBuildReq DEstimateRepetitionReq {..}) = do
  let tagGroups =
        [ Tags.TagGroup
            { display = False,
              code = "previous_cancellation_reasons",
              name = "Previous Cancellation Reasons",
              list = [Tags.Tag (Just False) (Just "cancellation_reason") (Just "Chargeable Distance") (Just . show $ Utils.castCancellationSource cancellationSource)]
            }
        ]
  fulfillment <- mkFullfillment Nothing ride booking Nothing Nothing (Just $ Tags.TG tagGroups)
  let item = EstimateRepetitionOU.Item {id = estimateId.getId}
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.EstimateRepetition $
            EstimateRepetitionOU.EstimateRepetitionEvent
              { id = booking.id.getId,
                item = item,
                fulfillment
              },
        update_target = "order.fufillment.state.code, order.tags"
      }
buildOnUpdateMessage (NewMessageBuildReq DNewMessageReq {..}) = do
  let tagGroups =
        [ Tags.TagGroup
            { display = False,
              code = "driver_new_message",
              name = "Driver New Message",
              list = [Tags.Tag (Just False) (Just "message") (Just "New Message") (Just message)]
            }
        ]
  fulfillment <- mkFullfillment (Just driver) ride booking (Just vehicle) Nothing (Just $ Tags.TG tagGroups)
  return $
    OnUpdate.OnUpdateMessage
      { update_target = "order.fufillment.state.code, order.fulfillment.tags",
        order =
          OnUpdate.NewMessage $
            NewMessageOU.NewMessageEvent
              { id = ride.bookingId.getId,
                fulfillment = fulfillment
              }
      }
buildOnUpdateMessage (SafetyAlertBuildReq DSafetyAlertReq {..}) = do
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.SafetyAlert $
            SafetyAlertDU.SafetyAlertEvent
              { id = ride.bookingId.getId,
                fulfillment = SafetyAlertDU.FulfillmentInfo ride.id.getId,
                reason = reason
              },
        update_target = "order.fufillment.state.code, order.fulfillment.tags"
      }

buildOnUpdateMessageV2 ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  DM.Merchant ->
  Maybe Context.City ->
  Maybe Context.Country ->
  OnUpdateBuildReq ->
  m Spec.OnUpdateReq
buildOnUpdateMessageV2 merchant mbBapCity mbBapCountry req = do
  msgId <- generateGUID
  let bppId = getShortId $ merchant.subscriberId
      city = fromMaybe merchant.city mbBapCity
      country = fromMaybe merchant.country mbBapCountry
  bppUri <- BUtils.mkBppUri merchant.id.getId
  TFOU.buildOnUpdateReqV2 Context.ON_UPDATE Context.MOBILITY msgId bppId bppUri city country req

-- buildOnUpdateMessageV2 merchant mbBapCity mbBapCountry (RideAssignedBuildReq req) = do
--   msgId <- generateGUID
--   let bppId = getShortId $ merchant.subscriberId
--       city = fromMaybe merchant.city mbBapCity
--       country = fromMaybe merchant.country mbBapCountry
--   bppUri <- BUtils.mkBppUri merchant.id.getId
--   driverNumber <- SP.getPersonNumber req.driver >>= fromMaybeM (InternalError "Driver mobile number is not present in RideAssignedBuildReq.")
--   TFRA.buildOnUpdateReqV2 Context.ON_UPDATE Context.MOBILITY msgId bppId bppUri city country req driverNumber
