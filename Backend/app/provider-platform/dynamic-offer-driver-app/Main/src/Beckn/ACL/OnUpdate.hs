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
import qualified Beckn.ACL.Common.Order as Common
import qualified Beckn.OnDemand.Transformer.OnUpdate as TFOU
import qualified Beckn.OnDemand.Utils.Common as BUtils
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
import qualified Domain.Types.Merchant as DM
import Domain.Types.OnUpdate as Reexport
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common

buildOnUpdateMessage ::
  (EsqDBFlow m r, EncFlow m r) =>
  OnUpdateBuildReq ->
  m OnUpdate.OnUpdateMessage
buildOnUpdateMessage (RideAssignedBuildReq DRideAssignedReq {..}) = do
  fulfillment <- Common.mkFulfillment (Just driver) ride booking (Just vehicle) image Nothing Nothing isDriverBirthDay isFreeRide
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.RideAssigned $
            RideAssignedOU.RideAssignedEvent
              { id = booking.id.getId,
                state = "ACTIVE",
                ..
              },
        update_target = "order.fufillment.state.code, order.fulfillment.agent, order.fulfillment.vehicle" <> ", order.fulfillment.start.authorization" -- TODO :: Remove authorization for NormalBooking once Customer side code is decoupled.
      }
buildOnUpdateMessage (RideStartedBuildReq DRideStartedReq {..}) = do
  let personTag = Common.mkLocationTagGroup tripStartLocation
  fulfillment <- Common.mkFulfillment (Just driver) ride booking (Just vehicle) Nothing Nothing (Just $ Tags.TG personTag) False False
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
buildOnUpdateMessage (RideCompletedBuildReq DRideCompletedReq {..}) = do
  let personTag = Common.mkLocationTagGroup tripEndLocation
  distanceTagGroup <- Common.buildDistanceTagGroup ride
  fulfillment <- Common.mkFulfillment (Just driver) ride booking (Just vehicle) Nothing (Just $ Tags.TG distanceTagGroup) (Just $ Tags.TG personTag) False False
  quote <- Common.buildRideCompletedQuote ride fareParams
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.RideCompleted
            RideCompletedOU.RideCompletedEvent
              { id = booking.id.getId,
                quote,
                payment = Just $ Common.mkRideCompletedPayment paymentMethodInfo paymentUrl,
                fulfillment = fulfillment
              },
        update_target = "order.payment, order.quote, order.fulfillment.tags, order.fulfillment.state.tags"
      }
buildOnUpdateMessage (BookingCancelledBuildReq DBookingCancelledReq {..}) = do
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.BookingCancelled $
            BookingCancelledOU.BookingCancelledEvent
              { id = booking.id.getId,
                state = "CANCELLED",
                cancellation_reason = Common.castCancellationSource cancellationSource
              },
        update_target = "state,fufillment.state.code"
      }
buildOnUpdateMessage (DriverArrivedBuildReq DDriverArrivedReq {..}) = do
  let tagGroups = Common.mkArrivalTimeTagGroup arrivalTime
  fulfillment <- Common.mkFulfillment (Just driver) ride booking (Just vehicle) Nothing (Just $ Tags.TG tagGroups) Nothing False False
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
              list = [Tags.Tag (Just False) (Just "cancellation_reason") (Just "Chargeable Distance") (Just . show $ Common.castCancellationSource cancellationSource)]
            }
        ]
  fulfillment <- Common.mkFulfillment Nothing ride booking Nothing Nothing (Just $ Tags.TG tagGroups) Nothing False False
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
  fulfillment <- Common.mkFulfillment (Just driver) ride booking (Just vehicle) Nothing (Just $ Tags.TG tagGroups) Nothing False False
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
  let tagGroups =
        [ Tags.TagGroup
            { display = False,
              code = "safety_alert",
              name = "Safety Alert",
              list = [Tags.Tag (Just False) (Just code) (Just "Safety Alert Trigger") (Just reason)]
            }
        ]
  fulfillment <- Common.mkFulfillment Nothing ride booking Nothing Nothing (Just $ Tags.TG tagGroups) Nothing False False
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.SafetyAlert $
            SafetyAlertDU.SafetyAlertEvent
              { id = ride.bookingId.getId,
                fulfillment = fulfillment
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
