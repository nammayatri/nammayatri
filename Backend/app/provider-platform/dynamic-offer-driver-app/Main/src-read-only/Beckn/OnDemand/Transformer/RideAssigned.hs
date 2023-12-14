{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.RideAssigned where

import qualified Beckn.OnDemand.Utils.OnUpdate
import qualified Beckn.Types.Core.Taxi.Common.Tags
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Utils.Common
import qualified BecknV2.OnDemand.Utils.Context
import qualified Data.List
import qualified Data.Text
import qualified Domain.Types.Booking
import qualified Domain.Types.OnUpdate
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Domain.Types.Vehicle
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common (type (:::))

buildOnUpdateAgentV2 :: () => Maybe Domain.Types.Person.Person -> Maybe Data.Text.Text -> Data.Text.Text -> (Maybe BecknV2.OnDemand.Types.Agent)
buildOnUpdateAgentV2 mbDriver mbImage driverNumber = do
  let agentContact_ = buildOnUpdateContactV2 (Just driverNumber)
  let agentPerson_ = buildOnUpdatePersonV2 mbDriver mbImage
  let returnData = BecknV2.OnDemand.Types.Agent {agentContact = agentContact_, agentPerson = agentPerson_}
  let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

buildOnUpdateContactV2 :: () => Maybe Data.Text.Text -> (Maybe BecknV2.OnDemand.Types.Contact)
buildOnUpdateContactV2 mbPhone = do
  let contactPhone_ = mbPhone
  let returnData = BecknV2.OnDemand.Types.Contact {contactPhone = contactPhone_}
  let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

buildOnUpdateFulfillmentV2 :: () => Maybe Domain.Types.Person.Person -> Domain.Types.Ride.Ride -> Domain.Types.Booking.Booking -> Maybe Domain.Types.Vehicle.Vehicle -> Maybe Data.Text.Text -> Data.Text.Text -> (Maybe BecknV2.OnDemand.Types.Fulfillment)
buildOnUpdateFulfillmentV2 mbDriver ride booking mbVehicle mbImage driverNumber = do
  let fulfillmentAgent_ = buildOnUpdateAgentV2 mbDriver mbImage driverNumber
  let fulfillmentCustomer_ = Nothing
  let fulfillmentId_ = ride.id.getId & Just
  let fulfillmentState_ = Nothing
  let fulfillmentStops_ = Beckn.OnDemand.Utils.OnUpdate.mkStops booking ride.otp
  let fulfillmentTags_ = Nothing
  let fulfillmentType_ = Beckn.OnDemand.Utils.OnUpdate.mkFulfillmentType booking.bookingType & Just
  let fulfillmentVehicle_ = buildOnUpdateVehicleV2 =<< mbVehicle
  let returnData = BecknV2.OnDemand.Types.Fulfillment {fulfillmentAgent = fulfillmentAgent_, fulfillmentCustomer = fulfillmentCustomer_, fulfillmentId = fulfillmentId_, fulfillmentState = fulfillmentState_, fulfillmentStops = fulfillmentStops_, fulfillmentTags = fulfillmentTags_, fulfillmentType = fulfillmentType_, fulfillmentVehicle = fulfillmentVehicle_}
  let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

buildOnUpdateImageV2 :: () => Maybe Data.Text.Text -> (Maybe BecknV2.OnDemand.Types.Image)
buildOnUpdateImageV2 mbImage = do
  let imageHeight_ = Nothing
  let imageSizeType_ = Nothing
  let imageUrl_ = mbImage
  let imageWidth_ = Nothing
  let returnData = BecknV2.OnDemand.Types.Image {imageHeight = imageHeight_, imageSizeType = imageSizeType_, imageUrl = imageUrl_, imageWidth = imageWidth_}
  let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

-- buildOnUpdateMessageV2 :: () => Domain.Types.OnUpdate.DRideAssignedReq -> Data.Text.Text -> (Maybe BecknV2.OnDemand.Types.ConfirmReqMessage)
-- buildOnUpdateMessageV2 req driverNumber = do
--   let confirmReqMessageOrder_ = buildOnUpdateOrderV2 req driverNumber
--   let returnData = BecknV2.OnDemand.Types.ConfirmReqMessage {confirmReqMessageOrder = confirmReqMessageOrder_}
--   let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
--   if allNothing
--     then Nothing
--     else Just returnData

-- buildOnUpdateOrderV2 :: () => Domain.Types.OnUpdate.DRideAssignedReq -> Data.Text.Text -> (BecknV2.OnDemand.Types.Order)
-- buildOnUpdateOrderV2 req driverNumber = do
--   let orderBilling_ = Nothing
--   let orderCancellationTerms_ = Nothing
--   let orderFulfillments_ = Data.List.singleton <$> (buildOnUpdateFulfillmentV2 (Just req.driver) req.ride req.booking (Just req.vehicle) req.image driverNumber)
--   let orderId_ = Just req.booking.id.getId
--   let orderItems_ = Nothing
--   let orderPayments_ = Nothing
--   let orderProvider_ = Nothing
--   let orderQuote_ = Nothing
--   let orderStatus_ = Just "ACTIVE"
--   BecknV2.OnDemand.Types.Order {orderBilling = orderBilling_, orderCancellationTerms = orderCancellationTerms_, orderFulfillments = orderFulfillments_, orderId = orderId_, orderItems = orderItems_, orderPayments = orderPayments_, orderProvider = orderProvider_, orderQuote = orderQuote_, orderStatus = orderStatus_}

buildOnUpdatePersonV2 :: () => Maybe Domain.Types.Person.Person -> Maybe Data.Text.Text -> (Maybe BecknV2.OnDemand.Types.Person)
buildOnUpdatePersonV2 mbDriver mbImage = do
  let personId_ = Nothing
  let personImage_ = buildOnUpdateImageV2 mbImage
  let personName_ = Domain.Types.Person.getPersonFullName =<< mbDriver
  let personTags_ = Beckn.OnDemand.Utils.OnUpdate.mkRideAssignedPersonTags =<< mbDriver
  let returnData = BecknV2.OnDemand.Types.Person {personId = personId_, personImage = personImage_, personName = personName_, personTags = personTags_}
  let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

-- buildOnUpdateReqV2 :: (Kernel.Types.App.MonadFlow m) => Kernel.Types.Beckn.Context.Action -> Kernel.Types.Beckn.Context.Domain -> Data.Text.Text -> Data.Text.Text -> Kernel.Prelude.BaseUrl -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Beckn.Context.Country -> Domain.Types.OnUpdate.DRideAssignedReq -> Data.Text.Text -> m (BecknV2.OnDemand.Types.OnUpdateReq)
-- buildOnUpdateReqV2 action domain messageId bppSubscriberId bppUri city country req driverNumber = do
--   let onUpdateReqError_ = Nothing
--   let onUpdateReqMessage_ = buildOnUpdateMessageV2 req driverNumber
--   onUpdateReqContext_ <- BecknV2.OnDemand.Utils.Context.buildContextV2 action domain messageId (Just req.booking.transactionId) req.booking.bapId req.booking.bapUri (Just bppSubscriberId) (Just bppUri) city country
--   pure $ BecknV2.OnDemand.Types.OnUpdateReq {onUpdateReqContext = onUpdateReqContext_, onUpdateReqError = onUpdateReqError_, onUpdateReqMessage = onUpdateReqMessage_}

buildOnUpdateVehicleV2 :: () => Domain.Types.Vehicle.Vehicle -> (Maybe BecknV2.OnDemand.Types.Vehicle)
buildOnUpdateVehicleV2 vehicle = do
  let vehicleCategory_ = Nothing
  let vehicleColor_ = Just vehicle.color
  let vehicleMake_ = Nothing
  let vehicleModel_ = Just vehicle.model
  let vehicleRegistration_ = Just vehicle.registrationNo
  let vehicleVariant_ = Beckn.OnDemand.Utils.OnUpdate.showVariant vehicle.variant
  let returnData = BecknV2.OnDemand.Types.Vehicle {vehicleCategory = vehicleCategory_, vehicleColor = vehicleColor_, vehicleMake = vehicleMake_, vehicleModel = vehicleModel_, vehicleRegistration = vehicleRegistration_, vehicleVariant = vehicleVariant_}
  let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData
