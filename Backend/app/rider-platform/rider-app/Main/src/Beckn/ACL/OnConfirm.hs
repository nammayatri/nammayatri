{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnConfirm (buildOnConfirmReqV2) where

import qualified Beckn.ACL.Cancel as CancelACL
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Domain.Action.Beckn.OnConfirm as DOnConfirm
import qualified Domain.Action.UI.Cancel as DCancel
import Domain.Types.CancellationReason
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.Queries.Booking as QRideB
import Tools.Error

buildOnConfirmReqV2 ::
  ( EncFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["_version" ::: Text, "internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl, "nwAddress" ::: BaseUrl]
  ) =>
  Spec.OnConfirmReq ->
  Bool ->
  m (Maybe DOnConfirm.OnConfirmReq)
buildOnConfirmReqV2 req isValueAddNP = do
  ContextV2.validateContext Context.ON_CONFIRM req.onConfirmReqContext
  handleErrorV2 req $ \message -> do
    case parseData message of
      Right dReq -> do
        case dReq of
          DOnConfirm.BookingConfirmed _ | not isValueAddNP -> do
            -- when its not a value-add-np ride flow, we need on_confirm to have DELIVERY fulfillmentType.
            let fulfType = message.confirmReqMessageOrder.orderFulfillments >>= listToMaybe >>= (.fulfillmentType) >>= readMaybe . T.unpack
            when (fulfType /= Just Enums.DELIVERY) $ do
              throwError . InvalidBecknSchema $ "Invalid fulfillment type in on_confirm:-" <> show fulfType <> ",expected:-" <> show Enums.DELIVERY
          _ -> pure ()
        return $ Just dReq
      Left err -> throwError . InvalidBecknSchema $ "on_confirm error:-" <> show err
  where
    parseData :: Spec.ConfirmReqMessage -> Either Text DOnConfirm.OnConfirmReq
    parseData message = do
      let order = message.confirmReqMessageOrder
      bppBookingIdText <- order.orderId & maybe (Left "Missing OrderId") Right
      let bppBookingId = Id bppBookingIdText
          fulf = order.orderFulfillments >>= listToMaybe
          mbRideOtp =
            fulf >>= (.fulfillmentStops) >>= Utils.getStartLocation >>= (.stopAuthorization)
              >>= \auth -> if auth.authorizationType == Just (show Enums.OTP) then auth.authorizationToken else Nothing

      let isDriverDetailsPresent = fulf >>= (.fulfillmentAgent) >>= (.agentContact) >>= (.contactPhone) & isJust

      if isDriverDetailsPresent
        then do
          let driverImage = fulf >>= (.fulfillmentAgent) >>= (.agentPerson) >>= (.personImage) >>= (.imageUrl)
              driverMobileCountryCode = Just "+91" -- TODO: check how to get countrycode via ONDC
              driverRating = Nothing
              driverRegisteredAt = Nothing
              isDriverBirthDay = False
              isFreeRide = False

          rideOtp <- maybe (Left "Missing rideOtp in on_confirm") Right mbRideOtp
          bppRideId <- fulf >>= (.fulfillmentId) & maybe (Left "Missing fulfillmentId") (Right . Id)
          driverName <- fulf >>= (.fulfillmentAgent) >>= (.agentPerson) >>= (.personName) & maybe (Left "Missing fulfillment.agent.person.name in on_confirm") Right
          driverMobileNumber <- fulf >>= (.fulfillmentAgent) >>= (.agentContact) >>= (.contactPhone) & maybe (Left "Missing fulfillment.agent.contact.phone in on_confirm") Right

          vehicleNumber <- fulf >>= (.fulfillmentVehicle) >>= (.vehicleRegistration) & maybe (Left "Missing fulfillment.vehicle.registration in on_confirm") Right
          let vehicleColor = fulf >>= (.fulfillmentVehicle) >>= (.vehicleColor)
          vehicleModel <- fulf >>= (.fulfillmentVehicle) >>= (.vehicleModel) & maybe (Left "Missing fulfillment.vehicle.model in on_confirm") Right

          Right $ DOnConfirm.RideAssigned DOnConfirm.RideAssignedInfo {..}
        else Right $ DOnConfirm.BookingConfirmed DOnConfirm.BookingConfirmedInfo {bppBookingId, specialZoneOtp = mbRideOtp}

handleErrorV2 ::
  (EncFlow m r, Esq.EsqDBReplicaFlow m r, EsqDBFlow m r, CacheFlow m r, MonadFlow m, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl, "nwAddress" ::: BaseUrl]) =>
  Spec.OnConfirmReq ->
  (Spec.ConfirmReqMessage -> m (Maybe DOnConfirm.OnConfirmReq)) ->
  m (Maybe DOnConfirm.OnConfirmReq)
handleErrorV2 req action =
  case req.onConfirmReqError of
    Nothing -> req.onConfirmReqMessage & maybe (pure Nothing) action
    Just err -> do
      logTagError "on_confirm req" $ "on_confirm error:-" <> show err
      transactionId <- (fmap UUID.toText req.onConfirmReqContext.contextTransactionId) & fromMaybeM (InvalidRequest "Missing transactionId in context")
      let cancelReq = buildCancelReq err.errorMessage
      booking <- QRideB.findLatestByTransactionId transactionId >>= fromMaybeM (BookingDoesNotExist transactionId)
      dCancelRes <- DCancel.cancel booking.id (booking.riderId, booking.merchantId) cancelReq
      void $ CallBPP.cancelV2 dCancelRes.bppUrl =<< CancelACL.buildCancelReqV2 dCancelRes
      pure Nothing

buildCancelReq :: Maybe Text -> DCancel.CancelReq
buildCancelReq mbErrorMessage =
  DCancel.CancelReq
    { reasonCode = CancellationReasonCode "BPP cancelled: on_init error",
      reasonStage = OnConfirm,
      additionalInfo = mbErrorMessage
    }
