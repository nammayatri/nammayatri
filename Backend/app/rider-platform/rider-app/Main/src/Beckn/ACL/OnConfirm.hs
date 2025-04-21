{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnConfirm (buildOnConfirmReqV2) where

import Beckn.ACL.Common as ACL
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Common as DCommon
import qualified Domain.Action.Beckn.OnConfirm as DOnConfirm
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error

buildOnConfirmReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    MonadFlow m,
    CacheFlow m r
  ) =>
  Spec.OnConfirmReq ->
  Bool ->
  m (Maybe DOnConfirm.OnConfirmReq)
buildOnConfirmReqV2 req isValueAddNP = do
  ContextV2.validateContext Context.ON_CONFIRM req.onConfirmReqContext
  handleErrorV2 req $ \message -> do
    let order = message.confirmReqMessageOrder
    fareParamsQuotationBreakup <- order.orderQuote >>= (.quotationBreakup) & fromMaybeM (InvalidRequest "Missing Quote Breakups.")
    let fareBreakups = mapMaybe ACL.mkDFareBreakup fareParamsQuotationBreakup
    case parseData message fareBreakups of
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
    parseData :: Spec.ConfirmReqMessage -> [DCommon.DFareBreakup] -> Either Text DOnConfirm.OnConfirmReq
    parseData message fareBreakups = do
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
          let agentPerson = fulf >>= (.fulfillmentAgent) >>= (.agentPerson)
              tagGroups = agentPerson >>= (.personTags)
              tagGroupsFullfillment = order.orderFulfillments >>= listToMaybe >>= (.fulfillmentTags)
              driverImage = agentPerson >>= (.personImage) >>= (.imageUrl)
              driverMobileCountryCode = Just "+91" -- TODO: check how to get countrycode via ONDC
              driverRating :: Maybe Centesimal = readMaybe . T.unpack =<< getTagV2' Tag.DRIVER_DETAILS Tag.RATING tagGroups
              driverRegisteredAt :: Maybe UTCTime = readMaybe . T.unpack =<< getTagV2' Tag.DRIVER_DETAILS Tag.REGISTERED_AT tagGroups
              isDriverBirthDay = isJust $ getTagV2' Tag.DRIVER_DETAILS Tag.IS_DRIVER_BIRTHDAY tagGroups
              isFreeRide = isJust $ getTagV2' Tag.DRIVER_DETAILS Tag.IS_FREE_RIDE tagGroups
              previousRideEndPos = getLocationFromTagV2 tagGroupsFullfillment Tag.FORWARD_BATCHING_REQUEST_INFO Tag.PREVIOUS_RIDE_DROP_LOCATION_LAT Tag.PREVIOUS_RIDE_DROP_LOCATION_LON
              vehicleAge :: Maybe Months = readMaybe . T.unpack =<< getTagV2' Tag.VEHICLE_AGE_INFO Tag.VEHICLE_AGE tagGroupsFullfillment
              driverAlternatePhoneNumber :: Maybe Text = getTagV2' Tag.DRIVER_DETAILS Tag.DRIVER_ALTERNATE_NUMBER tagGroups
              isAlreadyFav = isJust $ getTagV2' Tag.DRIVER_DETAILS Tag.IS_ALREADY_FAVOURITE tagGroups
              favCount :: Maybe Int = readMaybe . T.unpack =<< getTagV2' Tag.DRIVER_DETAILS Tag.FAVOURITE_COUNT tagGroups
              driverAccountId = getTagV2' Tag.DRIVER_DETAILS Tag.DRIVER_ACCOUNT_ID tagGroups
              isSafetyPlus' = isJust $ getTagV2' Tag.DRIVER_DETAILS Tag.IS_SAFETY_PLUS tagGroups
          rideOtp <- maybe (Left "Missing rideOtp in on_confirm") Right mbRideOtp
          bppRideId <- fulf >>= (.fulfillmentId) & maybe (Left "Missing fulfillmentId") (Right . Id)
          driverName <- fulf >>= (.fulfillmentAgent) >>= (.agentPerson) >>= (.personName) & maybe (Left "Missing fulfillment.agent.person.name in on_confirm") Right
          driverMobileNumber <- fulf >>= (.fulfillmentAgent) >>= (.agentContact) >>= (.contactPhone) & maybe (Left "Missing fulfillment.agent.contact.phone in on_confirm") Right
          vehicleNumber <- fulf >>= (.fulfillmentVehicle) >>= (.vehicleRegistration) & maybe (Left "Missing fulfillment.vehicle.registration in on_confirm") Right
          let vehicleColor = fulf >>= (.fulfillmentVehicle) >>= (.vehicleColor)
          vehicleModel <- fulf >>= (.fulfillmentVehicle) >>= (.vehicleModel) & maybe (Left "Missing fulfillment.vehicle.model in on_confirm") Right
          Right $ DOnConfirm.RideAssigned DOnConfirm.RideAssignedInfo {fareBreakups = Just fareBreakups, isSafetyPlus = isSafetyPlus', ..}
        else Right $ DOnConfirm.BookingConfirmed DOnConfirm.BookingConfirmedInfo {specialZoneOtp = mbRideOtp, ..}

handleErrorV2 ::
  (MonadFlow m) =>
  Spec.OnConfirmReq ->
  (Spec.ConfirmReqMessage -> m (Maybe DOnConfirm.OnConfirmReq)) ->
  m (Maybe DOnConfirm.OnConfirmReq)
handleErrorV2 req action =
  case req.onConfirmReqError of
    Nothing -> req.onConfirmReqMessage & maybe (pure Nothing) action
    Just err -> do
      logTagError "on_confirm req" $ "on_confirm error:-" <> show err
      pure Nothing
