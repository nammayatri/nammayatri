{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Update (buildUpdateReq) where

import qualified Beckn.OnDemand.Utils.Common as Common
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.Aeson as A
import Data.Maybe (listToMaybe)
import qualified Domain.Action.Beckn.Update as DUpdate
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import EulerHS.Prelude hiding (state)
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Tools.Error (GenericError (InvalidRequest))

buildUpdateReq ::
  ( HasFlowEnv m r '["_version" ::: Text]
  ) =>
  Subscriber.Subscriber ->
  Spec.UpdateReq ->
  m DUpdate.DUpdateReq
buildUpdateReq subscriber req = do
  ContextV2.validateContext Context.UPDATE $ req.updateReqContext
  bap_uri <- Common.getContextBapUri req.updateReqContext
  unless (Just subscriber.subscriber_id == req.updateReqContext.contextBapId) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
  parseEventV2 req.updateReqMessage

parseEventV2 :: (MonadFlow m) => Spec.UpdateReqMessage -> m DUpdate.DUpdateReq
parseEventV2 req = do
  eventType <-
    req.updateReqMessageOrder.orderFulfillments
      >>= listToMaybe
      >>= (.fulfillmentState)
      >>= (.fulfillmentStateDescriptor)
      >>= (.descriptorCode)
      & fromMaybeM (InvalidRequest "Event type is not present in OnUpdateReq.")
  case eventType of
    "PAYMENT_COMPLETED" -> parsePaymentCompletedEvent req
    "EDIT_LOCATION" -> parseEditLocationEvent req
    _ -> throwError (InvalidRequest "Invalid event type")

parsePaymentCompletedEvent :: (MonadFlow m) => Spec.UpdateReqMessage -> m DUpdate.DUpdateReq
parsePaymentCompletedEvent req = do
  rideId <- req.updateReqMessageOrder.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "RideId not present")
  payment <- req.updateReqMessageOrder.orderPayments >>= listToMaybe & fromMaybeM (InvalidRequest "Payment not present")
  orderId <- req.updateReqMessageOrder.orderId & fromMaybeM (InvalidRequest "OrderId not present")
  paymentMethodInfo <- mkPaymentMethodInfo payment
  return $
    DUpdate.PaymentCompletedReq
      { bookingId = Id orderId,
        rideId = Id rideId,
        paymentStatus = castPaymentStatus payment.paymentStatus,
        paymentMethodInfo = paymentMethodInfo
      }

parseEditLocationEvent :: (MonadFlow m) => Spec.UpdateReqMessage -> m DUpdate.DUpdateReq
parseEditLocationEvent req = do
  rideId <- req.updateReqMessageOrder.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "RideId not present")
  fulfillment <- req.updateReqMessageOrder.orderFulfillments >>= listToMaybe & fromMaybeM (InvalidRequest "No Fulfillment present")
  bookingId <- (req.updateReqMessageOrder.orderId) & fromMaybeM (InvalidRequest "bookingId not present")
  stops <- fulfillment.fulfillmentStops & fromMaybeM (InvalidRequest "No Locations present")
  origin <- findLocationByStopType stops "START"
  destination <- findLocationByStopType stops "END"
  return $
    DUpdate.EditLocationReq
      { bookingId = Id bookingId,
        rideId = Id rideId,
        ..
      }
  where
    findLocationByStopType :: MonadFlow m => [Spec.Stop] -> Text -> m (Maybe DL.Location)
    findLocationByStopType stopsList stopType' = do
      let res = find (\stop -> stop.stopType == Just stopType') stopsList
      case res of
        Just stop -> maybe (throwError (InvalidRequest "Stop Locations not present")) (fmap Just . buildLocation) stop.stopLocation
        Nothing -> return Nothing

buildLocation :: MonadFlow m => Spec.Location -> m DL.Location
buildLocation location = do
  guid <- generateGUID
  now <- getCurrentTime
  gps <- case location.locationGps of
    Just gpsText -> case A.decode $ A.encode gpsText of
      Just decodedGps -> return decodedGps
      Nothing -> throwError (InvalidRequest "GPS decode failed")
    Nothing -> throwError (InvalidRequest "LatLons not present")
  return $
    DL.Location
      { DL.id = guid,
        createdAt = now,
        updatedAt = now,
        lat = case gps of
          Gps.Gps {Gps.lat = latitude} -> latitude,
        lon = case gps of
          Gps.Gps {Gps.lon = longitude} -> longitude,
        address =
          DL.LocationAddress
            { street = location.locationAddress,
              door = Nothing,
              city = Spec.cityName =<< location.locationCity,
              state = Spec.stateName =<< location.locationState,
              country = Spec.countryName =<< location.locationCountry,
              building = Nothing,
              areaCode = location.locationAreaCode,
              area = Nothing,
              fullAddress = Nothing
            }
      }

mkPaymentMethodInfo :: (MonadFlow m) => Spec.Payment -> m DMPM.PaymentMethodInfo
mkPaymentMethodInfo Spec.Payment {..} = do
  collectedBy <- Common.castPaymentCollector (fromMaybe "" paymentCollectedBy)
  paymentType' <- Common.castPaymentType (fromMaybe "" paymentType)
  return $
    DMPM.PaymentMethodInfo
      { collectedBy = collectedBy,
        paymentType = paymentType',
        paymentInstrument = DMPM.Cash
      }

castPaymentStatus :: Maybe Text -> DUpdate.PaymentStatus
castPaymentStatus (Just "PAID") = DUpdate.PAID
castPaymentStatus (Just "NOT_PAID") = DUpdate.NOT_PAID
castPaymentStatus _ = DUpdate.NOT_PAID
