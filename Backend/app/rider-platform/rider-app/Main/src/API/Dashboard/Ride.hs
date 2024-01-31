{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Ride where

import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Ride as Common
import qualified Domain.Action.Dashboard.Ride as DRide
import Domain.Action.Dashboard.Route (mkGetLocation)
import qualified Domain.Types.Merchant as DM
import Environment
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import Servant hiding (throwError)
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.SystemConfigs ()

type API =
  "ride"
    :> ( ShareRideInfoAPI
           :<|> Common.ShareRideInfoByShortIdAPI
           :<|> Common.RideListAPI
           :<|> Common.TripRouteAPI
           :<|> Common.RideInfoAPI
           :<|> MultipleRideCancelAPI
           :<|> Common.MultipleRideSyncAPI
           :<|> Common.TicketRideListAPI
       )

type ShareRideInfoAPI = Common.ShareRideInfoAPI

type MultipleRideCancelAPI =
  "cancel"
    :> ReqBody '[JSON] DRide.MultipleRideCancelReq
    :> Post '[JSON] APISuccess

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  shareRideInfo merchantId
    :<|> shareRideInfoByShortId merchantId
    :<|> rideList merchantId
    :<|> callGetTripRoute merchantId
    :<|> callRideInfo merchantId
    :<|> multipleRideCancel -- FIXME merchantId ?
    :<|> multipleRideSync merchantId
    :<|> ticketRideList merchantId

shareRideInfo ::
  ShortId DM.Merchant ->
  Id Common.Ride ->
  FlowHandler Common.ShareRideInfoRes
shareRideInfo merchantShortId reqRideId = withFlowHandlerAPI $ DRide.shareRideInfo merchantShortId reqRideId

shareRideInfoByShortId ::
  ShortId DM.Merchant ->
  ShortId Common.Ride ->
  FlowHandler Common.ShareRideInfoRes
shareRideInfoByShortId merchantShortId reqRideShortId = withFlowHandlerAPI $ DRide.shareRideInfoByShortId merchantShortId reqRideShortId

rideList ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  FlowHandler Common.RideListRes
rideList merchantShortId mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone mbFrom mbTo =
  withFlowHandlerAPI $ DRide.rideList merchantShortId mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone mbFrom mbTo

callGetTripRoute :: ShortId DM.Merchant -> Id Common.Ride -> Double -> Double -> FlowHandler Maps.GetRoutesResp
callGetTripRoute merchantShortId rideId pickupLocationLat pickupLocationLon = withFlowHandlerAPI $ mkGetLocation merchantShortId rideId pickupLocationLat pickupLocationLon

callRideInfo ::
  ShortId DM.Merchant ->
  Id Common.Ride ->
  FlowHandler Common.RideInfoRes
callRideInfo merchantShortId rideId = withFlowHandlerAPI $ do
  merchant <- findMerchantByShortId merchantShortId
  DRide.rideInfo merchant.id rideId

multipleRideCancel ::
  DRide.MultipleRideCancelReq ->
  FlowHandler APISuccess
multipleRideCancel = withFlowHandlerAPI . DRide.multipleRideCancel

multipleRideSync ::
  ShortId DM.Merchant ->
  Common.MultipleRideSyncReq ->
  FlowHandler Common.MultipleRideSyncResp
multipleRideSync merchantShortId req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMultipleRideSyncReq req
  merchant <- findMerchantByShortId merchantShortId
  logTagInfo "dashboard -> multipleRideSync : " $ show (req.rides <&> (.rideId))
  respItems <- forM req.rides $ \reqItem -> do
    info <- handle Common.listItemErrHandler $ do
      void $ DRide.rideSync merchant reqItem.rideId
      pure Common.SuccessItem
    pure $ Common.MultipleRideSyncRespItem {rideId = reqItem.rideId, info}
  pure $ Common.MultipleRideSyncResp {list = respItems}

ticketRideList :: ShortId DM.Merchant -> Maybe (ShortId Common.Ride) -> Maybe Text -> Maybe Text -> Maybe Text -> FlowHandler Common.TicketRideListRes
ticketRideList merchantShortId mbRideShortId mbCountryCode mbPhoneNumber mbSupportPhoneNumber = withFlowHandlerAPI $ DRide.ticketRideList merchantShortId mbRideShortId mbCountryCode mbPhoneNumber mbSupportPhoneNumber
