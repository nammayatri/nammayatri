{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Management.Ride where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Ride as Common
import Data.Coerce (coerce)
import qualified Domain.Action.Dashboard.Ride as DRide
import qualified Domain.Action.UI.Ride.CancelRide as CHandler
import qualified Domain.Action.UI.Ride.EndRide as EHandler
import qualified Domain.Types.CancellationReason as DCReason
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common (Money, logTagInfo, withFlowHandlerAPI)
import Kernel.Utils.Validation (runRequestValidation)
import Servant hiding (Unauthorized, throwError)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

type API =
  "ride"
    :> ( Common.RideListAPI
           :<|> Common.MultipleRideEndAPI
           :<|> Common.MultipleRideCancelAPI
           :<|> Common.RideInfoAPI
           :<|> Common.RideSyncAPI
           :<|> Common.MultipleRideSyncAPI
           :<|> Common.RideRouteAPI
           :<|> Common.TicketRideListAPI
       )

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantId city =
  rideList merchantId city
    :<|> multipleRideEnd merchantId city
    :<|> multipleRideCancel merchantId city
    :<|> rideInfo merchantId city
    :<|> rideSync merchantId city
    :<|> multipleRideSync merchantId city
    :<|> rideRoute merchantId city
    :<|> ticketRideList merchantId city

rideList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  Maybe Money ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  FlowHandler Common.RideListRes
rideList merchantShortId opCity mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbFareDiff mbfrom mbto =
  withFlowHandlerAPI . DRide.rideList merchantShortId opCity mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbFareDiff mbfrom mbto

multipleRideEnd :: ShortId DM.Merchant -> Context.City -> Common.MultipleRideEndReq -> FlowHandler Common.MultipleRideEndResp
multipleRideEnd merchantShortId opCity req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMultipleRideEndReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  shandle <- EHandler.buildEndRideHandle merchant.id merchantOpCityId
  logTagInfo "dashboard -> multipleRideEnd : " $ show (req.rides <&> (.rideId))
  respItems <- forM req.rides $ \reqItem -> do
    info <- handle Common.listItemErrHandler $ do
      let rideId = cast @Common.Ride @DRide.Ride reqItem.rideId
      let cronJobReq =
            EHandler.CronJobEndRideReq
              { point = reqItem.point,
                merchantId = merchant.id
              }
      Success <- EHandler.cronJobEndRide shandle rideId cronJobReq
      pure Common.SuccessItem
    pure $ Common.MultipleRideSyncRespItem {rideId = reqItem.rideId, info}
  pure $ Common.MultipleRideSyncResp {list = respItems}

multipleRideCancel :: ShortId DM.Merchant -> Context.City -> Common.MultipleRideCancelReq -> FlowHandler Common.MultipleRideCancelResp
multipleRideCancel merchantShortId _ req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMultipleRideCancelReq req
  merchant <- findMerchantByShortId merchantShortId
  logTagInfo "dashboard -> multipleRideCancel : " $ show (req.rides <&> (.rideId))
  respItems <- forM req.rides $ \reqItem -> do
    info <- handle Common.listItemErrHandler $ do
      let rideId = cast @Common.Ride @DRide.Ride reqItem.rideId
      let dashboardReq =
            CHandler.CancelRideReq
              { reasonCode = coerce @Common.CancellationReasonCode @DCReason.CancellationReasonCode reqItem.reasonCode,
                additionalInfo = reqItem.additionalInfo
              }
      Success <- CHandler.dashboardCancelRideHandler CHandler.cancelRideHandle merchant.id rideId dashboardReq
      pure Common.SuccessItem
    pure $ Common.MultipleRideSyncRespItem {rideId = reqItem.rideId, info}
  pure $ Common.MultipleRideSyncResp {list = respItems}

rideInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> FlowHandler Common.RideInfoRes
rideInfo merchantShortId _ = withFlowHandlerAPI . DRide.rideInfo merchantShortId

rideSync :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> FlowHandler Common.RideSyncRes
rideSync merchantShortId _ = withFlowHandlerAPI . DRide.rideSync merchantShortId

multipleRideSync :: ShortId DM.Merchant -> Context.City -> Common.MultipleRideSyncReq -> FlowHandler Common.MultipleRideSyncRes
multipleRideSync merchantShortId _ = withFlowHandlerAPI . DRide.multipleRideSync merchantShortId

rideRoute :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> FlowHandler Common.RideRouteRes
rideRoute merchantShortId _ rideId = withFlowHandlerAPI $ DRide.rideRoute merchantShortId rideId

ticketRideList :: ShortId DM.Merchant -> Context.City -> Maybe (ShortId Common.Ride) -> Maybe Text -> Maybe Text -> Maybe Text -> FlowHandler Common.TicketRideListRes
ticketRideList merchantShortId _ mbRideShortId mbCountryCode mbPhoneNumber mbSupportPhoneNumber = withFlowHandlerAPI $ DRide.ticketRideList merchantShortId mbRideShortId mbCountryCode mbPhoneNumber mbSupportPhoneNumber
