module Domain.Action.ProviderPlatform.RideBooking.Ride
  ( postRideStart,
    postRideEnd,
    getRideCurrentActiveRide,
    postRideCancel,
    postRideBookingWithVehicleNumberAndPhone,
  )
where

import qualified API.Client.ProviderPlatform.RideBooking
import qualified API.Types.Dashboard.RideBooking.Ride
import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

postRideStart :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> API.Types.Dashboard.RideBooking.Ride.StartRideReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postRideStart merchantShortId opCity apiTokenInfo rideId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing (Kernel.Prelude.Just rideId) (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.rideDSL.postRideStart) rideId req

postRideEnd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> API.Types.Dashboard.RideBooking.Ride.EndRideReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postRideEnd merchantShortId opCity apiTokenInfo rideId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing (Kernel.Prelude.Just rideId) (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.rideDSL.postRideEnd) rideId req

getRideCurrentActiveRide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow (Kernel.Types.Id.Id Dashboard.Common.Ride))
getRideCurrentActiveRide merchantShortId opCity apiTokenInfo vehicleNumber = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.rideDSL.getRideCurrentActiveRide) vehicleNumber

postRideCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> API.Types.Dashboard.RideBooking.Ride.CancelRideReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postRideCancel merchantShortId opCity apiTokenInfo rideId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing (Kernel.Prelude.Just rideId) (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.rideDSL.postRideCancel) rideId req

postRideBookingWithVehicleNumberAndPhone :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.RideBooking.Ride.BookingWithVehicleAndPhoneReq -> Environment.Flow API.Types.Dashboard.RideBooking.Ride.BookingWithVehicleAndPhoneRes)
postRideBookingWithVehicleNumberAndPhone merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.rideDSL.postRideBookingWithVehicleNumberAndPhone) req
