{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.Ride
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Ride
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.Ride
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("ride" :> (GetRideList :<|> GetRideListV2 :<|> PostRideEndMultiple :<|> PostRideCancelMultiple :<|> GetRideInfo :<|> PostRideSync :<|> PostRideSyncMultiple :<|> PostRideRoute :<|> GetRideKaptureList :<|> GetRideFareBreakUp :<|> PostRideWaiverRideCancellationPenalty))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRideList merchantId city :<|> getRideListV2 merchantId city :<|> postRideEndMultiple merchantId city :<|> postRideCancelMultiple merchantId city :<|> getRideInfo merchantId city :<|> postRideSync merchantId city :<|> postRideSyncMultiple merchantId city :<|> postRideRoute merchantId city :<|> getRideKaptureList merchantId city :<|> getRideFareBreakUp merchantId city :<|> postRideWaiverRideCancellationPenalty merchantId city

type GetRideList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RIDE) / ('API.Types.ProviderPlatform.Management.Ride.GET_RIDE_LIST))
      :> API.Types.ProviderPlatform.Management.Ride.GetRideList
  )

type GetRideListV2 =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RIDE) / ('API.Types.ProviderPlatform.Management.Ride.GET_RIDE_LIST_V2))
      :> API.Types.ProviderPlatform.Management.Ride.GetRideListV2
  )

type PostRideEndMultiple =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RIDE) / ('API.Types.ProviderPlatform.Management.Ride.POST_RIDE_END_MULTIPLE))
      :> API.Types.ProviderPlatform.Management.Ride.PostRideEndMultiple
  )

type PostRideCancelMultiple =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RIDE) / ('API.Types.ProviderPlatform.Management.Ride.POST_RIDE_CANCEL_MULTIPLE))
      :> API.Types.ProviderPlatform.Management.Ride.PostRideCancelMultiple
  )

type GetRideInfo =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RIDE) / ('API.Types.ProviderPlatform.Management.Ride.GET_RIDE_INFO))
      :> API.Types.ProviderPlatform.Management.Ride.GetRideInfo
  )

type PostRideSync =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RIDE) / ('API.Types.ProviderPlatform.Management.Ride.POST_RIDE_SYNC))
      :> API.Types.ProviderPlatform.Management.Ride.PostRideSync
  )

type PostRideSyncMultiple =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RIDE) / ('API.Types.ProviderPlatform.Management.Ride.POST_RIDE_SYNC_MULTIPLE))
      :> API.Types.ProviderPlatform.Management.Ride.PostRideSyncMultiple
  )

type PostRideRoute =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RIDE) / ('API.Types.ProviderPlatform.Management.Ride.POST_RIDE_ROUTE))
      :> API.Types.ProviderPlatform.Management.Ride.PostRideRoute
  )

type GetRideKaptureList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RIDE) / ('API.Types.ProviderPlatform.Management.Ride.GET_RIDE_KAPTURE_LIST))
      :> API.Types.ProviderPlatform.Management.Ride.GetRideKaptureList
  )

type GetRideFareBreakUp =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RIDE) / ('API.Types.ProviderPlatform.Management.Ride.GET_RIDE_FARE_BREAK_UP))
      :> API.Types.ProviderPlatform.Management.Ride.GetRideFareBreakUp
  )

type PostRideWaiverRideCancellationPenalty =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RIDE) / ('API.Types.ProviderPlatform.Management.Ride.POST_RIDE_WAIVER_RIDE_CANCELLATION_PENALTY))
      :> API.Types.ProviderPlatform.Management.Ride.PostRideWaiverRideCancellationPenalty
  )

getRideList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Ride.BookingStatus) -> Kernel.Prelude.Maybe (Kernel.Types.Common.Currency) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideListRes)
getRideList merchantShortId opCity apiTokenInfo bookingStatus currency customerPhoneNo driverPhoneNo from limit offset rideShortId to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Ride.getRideList merchantShortId opCity apiTokenInfo bookingStatus currency customerPhoneNo driverPhoneNo from limit offset rideShortId to

getRideListV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Common.Currency) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Ride.RideStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideListResV2)
getRideListV2 merchantShortId opCity apiTokenInfo currency customerPhoneNo driverPhoneNo from limit offset rideShortId rideStatus to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Ride.getRideListV2 merchantShortId opCity apiTokenInfo currency customerPhoneNo driverPhoneNo from limit offset rideShortId rideStatus to

postRideEndMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Ride.MultipleRideEndReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.MultipleRideEndResp)
postRideEndMultiple merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Ride.postRideEndMultiple merchantShortId opCity apiTokenInfo req

postRideCancelMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Ride.MultipleRideCancelReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.MultipleRideCancelResp)
postRideCancelMultiple merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Ride.postRideCancelMultiple merchantShortId opCity apiTokenInfo req

getRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideInfoRes)
getRideInfo merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Ride.getRideInfo merchantShortId opCity apiTokenInfo rideId

postRideSync :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideSyncRes)
postRideSync merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Ride.postRideSync merchantShortId opCity apiTokenInfo rideId

postRideSyncMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Ride.MultipleRideSyncReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.MultipleRideSyncRes)
postRideSyncMultiple merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Ride.postRideSyncMultiple merchantShortId opCity apiTokenInfo req

postRideRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.RideRouteRes)
postRideRoute merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Ride.postRideRoute merchantShortId opCity apiTokenInfo rideId

getRideKaptureList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.TicketRideListRes)
getRideKaptureList merchantShortId opCity apiTokenInfo rideShortId countryCode phoneNumber supportPhoneNumber = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Ride.getRideKaptureList merchantShortId opCity apiTokenInfo rideShortId countryCode phoneNumber supportPhoneNumber

getRideFareBreakUp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Ride.FareBreakUpRes)
getRideFareBreakUp merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Ride.getRideFareBreakUp merchantShortId opCity apiTokenInfo rideId

postRideWaiverRideCancellationPenalty :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Ride -> API.Types.ProviderPlatform.Management.Ride.WaiverRideCancellationPenaltyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideWaiverRideCancellationPenalty merchantShortId opCity apiTokenInfo rideId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Ride.postRideWaiverRideCancellationPenalty merchantShortId opCity apiTokenInfo rideId req
