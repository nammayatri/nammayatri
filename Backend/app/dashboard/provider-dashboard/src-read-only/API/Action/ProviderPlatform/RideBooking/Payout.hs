{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.RideBooking.Payout
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.Payout
import qualified Domain.Action.ProviderPlatform.RideBooking.Payout
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("payout" :> (GetPayoutStatus :<|> PostPayoutCancel :<|> PostPayoutRetry :<|> PostPayoutMarkCashPaid))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPayoutStatus merchantId city :<|> postPayoutCancel merchantId city :<|> postPayoutRetry merchantId city :<|> postPayoutMarkCashPaid merchantId city

type GetPayoutStatus =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.PAYOUT / 'API.Types.Dashboard.RideBooking.Payout.GET_PAYOUT_STATUS)
      :> API.Types.Dashboard.RideBooking.Payout.GetPayoutStatus
  )

type PostPayoutCancel =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.PAYOUT / 'API.Types.Dashboard.RideBooking.Payout.POST_PAYOUT_CANCEL)
      :> API.Types.Dashboard.RideBooking.Payout.PostPayoutCancel
  )

type PostPayoutRetry =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.PAYOUT / 'API.Types.Dashboard.RideBooking.Payout.POST_PAYOUT_RETRY)
      :> API.Types.Dashboard.RideBooking.Payout.PostPayoutRetry
  )

type PostPayoutMarkCashPaid =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.PAYOUT / 'API.Types.Dashboard.RideBooking.Payout.POST_PAYOUT_MARK_CASH_PAID)
      :> API.Types.Dashboard.RideBooking.Payout.PostPayoutMarkCashPaid
  )

getPayoutStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.RideBooking.Payout.PayoutStatusResp)
getPayoutStatus merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Payout.getPayoutStatus merchantShortId opCity apiTokenInfo rideId

postPayoutCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> API.Types.Dashboard.RideBooking.Payout.PayoutCancelReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutCancel merchantShortId opCity apiTokenInfo rideId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Payout.postPayoutCancel merchantShortId opCity apiTokenInfo rideId req

postPayoutRetry :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutRetry merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Payout.postPayoutRetry merchantShortId opCity apiTokenInfo rideId

postPayoutMarkCashPaid :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutMarkCashPaid merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Payout.postPayoutMarkCashPaid merchantShortId opCity apiTokenInfo rideId
