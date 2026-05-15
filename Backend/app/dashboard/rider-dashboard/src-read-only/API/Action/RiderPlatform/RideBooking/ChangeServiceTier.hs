{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.RideBooking.ChangeServiceTier
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking
import qualified "rider-app" API.Types.Dashboard.RideBooking.ChangeServiceTier
import qualified Domain.Action.RiderPlatform.RideBooking.ChangeServiceTier
import qualified "rider-app" Domain.Action.UI.Quote
import qualified "rider-app" Domain.Types.Booking
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("rideBooking" :> (GetChangeServiceTierQuotes :<|> PostChangeServiceTierConfirm))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getChangeServiceTierQuotes merchantId city :<|> postChangeServiceTierConfirm merchantId city

type GetChangeServiceTierQuotes =
  ( ApiAuth
      ('APP_BACKEND)
      ('DSL)
      (('RIDER_RIDE_BOOKING) / ('API.Types.Dashboard.RideBooking.CHANGE_SERVICE_TIER) / ('API.Types.Dashboard.RideBooking.ChangeServiceTier.GET_CHANGE_SERVICE_TIER_QUOTES))
      :> API.Types.Dashboard.RideBooking.ChangeServiceTier.GetChangeServiceTierQuotes
  )

type PostChangeServiceTierConfirm =
  ( ApiAuth
      ('APP_BACKEND)
      ('DSL)
      (('RIDER_RIDE_BOOKING) / ('API.Types.Dashboard.RideBooking.CHANGE_SERVICE_TIER) / ('API.Types.Dashboard.RideBooking.ChangeServiceTier.POST_CHANGE_SERVICE_TIER_CONFIRM))
      :> API.Types.Dashboard.RideBooking.ChangeServiceTier.PostChangeServiceTierConfirm
  )

getChangeServiceTierQuotes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler Domain.Action.UI.Quote.GetQuotesRes)
getChangeServiceTierQuotes merchantShortId opCity apiTokenInfo bookingId customerId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.ChangeServiceTier.getChangeServiceTierQuotes merchantShortId opCity apiTokenInfo bookingId customerId

postChangeServiceTierConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Dashboard.RideBooking.ChangeServiceTier.ChangeServiceTierConfirmReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postChangeServiceTierConfirm merchantShortId opCity apiTokenInfo bookingId customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.ChangeServiceTier.postChangeServiceTierConfirm merchantShortId opCity apiTokenInfo bookingId customerId req
