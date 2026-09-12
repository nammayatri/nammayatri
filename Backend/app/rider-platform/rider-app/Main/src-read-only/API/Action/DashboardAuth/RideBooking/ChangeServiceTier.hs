{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.RideBooking.ChangeServiceTier
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.ChangeServiceTier
import qualified Domain.Action.Dashboard.RideBooking.ChangeServiceTier
import qualified "this" Domain.Action.UI.Quote
import qualified "this" Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("rideBooking" :> (GetChangeServiceTierQuotes :<|> PostChangeServiceTierConfirm))

type GetChangeServiceTierQuotes =
  ( DashboardUserAuth
      ('APP_BACKEND)
      "RIDER_RIDE_BOOKING/CHANGE_SERVICE_TIER/GET_CHANGE_SERVICE_TIER_QUOTES"
      :> API.Types.Dashboard.RideBooking.ChangeServiceTier.GetChangeServiceTierQuotes
  )

type PostChangeServiceTierConfirm =
  ( DashboardUserAuth
      ('APP_BACKEND)
      "RIDER_RIDE_BOOKING/CHANGE_SERVICE_TIER/POST_CHANGE_SERVICE_TIER_CONFIRM"
      :> API.Types.Dashboard.RideBooking.ChangeServiceTier.PostChangeServiceTierConfirm
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getChangeServiceTierQuotes merchantId city :<|> postChangeServiceTierConfirm merchantId city

getChangeServiceTierQuotes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler Domain.Action.UI.Quote.GetQuotesRes)
getChangeServiceTierQuotes a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.ChangeServiceTier.getChangeServiceTierQuotes a5 a4 a2 a1

postChangeServiceTierConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Dashboard.RideBooking.ChangeServiceTier.ChangeServiceTierConfirmReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postChangeServiceTierConfirm a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.ChangeServiceTier.postChangeServiceTierConfirm a6 a5 a3 a2 a1
