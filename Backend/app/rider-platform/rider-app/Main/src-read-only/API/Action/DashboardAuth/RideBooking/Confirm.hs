{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.RideBooking.Confirm
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Confirm
import qualified "this" API.UI.Confirm
import qualified Domain.Action.Dashboard.RideBooking.Confirm
import qualified "this" Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.Quote
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Interface
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("confirm" :> PostConfirmRideSearchQuotes)

type PostConfirmRideSearchQuotes =
  ( DashboardUserAuth
      ('APP_BACKEND)
      "RIDER_RIDE_BOOKING/CONFIRM/POST_CONFIRM_RIDE_SEARCH_QUOTES"
      :> API.Types.Dashboard.RideBooking.Confirm.PostConfirmRideSearchQuotes
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postConfirmRideSearchQuotes merchantId city

postConfirmRideSearchQuotes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Quote.Quote -> Kernel.Prelude.Maybe (Kernel.External.Payment.Interface.PaymentMethodId) -> Kernel.Prelude.Maybe (Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.UI.Confirm.ConfirmRes)
postConfirmRideSearchQuotes a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Confirm.postConfirmRideSearchQuotes a8 a7 a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a6)) a3 a2 a1
