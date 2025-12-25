{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.RideBooking.Confirm
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking
import qualified "rider-app" API.Types.Dashboard.RideBooking.Confirm
import qualified "rider-app" API.UI.Confirm
import qualified Domain.Action.RiderPlatform.RideBooking.Confirm
import qualified "rider-app" Domain.Types.Extra.MerchantPaymentMethod
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "rider-app" Domain.Types.Quote
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Interface
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("confirm" :> PostConfirmRideSearchQuotes)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postConfirmRideSearchQuotes merchantId city

type PostConfirmRideSearchQuotes =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.CONFIRM / 'API.Types.Dashboard.RideBooking.Confirm.POST_CONFIRM_RIDE_SEARCH_QUOTES)
      :> API.Types.Dashboard.RideBooking.Confirm.PostConfirmRideSearchQuotes
  )

postConfirmRideSearchQuotes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Quote.Quote -> Kernel.Prelude.Maybe Kernel.External.Payment.Interface.PaymentMethodId -> Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.FlowHandler API.UI.Confirm.ConfirmRes)
postConfirmRideSearchQuotes merchantShortId opCity apiTokenInfo customerId quoteId paymentMethodId paymentInstrument isAdvancedBookingEnabled = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Confirm.postConfirmRideSearchQuotes merchantShortId opCity apiTokenInfo customerId quoteId paymentMethodId paymentInstrument isAdvancedBookingEnabled
