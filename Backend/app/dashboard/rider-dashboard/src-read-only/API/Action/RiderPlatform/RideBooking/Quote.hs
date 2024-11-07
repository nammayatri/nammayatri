{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.RideBooking.Quote
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking
import qualified "rider-app" API.Types.Dashboard.RideBooking.Quote
import qualified Domain.Action.RiderPlatform.RideBooking.Quote
import qualified "rider-app" Domain.Action.UI.Quote
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "rider-app" Domain.Types.SearchRequest
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("quote" :> GetQuoteResult)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getQuoteResult merchantId city

type GetQuoteResult =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.QUOTE / 'API.Types.Dashboard.RideBooking.Quote.GET_QUOTE_RESULT)
      :> API.Types.Dashboard.RideBooking.Quote.GetQuoteResult
  )

getQuoteResult :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler Domain.Action.UI.Quote.GetQuotesRes)
getQuoteResult merchantShortId opCity apiTokenInfo searchId customerId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Quote.getQuoteResult merchantShortId opCity apiTokenInfo searchId customerId
