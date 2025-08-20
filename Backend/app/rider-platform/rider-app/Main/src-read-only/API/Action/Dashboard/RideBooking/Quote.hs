{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.RideBooking.Quote
  ( API.Types.Dashboard.RideBooking.Quote.API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Quote
import qualified Domain.Action.Dashboard.RideBooking.Quote
import qualified "this" Domain.Action.UI.Quote
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.SearchRequest
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.RideBooking.Quote.API)
handler merchantId city = getQuoteResult merchantId city

getQuoteResult :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler Domain.Action.UI.Quote.GetQuotesRes)
getQuoteResult a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Quote.getQuoteResult a4 a3 a2 a1
