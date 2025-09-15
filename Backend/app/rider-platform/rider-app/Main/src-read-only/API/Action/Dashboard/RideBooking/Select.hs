{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.RideBooking.Select
  ( API.Types.Dashboard.RideBooking.Select.API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Select
import qualified Domain.Action.Dashboard.RideBooking.Select
import qualified "this" Domain.Action.UI.Select
import qualified "this" Domain.Types.Estimate
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.Cancel
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.RideBooking.Select.API)
handler merchantId city = postSelectEstimate merchantId city :<|> getSelectQuotes merchantId city :<|> getSelectResult merchantId city :<|> postSelectCancelSearch merchantId city

postSelectEstimate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Domain.Action.UI.Select.DSelectReq -> Environment.FlowHandler Domain.Action.UI.Select.MultimodalSelectRes)
postSelectEstimate a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Select.postSelectEstimate a5 a4 a3 a2 a1

getSelectQuotes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Environment.FlowHandler Domain.Action.UI.Select.SelectListRes)
getSelectQuotes a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Select.getSelectQuotes a4 a3 a2 a1

getSelectResult :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Environment.FlowHandler Domain.Action.UI.Select.QuotesResultResponse)
getSelectResult a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Select.getSelectResult a4 a3 a2 a1

postSelectCancelSearch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Environment.FlowHandler SharedLogic.Cancel.CancelAPIResponse)
postSelectCancelSearch a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Select.postSelectCancelSearch a4 a3 a2 a1
