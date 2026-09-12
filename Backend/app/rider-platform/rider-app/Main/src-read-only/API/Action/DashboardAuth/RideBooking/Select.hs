{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.RideBooking.Select
  ( API,
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
import qualified "this" SharedLogic.Cancel
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("select" :> (PostSelectEstimate :<|> GetSelectQuotes :<|> GetSelectResult :<|> PostSelectCancelSearch))

type PostSelectEstimate = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/SELECT/POST_SELECT_ESTIMATE" :> API.Types.Dashboard.RideBooking.Select.PostSelectEstimate)

type GetSelectQuotes = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/SELECT/GET_SELECT_QUOTES" :> API.Types.Dashboard.RideBooking.Select.GetSelectQuotes)

type GetSelectResult = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/SELECT/GET_SELECT_RESULT" :> API.Types.Dashboard.RideBooking.Select.GetSelectResult)

type PostSelectCancelSearch = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/SELECT/POST_SELECT_CANCEL_SEARCH" :> API.Types.Dashboard.RideBooking.Select.PostSelectCancelSearch)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postSelectEstimate merchantId city :<|> getSelectQuotes merchantId city :<|> getSelectResult merchantId city :<|> postSelectCancelSearch merchantId city

postSelectEstimate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Domain.Action.UI.Select.DSelectReq -> Environment.FlowHandler Domain.Action.UI.Select.MultimodalSelectRes)
postSelectEstimate a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Select.postSelectEstimate a6 a5 a3 a2 a1

getSelectQuotes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Environment.FlowHandler Domain.Action.UI.Select.SelectListRes)
getSelectQuotes a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Select.getSelectQuotes a5 a4 a2 a1

getSelectResult :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Environment.FlowHandler Domain.Action.UI.Select.QuotesResultResponse)
getSelectResult a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Select.getSelectResult a5 a4 a2 a1

postSelectCancelSearch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Environment.FlowHandler SharedLogic.Cancel.CancelAPIResponse)
postSelectCancelSearch a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Select.postSelectCancelSearch a5 a4 a2 a1
