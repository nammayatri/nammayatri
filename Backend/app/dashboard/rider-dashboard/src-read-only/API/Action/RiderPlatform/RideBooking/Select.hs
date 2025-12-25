{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.RideBooking.Select
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking
import qualified "rider-app" API.Types.Dashboard.RideBooking.Select
import qualified Domain.Action.RiderPlatform.RideBooking.Select
import qualified "rider-app" Domain.Action.UI.Select
import qualified "rider-app" Domain.Types.Estimate
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.Cancel
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("select" :> (PostSelectEstimate :<|> GetSelectQuotes :<|> GetSelectResult :<|> PostSelectCancelSearch))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postSelectEstimate merchantId city :<|> getSelectQuotes merchantId city :<|> getSelectResult merchantId city :<|> postSelectCancelSearch merchantId city

type PostSelectEstimate =
  ( ApiAuth
      ('APP_BACKEND)
      ('DSL)
      (('RIDER_RIDE_BOOKING) / ('API.Types.Dashboard.RideBooking.SELECT) / ('API.Types.Dashboard.RideBooking.Select.POST_SELECT_ESTIMATE))
      :> API.Types.Dashboard.RideBooking.Select.PostSelectEstimate
  )

type GetSelectQuotes =
  ( ApiAuth
      ('APP_BACKEND)
      ('DSL)
      (('RIDER_RIDE_BOOKING) / ('API.Types.Dashboard.RideBooking.SELECT) / ('API.Types.Dashboard.RideBooking.Select.GET_SELECT_QUOTES))
      :> API.Types.Dashboard.RideBooking.Select.GetSelectQuotes
  )

type GetSelectResult =
  ( ApiAuth
      ('APP_BACKEND)
      ('DSL)
      (('RIDER_RIDE_BOOKING) / ('API.Types.Dashboard.RideBooking.SELECT) / ('API.Types.Dashboard.RideBooking.Select.GET_SELECT_RESULT))
      :> API.Types.Dashboard.RideBooking.Select.GetSelectResult
  )

type PostSelectCancelSearch =
  ( ApiAuth
      ('APP_BACKEND)
      ('DSL)
      (('RIDER_RIDE_BOOKING) / ('API.Types.Dashboard.RideBooking.SELECT) / ('API.Types.Dashboard.RideBooking.Select.POST_SELECT_CANCEL_SEARCH))
      :> API.Types.Dashboard.RideBooking.Select.PostSelectCancelSearch
  )

postSelectEstimate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Domain.Action.UI.Select.DSelectReq -> Environment.FlowHandler Domain.Action.UI.Select.MultimodalSelectRes)
postSelectEstimate merchantShortId opCity apiTokenInfo customerId estimateId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Select.postSelectEstimate merchantShortId opCity apiTokenInfo customerId estimateId req

getSelectQuotes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Environment.FlowHandler Domain.Action.UI.Select.SelectListRes)
getSelectQuotes merchantShortId opCity apiTokenInfo customerId estimateId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Select.getSelectQuotes merchantShortId opCity apiTokenInfo customerId estimateId

getSelectResult :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Environment.FlowHandler Domain.Action.UI.Select.QuotesResultResponse)
getSelectResult merchantShortId opCity apiTokenInfo customerId estimateId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Select.getSelectResult merchantShortId opCity apiTokenInfo customerId estimateId

postSelectCancelSearch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Environment.FlowHandler SharedLogic.Cancel.CancelAPIResponse)
postSelectCancelSearch merchantShortId opCity apiTokenInfo customerId estimateId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Select.postSelectCancelSearch merchantShortId opCity apiTokenInfo customerId estimateId
