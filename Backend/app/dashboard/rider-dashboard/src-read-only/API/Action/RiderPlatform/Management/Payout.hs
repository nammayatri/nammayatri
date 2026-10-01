{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.Payout
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Payout
import qualified Domain.Action.RiderPlatform.Management.Payout
import "rider-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "payment" Lib.Payment.API.Payout.Types
import Servant
import Storage.Beam.CommonInstances ()

type API = ("payout" :> (GetPayoutPayoutOrder :<|> PostPayoutPayoutRetrigger))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPayoutPayoutOrder merchantId city :<|> postPayoutPayoutRetrigger merchantId city

type GetPayoutPayoutOrder =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.PAYOUT / 'API.Types.RiderPlatform.Management.Payout.GET_PAYOUT_PAYOUT_ORDER)
      :> API.Types.RiderPlatform.Management.Payout.GetPayoutPayoutOrder
  )

type PostPayoutPayoutRetrigger =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.PAYOUT / 'API.Types.RiderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_RETRIGGER)
      :> API.Types.RiderPlatform.Management.Payout.PostPayoutPayoutRetrigger
  )

getPayoutPayoutOrder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutOrderResp)
getPayoutPayoutOrder merchantShortId opCity apiTokenInfo payoutOrderId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Payout.getPayoutPayoutOrder merchantShortId opCity apiTokenInfo payoutOrderId

postPayoutPayoutRetrigger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.Management.Payout.RetriggerPayoutReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Payout.RetriggerPayoutResp)
postPayoutPayoutRetrigger merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Payout.postPayoutPayoutRetrigger merchantShortId opCity apiTokenInfo req
