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
import Tools.Auth.Api

type API = ("payout" :> GetPayoutPayoutOrder)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPayoutPayoutOrder merchantId city

type GetPayoutPayoutOrder =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.PAYOUT) / ('API.Types.RiderPlatform.Management.Payout.GET_PAYOUT_PAYOUT_ORDER))
      :> API.Types.RiderPlatform.Management.Payout.GetPayoutPayoutOrder
  )

getPayoutPayoutOrder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutOrderResp)
getPayoutPayoutOrder merchantShortId opCity apiTokenInfo payoutOrderId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Payout.getPayoutPayoutOrder merchantShortId opCity apiTokenInfo payoutOrderId
