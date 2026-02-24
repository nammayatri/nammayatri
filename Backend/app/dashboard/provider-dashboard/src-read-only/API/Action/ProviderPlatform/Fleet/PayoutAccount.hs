{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Fleet.PayoutAccount
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet
import qualified API.Types.ProviderPlatform.Fleet.PayoutAccount
import qualified Domain.Action.ProviderPlatform.Fleet.PayoutAccount
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("fleet" :> PostPayoutAccount)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postPayoutAccount merchantId city

type PostPayoutAccount =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_FLEET) / ('API.Types.ProviderPlatform.Fleet.PAYOUT_ACCOUNT) / ('API.Types.ProviderPlatform.Fleet.PayoutAccount.POST_PAYOUT_ACCOUNT))
      :> API.Types.ProviderPlatform.Fleet.PayoutAccount.PostPayoutAccount
  )

postPayoutAccount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountResp)
postPayoutAccount merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.PayoutAccount.postPayoutAccount merchantShortId opCity apiTokenInfo req
