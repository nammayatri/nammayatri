{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.Account
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Account
import qualified Dashboard.Common
import qualified Domain.Action.RiderPlatform.Management.Account
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("account" :> PutAccountUpdateRole)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = putAccountUpdateRole merchantId city

type PutAccountUpdateRole =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.ACCOUNT) / ('API.Types.RiderPlatform.Management.Account.PUT_ACCOUNT_UPDATE_ROLE))
      :> API.Types.RiderPlatform.Management.Account.PutAccountUpdateRole
  )

putAccountUpdateRole :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Person -> Kernel.Types.Id.Id Dashboard.Common.Role -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putAccountUpdateRole merchantShortId opCity apiTokenInfo personId roleId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Account.putAccountUpdateRole merchantShortId opCity apiTokenInfo personId roleId
