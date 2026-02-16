{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Account
  ( API.Types.RiderPlatform.Management.Account.API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Account
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Account
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.Account.API)
handler merchantId city = putAccountUpdateRole merchantId city

putAccountUpdateRole :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Person -> Kernel.Types.Id.Id Dashboard.Common.Role -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putAccountUpdateRole a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Account.putAccountUpdateRole a4 a3 a2 a1
