{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Account
  ( API.Types.ProviderPlatform.Management.Account.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Account
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.Account
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.Account.API)
handler merchantId city = getAccountFetchUnverifiedAccounts merchantId city :<|> postAccountVerifyAccount merchantId city :<|> putAccountUpdateRole merchantId city

getAccountFetchUnverifiedAccounts :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Account.FleetOwnerStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Account.UnverifiedAccountsResp)
getAccountFetchUnverifiedAccounts a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Account.getAccountFetchUnverifiedAccounts a8 a7 a6 a5 a4 a3 a2 a1

postAccountVerifyAccount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Account.VerifyAccountReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postAccountVerifyAccount a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Account.postAccountVerifyAccount a3 a2 a1

putAccountUpdateRole :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Person -> API.Types.ProviderPlatform.Management.Account.DashboardAccessType -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putAccountUpdateRole a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Account.putAccountUpdateRole a4 a3 a2 a1
