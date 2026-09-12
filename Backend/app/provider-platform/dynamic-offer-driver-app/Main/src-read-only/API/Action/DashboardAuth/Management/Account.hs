{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Account
  ( API,
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
import qualified Tools.Auth.DashboardRegistration
import Tools.Auth.DashboardUserAuth

type API = ("account" :> (GetAccountFetchUnverifiedAccounts :<|> PostAccountVerifyAccount :<|> PutAccountUpdateRole))

type GetAccountFetchUnverifiedAccounts =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/ACCOUNT/GET_ACCOUNT_FETCH_UNVERIFIED_ACCOUNTS"
      :> API.Types.ProviderPlatform.Management.Account.GetAccountFetchUnverifiedAccounts
  )

type PostAccountVerifyAccount =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/ACCOUNT/POST_ACCOUNT_VERIFY_ACCOUNT"
      :> API.Types.ProviderPlatform.Management.Account.PostAccountVerifyAccount
  )

type PutAccountUpdateRole =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/ACCOUNT/PUT_ACCOUNT_UPDATE_ROLE"
      :> API.Types.ProviderPlatform.Management.Account.PutAccountUpdateRole
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getAccountFetchUnverifiedAccounts merchantId city :<|> postAccountVerifyAccount merchantId city :<|> putAccountUpdateRole merchantId city

getAccountFetchUnverifiedAccounts :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Account.FleetOwnerStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Account.UnverifiedAccountsResp)
getAccountFetchUnverifiedAccounts _a9 _a8 _a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    Tools.Auth.DashboardRegistration.listUnverifiedDashboardAccounts a6 a5 a4 a3 a2 a1

postAccountVerifyAccount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Account.VerifyAccountReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postAccountVerifyAccount a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Account.postAccountVerifyAccount a4 a3 a1

putAccountUpdateRole :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Person -> Kernel.Types.Id.Id Dashboard.Common.Role -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putAccountUpdateRole _a5 _a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardUserAuth.updateDashboardPersonRole a2.getId a1.getId
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success
