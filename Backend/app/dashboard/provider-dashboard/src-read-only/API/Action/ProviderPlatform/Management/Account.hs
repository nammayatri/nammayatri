{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.Account
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Account
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.Account
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("account" :> (GetAccountFetchUnverifiedAccounts :<|> PostAccountVerifyAccount :<|> PutAccountUpdateRole))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getAccountFetchUnverifiedAccounts merchantId city :<|> postAccountVerifyAccount merchantId city :<|> putAccountUpdateRole merchantId city

type GetAccountFetchUnverifiedAccounts =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.ACCOUNT / 'API.Types.ProviderPlatform.Management.Account.GET_ACCOUNT_FETCH_UNVERIFIED_ACCOUNTS)
      :> API.Types.ProviderPlatform.Management.Account.GetAccountFetchUnverifiedAccounts
  )

type PostAccountVerifyAccount =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.ACCOUNT / 'API.Types.ProviderPlatform.Management.Account.POST_ACCOUNT_VERIFY_ACCOUNT)
      :> API.Types.ProviderPlatform.Management.Account.PostAccountVerifyAccount
  )

type PutAccountUpdateRole =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.ACCOUNT / 'API.Types.ProviderPlatform.Management.Account.PUT_ACCOUNT_UPDATE_ROLE)
      :> API.Types.ProviderPlatform.Management.Account.PutAccountUpdateRole
  )

getAccountFetchUnverifiedAccounts :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Account.FleetOwnerStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Account.UnverifiedAccountsResp)
getAccountFetchUnverifiedAccounts merchantShortId opCity apiTokenInfo fromDate toDate mobileNumber status limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Account.getAccountFetchUnverifiedAccounts merchantShortId opCity apiTokenInfo fromDate toDate mobileNumber status limit offset

postAccountVerifyAccount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Account.VerifyAccountReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postAccountVerifyAccount merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Account.postAccountVerifyAccount merchantShortId opCity apiTokenInfo req

putAccountUpdateRole :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Person -> Kernel.Types.Id.Id Dashboard.Common.Role -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putAccountUpdateRole merchantShortId opCity apiTokenInfo personId roleId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Account.putAccountUpdateRole merchantShortId opCity apiTokenInfo personId roleId
