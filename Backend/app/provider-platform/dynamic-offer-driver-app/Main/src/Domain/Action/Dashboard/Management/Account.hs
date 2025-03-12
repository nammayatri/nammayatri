{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.Account
  ( getAccountFetchUnverifiedAccounts,
    postAccountVerifyAccount,
  )
where

import qualified API.Types.ProviderPlatform.Management.Account
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

-- This function will not be called.
getAccountFetchUnverifiedAccounts ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Account.FleetOwnerStatus ->
  Environment.Flow [API.Types.ProviderPlatform.Management.Account.PersonAPIEntity]
getAccountFetchUnverifiedAccounts _merchantShortId _opCity _mbFromDate _mbToDate _mbMobileNumber _mbStatus = do
  pure []

postAccountVerifyAccount ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.ProviderPlatform.Management.Account.VerifyAccountReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postAccountVerifyAccount _merchantShortId _opCity _req = do
  -- Approve -> Enable the account and create at BPP
  -- Reject -> Delete the account and store reason of rejection in
  --  person table in another column rejection_reason.
  --    Store timestamp as well - rejected_at
  pure Kernel.Types.APISuccess.Success
