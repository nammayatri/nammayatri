{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.Account
  ( getAccountFetchUnverifiedAccounts,
    postAccountVerifyAccount,
  )
where

import qualified API.Types.ProviderPlatform.Management.Account as Common
import Dashboard.Common (Person)
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
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import Tools.Auth

-- This function will not be called.
getAccountFetchUnverifiedAccounts ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Common.FleetOwnerStatus ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Environment.Flow [Common.PersonAPIEntity]
getAccountFetchUnverifiedAccounts _merchantShortId _opCity _mbFromDate _mbToDate _mbMobileNumber _mbStatus _mbLimit _mbOffset = do
  pure []

postAccountVerifyAccount ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.VerifyAccountReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postAccountVerifyAccount _merchantShortId _opCity Common.VerifyAccountReq {..} = do
  let enabled = case status of
        Common.Approved -> True
        _ -> False
  QFOI.updateFleetOwnerEnabledStatus enabled $ Kernel.Types.Id.cast fleetOwnerId
  pure Kernel.Types.APISuccess.Success
