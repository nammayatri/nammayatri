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
import "lib-dashboard" Storage.Queries.Person (findAllByFromDateAndToDateAndMobileNumberAndStatus)
import Tools.Auth

getAccountFetchUnverifiedAccounts ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Account.FleetOwnerStatus ->
  Environment.Flow [Domain.Types.Person.Person]
getAccountFetchUnverifiedAccounts _merchantShortId _opCity mbFromDate mbToDate mbMobileNumber mbStatus = do
  findAllByFromDateAndToDateAndMobileNumberAndStatus mbFromDate mbToDate mbMobileNumber mbStatus

postAccountVerifyAccount ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.ProviderPlatform.Management.Account.VerifyAccountReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postAccountVerifyAccount _merchantShortId _opCity req = do
  pure Kernel.Types.APISuccess.Success
