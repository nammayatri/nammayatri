{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Account where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "lib-dashboard" Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data FleetOwnerStatus
  = Approved
  | Rejected
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VerifyAccountReq = VerifyAccountReq {status :: FleetOwnerStatus, reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text, fleetOwnerId :: Kernel.Types.Id.Id Domain.Types.Person.Person}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("account" :> (GetAccountFetchUnverifiedAccounts :<|> PostAccountVerifyAccount))

type GetAccountFetchUnverifiedAccounts =
  ( "fetchUnverifiedAccounts" :> QueryParam "fromDate" Kernel.Prelude.UTCTime :> QueryParam "toDate" Kernel.Prelude.UTCTime
      :> QueryParam
           "mobileNumber"
           Kernel.Prelude.Text
      :> QueryParam "status" FleetOwnerStatus
      :> Get ('[JSON]) [Domain.Types.Person.Person]
  )

type PostAccountVerifyAccount = ("verifyAccount" :> ReqBody ('[JSON]) VerifyAccountReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

data AccountAPIs = AccountAPIs
  { getAccountFetchUnverifiedAccounts :: (Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (FleetOwnerStatus) -> EulerHS.Types.EulerClient [Domain.Types.Person.Person]),
    postAccountVerifyAccount :: (VerifyAccountReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkAccountAPIs :: (Client EulerHS.Types.EulerClient API -> AccountAPIs)
mkAccountAPIs accountClient = (AccountAPIs {..})
  where
    getAccountFetchUnverifiedAccounts :<|> postAccountVerifyAccount = accountClient

data AccountUserActionType
  = GET_ACCOUNT_FETCH_UNVERIFIED_ACCOUNTS
  | POST_ACCOUNT_VERIFY_ACCOUNT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''AccountUserActionType)])
