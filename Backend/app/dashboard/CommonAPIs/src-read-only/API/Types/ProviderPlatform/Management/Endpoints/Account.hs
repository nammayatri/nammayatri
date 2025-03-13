{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Account where

import qualified Dashboard.Common
import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import Servant
import Servant.Client

data FleetOwnerStatus
  = Approved
  | Rejected
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data PersonAPIEntity = PersonAPIEntity
  { id :: Kernel.Types.Id.Id Dashboard.Common.Person,
    firstName :: Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Text,
    roleId :: Kernel.Types.Id.Id Dashboard.Common.Role,
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    receiveNotification :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    updatedAt :: Kernel.Prelude.UTCTime,
    verified :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    rejectionReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rejectedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VerifyAccountReq = VerifyAccountReq {status :: FleetOwnerStatus, reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text, fleetOwnerId :: Kernel.Types.Id.Id Dashboard.Common.Person}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets VerifyAccountReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("account" :> (GetAccountFetchUnverifiedAccounts :<|> PostAccountVerifyAccount))

type GetAccountFetchUnverifiedAccounts =
  ( "fetchUnverifiedAccounts" :> QueryParam "fromDate" Kernel.Prelude.UTCTime :> QueryParam "toDate" Kernel.Prelude.UTCTime
      :> QueryParam
           "mobileNumber"
           Kernel.Prelude.Text
      :> QueryParam "status" FleetOwnerStatus
      :> Get '[JSON] [PersonAPIEntity]
  )

type PostAccountVerifyAccount = ("verifyAccount" :> ReqBody '[JSON] VerifyAccountReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data AccountAPIs = AccountAPIs
  { getAccountFetchUnverifiedAccounts :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe FleetOwnerStatus -> EulerHS.Types.EulerClient [PersonAPIEntity],
    postAccountVerifyAccount :: VerifyAccountReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
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

$(mkHttpInstancesForEnum ''FleetOwnerStatus)

$(Data.Singletons.TH.genSingletons [''AccountUserActionType])
