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

data DashboardAccessType
  = DASHBOARD_USER
  | DASHBOARD_ADMIN
  | FLEET_OWNER
  | DASHBOARD_RELEASE_ADMIN
  | MERCHANT_ADMIN
  | RENTAL_FLEET_OWNER
  | MERCHANT_MAKER
  | MERCHANT_SERVER
  | DASHBOARD_OPERATOR
  | TICKET_DASHBOARD_USER
  | TICKET_DASHBOARD_MERCHANT
  | TICKET_DASHBOARD_ADMIN
  | TICKET_DASHBOARD_APPROVER
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data FleetOwnerStatus
  = Approved
  | Rejected
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data PersonAPIEntity = PersonAPIEntity
  { id :: Kernel.Types.Id.Id Dashboard.Common.Person,
    firstName :: Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Text,
    roleAPIEntity :: RoleAPIEntity,
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Text,
    dashboardAccessType :: Kernel.Prelude.Maybe DashboardAccessType,
    createdAt :: Kernel.Prelude.UTCTime,
    receiveNotification :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    updatedAt :: Kernel.Prelude.UTCTime,
    verified :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    rejectionReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rejectedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RoleAPIEntity = RoleAPIEntity {id :: Kernel.Types.Id.Id Dashboard.Common.Role, name :: Kernel.Prelude.Text, dashboardAccessType :: DashboardAccessType, description :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UnverifiedAccountsResp = UnverifiedAccountsResp {listItems :: [PersonAPIEntity], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VerifyAccountReq = VerifyAccountReq {status :: FleetOwnerStatus, reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text, fleetOwnerId :: Kernel.Types.Id.Id Dashboard.Common.Person}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets VerifyAccountReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("account" :> (GetAccountFetchUnverifiedAccounts :<|> PostAccountVerifyAccount :<|> PutAccountUpdateRoleHelper))

type GetAccountFetchUnverifiedAccounts =
  ( "fetchUnverifiedAccounts" :> QueryParam "fromDate" Kernel.Prelude.UTCTime :> QueryParam "toDate" Kernel.Prelude.UTCTime
      :> QueryParam
           "mobileNumber"
           Kernel.Prelude.Text
      :> QueryParam "status" FleetOwnerStatus
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           UnverifiedAccountsResp
  )

type PostAccountVerifyAccount = ("verifyAccount" :> ReqBody '[JSON] VerifyAccountReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PutAccountUpdateRole =
  ( "updateRole" :> Capture "personId" (Kernel.Types.Id.Id Dashboard.Common.Person) :> Capture "roleId" (Kernel.Types.Id.Id Dashboard.Common.Role)
      :> Put
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PutAccountUpdateRoleHelper =
  ( "updateRole" :> Capture "personId" (Kernel.Types.Id.Id Dashboard.Common.Person) :> Capture "accessType" DashboardAccessType
      :> Put
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data AccountAPIs = AccountAPIs
  { getAccountFetchUnverifiedAccounts :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe FleetOwnerStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient UnverifiedAccountsResp,
    postAccountVerifyAccount :: VerifyAccountReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    putAccountUpdateRole :: Kernel.Types.Id.Id Dashboard.Common.Person -> DashboardAccessType -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkAccountAPIs :: (Client EulerHS.Types.EulerClient API -> AccountAPIs)
mkAccountAPIs accountClient = (AccountAPIs {..})
  where
    getAccountFetchUnverifiedAccounts :<|> postAccountVerifyAccount :<|> putAccountUpdateRole = accountClient

data AccountUserActionType
  = GET_ACCOUNT_FETCH_UNVERIFIED_ACCOUNTS
  | POST_ACCOUNT_VERIFY_ACCOUNT
  | PUT_ACCOUNT_UPDATE_ROLE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''DashboardAccessType)

$(mkHttpInstancesForEnum ''FleetOwnerStatus)

$(Data.Singletons.TH.genSingletons [''AccountUserActionType])
