{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Operator.Endpoints.Registration where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data CreateDashboardOperatorFleetAssignmentReq = CreateDashboardOperatorFleetAssignmentReq {fleetOwnerMobileCountryCode :: Kernel.Prelude.Text, fleetOwnerMobileNo :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateDashboardOperatorReq = CreateDashboardOperatorReq
  { email :: Kernel.Prelude.Text,
    firstName :: Kernel.Prelude.Text,
    fleetAssignment :: Kernel.Prelude.Maybe CreateDashboardOperatorFleetAssignmentReq,
    lastName :: Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    password :: Kernel.Prelude.Text,
    roleId :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OperatorRegisterReq = OperatorRegisterReq
  { email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    firstName :: Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype OperatorRegisterResp = OperatorRegisterResp {personId :: Kernel.Types.Id.Id Dashboard.Common.Operator}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OperatorRegisterTReq = OperatorRegisterTReq {firstName :: Kernel.Prelude.Text, lastName :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("operator" :> (PostRegistrationRegisterHelper :<|> PostRegistrationDashboardRegisterHelper))

type PostOperatorRegister = ("register" :> ReqBody '[JSON] OperatorRegisterReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostRegistrationRegisterHelper = ("register" :> ReqBody '[JSON] OperatorRegisterReq :> Post '[JSON] OperatorRegisterResp)

type PostRegistrationDashboardRegister = ("dashboard" :> "register" :> ReqBody '[JSON] CreateDashboardOperatorReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostRegistrationDashboardRegisterHelper = ("dashboard" :> "register" :> ReqBody '[JSON] CreateDashboardOperatorReq :> Post '[JSON] OperatorRegisterResp)

data RegistrationAPIs = RegistrationAPIs
  { postOperatorRegister :: OperatorRegisterReq -> EulerHS.Types.EulerClient OperatorRegisterResp,
    postRegistrationDashboardRegister :: CreateDashboardOperatorReq -> EulerHS.Types.EulerClient OperatorRegisterResp
  }

mkRegistrationAPIs :: (Client EulerHS.Types.EulerClient API -> RegistrationAPIs)
mkRegistrationAPIs registrationClient = (RegistrationAPIs {..})
  where
    postOperatorRegister :<|> postRegistrationDashboardRegister = registrationClient

data RegistrationUserActionType
  = POST_OPERATOR_REGISTER
  | POST_REGISTRATION_DASHBOARD_REGISTER
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''RegistrationUserActionType])
