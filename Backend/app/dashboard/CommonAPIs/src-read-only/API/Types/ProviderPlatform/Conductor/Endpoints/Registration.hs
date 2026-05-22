{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Conductor.Endpoints.Registration where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import Servant
import Servant.Client

data ConductorBulkRegisterReq = ConductorBulkRegisterReq {conductors :: [ConductorRegisterReq]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ConductorBulkRegisterResp = ConductorBulkRegisterResp {results :: [ConductorBulkRegisterResult]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ConductorBulkRegisterResult = ConductorBulkRegisterResult {operatorBadgeToken :: Kernel.Prelude.Text, success :: Kernel.Prelude.Bool, errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ConductorBulkRegisterTReq = ConductorBulkRegisterTReq {conductors :: [ConductorRegisterTReq]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ConductorRegisterReq = ConductorRegisterReq {operatorBadgeToken :: Kernel.Prelude.Text, firstName :: Kernel.Prelude.Text, email :: Kernel.Prelude.Text, password :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ConductorRegisterResp = ConductorRegisterResp {success :: Kernel.Prelude.Bool, operatorBadgeToken :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ConductorRegisterTReq = ConductorRegisterTReq {operatorBadgeToken :: Kernel.Prelude.Text, firstName :: Kernel.Prelude.Text, email :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("conductor" :> (PostConductorRegister :<|> PostConductorBulkRegister))

type PostConductorRegister = ("register" :> ReqBody '[JSON] ConductorRegisterReq :> Post '[JSON] ConductorRegisterResp)

type PostConductorBulkRegister = ("bulkRegister" :> ReqBody '[JSON] ConductorBulkRegisterReq :> Post '[JSON] ConductorBulkRegisterResp)

data RegistrationAPIs = RegistrationAPIs
  { postConductorRegister :: ConductorRegisterReq -> EulerHS.Types.EulerClient ConductorRegisterResp,
    postConductorBulkRegister :: ConductorBulkRegisterReq -> EulerHS.Types.EulerClient ConductorBulkRegisterResp
  }

mkRegistrationAPIs :: (Client EulerHS.Types.EulerClient API -> RegistrationAPIs)
mkRegistrationAPIs registrationClient = (RegistrationAPIs {..})
  where
    postConductorRegister :<|> postConductorBulkRegister = registrationClient

data RegistrationUserActionType
  = POST_CONDUCTOR_REGISTER
  | POST_CONDUCTOR_BULK_REGISTER
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''RegistrationUserActionType])
