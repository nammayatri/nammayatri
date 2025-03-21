{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Operator.Endpoints.Registration where

import qualified Dashboard.Common
import qualified Data.Aeson
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

type API = ("operator" :> PostRegistrationRegisterHelper)

type PostOperatorRegister = ("register" :> ReqBody '[JSON] OperatorRegisterReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostRegistrationRegisterHelper = ("register" :> ReqBody '[JSON] OperatorRegisterReq :> Post '[JSON] OperatorRegisterResp)

newtype RegistrationAPIs = RegistrationAPIs {postOperatorRegister :: OperatorRegisterReq -> EulerHS.Types.EulerClient OperatorRegisterResp}

mkRegistrationAPIs :: (Client EulerHS.Types.EulerClient API -> RegistrationAPIs)
mkRegistrationAPIs registrationClient = (RegistrationAPIs {..})
  where
    postOperatorRegister = registrationClient

data RegistrationUserActionType
  = POST_OPERATOR_REGISTER
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON RegistrationUserActionType where
  toJSON POST_OPERATOR_REGISTER = Data.Aeson.String "POST_OPERATOR_REGISTER"

instance FromJSON RegistrationUserActionType where
  parseJSON (Data.Aeson.String "POST_OPERATOR_REGISTER") = pure POST_OPERATOR_REGISTER
  parseJSON _ = fail "POST_OPERATOR_REGISTER expected"

$(Data.Singletons.TH.genSingletons [''RegistrationUserActionType])
