{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.Passetto where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data PassettoDecryptReq = PassettoDecryptReq {encryptedValue :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PassettoDecryptReq where
  hideSecrets = Kernel.Prelude.identity

data PassettoDecryptResp = PassettoDecryptResp {value :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PassettoDecryptResp where
  hideSecrets = Kernel.Prelude.identity

data PassettoEncryptReq = PassettoEncryptReq {value :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PassettoEncryptReq where
  hideSecrets = Kernel.Prelude.identity

data PassettoEncryptResp = PassettoEncryptResp {encryptedValue :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("passetto" :> (PostPassettoEncrypt :<|> PostPassettoDecrypt))

type PostPassettoEncrypt = ("encrypt" :> ReqBody '[JSON] PassettoEncryptReq :> Post '[JSON] PassettoEncryptResp)

type PostPassettoDecrypt = ("decrypt" :> ReqBody '[JSON] PassettoDecryptReq :> Post '[JSON] PassettoDecryptResp)

data PassettoAPIs = PassettoAPIs
  { postPassettoEncrypt :: PassettoEncryptReq -> EulerHS.Types.EulerClient PassettoEncryptResp,
    postPassettoDecrypt :: PassettoDecryptReq -> EulerHS.Types.EulerClient PassettoDecryptResp
  }

mkPassettoAPIs :: (Client EulerHS.Types.EulerClient API -> PassettoAPIs)
mkPassettoAPIs passettoClient = (PassettoAPIs {..})
  where
    postPassettoEncrypt :<|> postPassettoDecrypt = passettoClient

data PassettoUserActionType
  = POST_PASSETTO_ENCRYPT
  | POST_PASSETTO_DECRYPT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''PassettoUserActionType])
