module App.Routes where

import           Types.App
import           Epass.Types.Common
import qualified Product.Registration                 as Registration
import           Types.API.Registration

import           Data.Aeson
import qualified Data.Vault.Lazy                      as V
import           EulerHS.Prelude
import           Network.Wai.Parse
import           Servant
import           Servant.Multipart

type TransporterAPIs
    = "v1" :> (    Get '[ JSON] Text
              :<|> RegistrationAPIs
              )

---- Registration Flow ------
type RegistrationAPIs
   = "token"
   :> (    ReqBody '[ JSON] InitiateLoginReq
        :> Post '[ JSON] InitiateLoginRes
      :<|> Capture "tokenId" Text
        :> "verify"
        :> ReqBody '[ JSON] LoginReq
        :> Post '[ JSON] LoginRes
      :<|> Capture "tokenId" Text
        :> "resend"
        :> ReqBody '[ JSON] ReInitiateLoginReq
        :> Post '[ JSON] InitiateLoginRes
      )

registrationFlow :: FlowServer RegistrationAPIs
registrationFlow =
       Registration.initiateLogin
  :<|> Registration.login
  :<|> Registration.reInitiateLogin

-------------------------------

transporterAPIs :: Proxy TransporterAPIs
transporterAPIs = Proxy

transporterServer' :: V.Key (HashMap Text Text) -> FlowServer TransporterAPIs
transporterServer' key = 
                    pure "App is UP"
              :<|>  registrationFlow
