{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module App.Routes where

import Data.Aeson
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import Network.Wai.Parse
import qualified Product.Registration as Registration
import Servant
import Types.API.Registration
import Types.App
import           Beckn.Types.Core.Ack
import qualified Beckn.Types.API.Confirm as Confirm
import           Data.Aeson
import qualified Data.Vault.Lazy          as V
import           EulerHS.Prelude
import           Network.Wai.Parse
import           Servant
import           Types.App

type AppAPIs =
  "v1"
    :> ( Get '[JSON] Text
           :<|> RegistrationAPIs
       )

appAPIs :: Proxy AppAPIs
appAPIs = Proxy

appServer' :: V.Key (HashMap Text Text) -> FlowServer AppAPIs
appServer' key =
  pure "App is UP"
    :<|> registrationFlow

---- Registration Flow ------
type RegistrationAPIs =
  "token"
    :> ( ReqBody '[JSON] InitiateLoginReq
           :> Post '[JSON] InitiateLoginRes
           :<|> Capture "tokenId" Text
             :> "verify"
             :> ReqBody '[JSON] LoginReq
             :> Post '[JSON] LoginRes
           :<|> Capture "tokenId" Text
             :> "resend"
             :> ReqBody '[JSON] ReInitiateLoginReq
             :> Post '[JSON] InitiateLoginRes
       )

registrationFlow :: FlowServer RegistrationAPIs
registrationFlow =
  Registration.initiateLogin
    :<|> Registration.login
    :<|> Registration.reInitiateLogin

type ConfirmAPIs =
  (   "confirm"
   :> MandatoryQueryParam "caseId" Text
   :> MandatoryQueryParam "productId" Text
   :> Get '[JSON] Ack
  :<|>
      "on_confirm"
   :> ReqBody '[JSON] Confirm.OnConfirmReq
   :> Post '[JSON] Confirm.OnConfirmRes
  )
