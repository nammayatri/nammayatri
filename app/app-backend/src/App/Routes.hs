module App.Routes where

import Data.Aeson
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import Network.Wai.Parse
import Servant
import Types.App

type AppAPIs =
  "v1"
    :> ( Get '[JSON] Text
       )

appAPIs :: Proxy AppAPIs
appAPIs = Proxy

appServer' :: V.Key (HashMap Text Text) -> FlowServer AppAPIs
appServer' key =
  pure "App is UP"
