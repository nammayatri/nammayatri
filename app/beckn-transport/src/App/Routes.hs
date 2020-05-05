module App.Routes where

import qualified Beckn.Data.Accessor                  as Accessor
import           Types.App
import           Beckn.Types.Common
import           Data.Aeson
import qualified Data.Vault.Lazy                      as V
import           EulerHS.Prelude
import           Network.Wai.Parse
import           Servant
import           Servant.Multipart

type TransporterAPIs
    = "v1" :> (    Get '[ JSON] Text
              )

transporterAPIs :: Proxy TransporterAPIs
transporterAPIs = Proxy

transporterServer' :: V.Key (HashMap Text Text) -> FlowServer TransporterAPIs
transporterServer' key = pure "App is UP"