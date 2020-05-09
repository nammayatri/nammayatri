module App.Routes where

import           Types.App
import           Beckn.Types.API.Search 
import           Beckn.Types.API.Confirm 
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

type SearchAPIs =
      "search" 
        :> "services"
        :> (    ReqBody '[ JSON] SearchReq
            :>  Post '[ JSON] SearchRes
            )   
 :<|> "on_search"
        :> "services"
        :> (    ReqBody '[ JSON] OnSearchReq
            :>  Post '[ JSON] OnSearchRes
            )  

type ConfirmAPIs =
      "confirm" 
        :> "services"
        :> (    ReqBody '[ JSON] ConfirmReq
            :>  Post '[ JSON] ConfirmRes
            )   
 :<|> "on_confirm"
        :> "services"
        :> (    ReqBody '[ JSON] OnConfirmReq
            :>  Post '[ JSON] OnConfirmRes
            )  
