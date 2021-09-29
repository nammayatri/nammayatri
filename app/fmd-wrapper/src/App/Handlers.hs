module App.Handlers where

import App.Types
import Beckn.Types.Registry.Routes (OnSubscribeAPI)
import Beckn.Utils.Servant.SignatureAuth
import EulerHS.Prelude
import qualified Product.APIMapper as API
import Product.OnSubscribe (onSubscribe)
import Servant
import Types.Beckn.API.Cancel (CancelAPI)
import Types.Beckn.API.Confirm (ConfirmAPI)
import Types.Beckn.API.Init (InitAPI)
import Types.Beckn.API.Search (SearchAPI)
import Types.Beckn.API.Select (SelectAPI)
import Types.Beckn.API.Status (StatusAPI)
import Types.Beckn.API.Track (TrackAPI)
import Types.Beckn.API.Update (UpdateAPI)
import Utils.Auth

type WrapperAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> SignatureAuth "Authorization" LookupRegistryOrg :> SearchAPI
           :<|> SignatureAuth "Authorization" LookupRegistryOrg :> SelectAPI
           :<|> SignatureAuth "Authorization" LookupRegistryOrg :> InitAPI
           :<|> SignatureAuth "Authorization" LookupRegistryOrg :> ConfirmAPI
           :<|> SignatureAuth "Authorization" LookupRegistryOrg :> StatusAPI
           :<|> SignatureAuth "Authorization" LookupRegistryOrg :> TrackAPI
           :<|> SignatureAuth "Authorization" LookupRegistryOrg :> CancelAPI
           :<|> SignatureAuth "Authorization" LookupRegistryOrg :> UpdateAPI
           :<|> OnSubscribeAPI LookupRegistryOnSubscribe
       )

wrapperAPI :: Proxy WrapperAPI
wrapperAPI = Proxy

fmdWrapperBackendServer :: FlowServer WrapperAPI
fmdWrapperBackendServer =
  pure "FMD wrapper backend is UP"
    :<|> API.search
    :<|> API.select
    :<|> API.init
    :<|> API.confirm
    :<|> API.status
    :<|> API.track
    :<|> API.cancel
    :<|> API.update
    :<|> onSubscribe
