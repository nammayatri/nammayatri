module Beckn.Types.Registry.Routes where

import qualified Beckn.Types.Registry.API as API
import Beckn.Utils.Servant.SignatureAuth (SignatureAuth)
import EulerHS.Prelude
import Servant

type LookupAPI =
  "lookup"
    :> ReqBody '[JSON] API.LookupRequest
    :> Post '[JSON] API.LookupResponse

lookupAPI ::
  Proxy
    ( "lookup"
        :> ReqBody '[JSON] API.LookupRequest
        :> Post '[JSON] API.LookupResponse
    )
lookupAPI = Proxy

type OnSubscribeAPI registryLookup =
  SignatureAuth "Signature" registryLookup
    :> "on_subscribe"
    :> ReqBody '[JSON] API.OnSubscribeRequest
    :> Post '[JSON] API.OnSubscribeResponse
