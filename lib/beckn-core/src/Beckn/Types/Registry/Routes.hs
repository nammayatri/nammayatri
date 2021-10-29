module Beckn.Types.Registry.Routes where

import qualified Beckn.Types.Registry.API as API
import EulerHS.Prelude
import Servant

type LookupAPI =
  "lookup"
    :> ReqBody '[JSON] API.LookupRequest
    :> Post '[JSON] API.LookupResponse

lookupAPI :: Proxy LookupAPI
lookupAPI = Proxy
