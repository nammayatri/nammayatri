module Beckn.Types.Core.API.Auth where

import Beckn.Utils.Servant.HeaderAuth
import Beckn.Utils.Servant.SignatureAuth
import Servant

type BecknAuth apiKey api =
  SignatureAuth "Authorization" :> api
    :<|> APIKeyAuth apiKey :> api

type BecknAuthProxy apiKey api =
  SignatureAuth "Authorization" :> SignatureAuth "Proxy-Authorization" :> api
    :<|> APIKeyAuth apiKey :> api
