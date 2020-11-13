module Beckn.Types.API.Auth where

import Beckn.Utils.Servant.HeaderAuth
import Beckn.Utils.Servant.SignatureAuth
import Servant ((:<|>), (:>))

type BecknAuth sig apiKey api =
  SignatureAuth "Authorization" sig :> api
    :<|> APIKeyAuth apiKey :> api
