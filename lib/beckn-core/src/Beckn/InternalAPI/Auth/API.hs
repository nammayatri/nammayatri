module Beckn.InternalAPI.Auth.API where

import Beckn.Prelude
import Servant

type API =
  "internal"
    :> "auth"
    :> Capture "token" Text
    :> Get '[JSON] PersonId

type PersonId = Text
