module Beckn.InternalAPI.Auth.API where

import Beckn.Prelude
import Servant

type API =
  "internal"
    :> "auth"
    :> Capture "token" Token
    :> Get '[JSON] PersonId

type Token = Text

type PersonId = Text
