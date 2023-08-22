module API.Internal
  ( API,
    handler,
  )
where

import qualified API.Internal.Rating as Rating
import Environment
import Servant
import Tools.Auth ()

type API =
  "internal"
    :> Rating.API

handler :: FlowServer API
handler = Rating.handler
