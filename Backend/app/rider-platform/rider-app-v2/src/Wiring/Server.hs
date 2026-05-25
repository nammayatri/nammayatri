{-
  Rider-app-v2 server: API composition.

  Rider-facing endpoints only. Beckn callbacks stay on original rider-app.
  Runs on port 8113.
-}
module Wiring.Server
  ( V2API,
    v2Server,
  )
where

import Environment
import Kernel.Prelude
import Servant
import Wiring.BAP

-- | V2 API: rider-facing endpoints through mobility-flows framework.
type V2API =
  "v2"
    :> ( SearchAPI
           :<|> ("rideSearch" :> GetQuotesAPI)
       )

v2Server :: FlowServer V2API
v2Server = searchHandler :<|> getQuotesHandler
