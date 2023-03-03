module API.Internal
  ( API,
    handler,
  )
where

import qualified API.Internal.DriverReferee as DriverReferee
import Environment
import Servant

type API =
  "internal"
    :> DriverReferee.API

handler :: FlowServer API
handler =
  DriverReferee.handler
