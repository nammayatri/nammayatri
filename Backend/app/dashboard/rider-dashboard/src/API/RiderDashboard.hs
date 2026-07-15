module API.RiderDashboard
  ( API,
    handler,
  )
where

import qualified API.RiderDashboard.Entity as Entity
import qualified API.RiderDashboard.PTEmployee as PTEmployee
import "lib-dashboard" Environment
import Servant
import "lib-dashboard" Storage.Beam.BeamFlow

type API =
  Entity.API
    :<|> PTEmployee.API

handler :: BeamFlow' => FlowServer API
handler =
  Entity.handler
    :<|> PTEmployee.handler
