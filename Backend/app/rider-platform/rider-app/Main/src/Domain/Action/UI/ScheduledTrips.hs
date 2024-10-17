{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.ScheduledTrips (getScheduledTrips) where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Route
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

getScheduledTrips ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Route.Route ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
getScheduledTrips (_, _) routeId = do error "Logic yet to be decided"
