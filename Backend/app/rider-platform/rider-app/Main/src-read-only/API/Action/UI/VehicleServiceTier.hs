{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.VehicleServiceTier
  ( API,
    handler,
  )
where

import qualified API.Types.UI.VehicleServiceTier
import qualified Control.Lens
import qualified Domain.Action.UI.VehicleServiceTier
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "vehicleServiceTiers" :> Get ('[JSON]) [API.Types.UI.VehicleServiceTier.VehicleServiceTierAPIEntity])

handler :: Environment.FlowServer API
handler = getVehicleServiceTiers

getVehicleServiceTiers ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler [API.Types.UI.VehicleServiceTier.VehicleServiceTierAPIEntity]
  )
getVehicleServiceTiers a1 = withFlowHandlerAPI $ Domain.Action.UI.VehicleServiceTier.getVehicleServiceTiers (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
