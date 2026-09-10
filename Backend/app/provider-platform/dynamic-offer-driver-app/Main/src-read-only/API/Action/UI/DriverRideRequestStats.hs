{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.DriverRideRequestStats
  ( API,
    handler,
  )
where

import qualified API.Types.UI.DriverRideRequestStats
import qualified Control.Lens
import qualified Domain.Action.UI.DriverRideRequestStats
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "rideRequestStats" :> QueryParam "durationInMinutes" Kernel.Prelude.Int :> Get ('[JSON]) API.Types.UI.DriverRideRequestStats.DriverRideRequestStatsRes)

handler :: Environment.FlowServer API
handler = getRideRequestStats

getRideRequestStats ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Environment.FlowHandler API.Types.UI.DriverRideRequestStats.DriverRideRequestStatsRes
  )
getRideRequestStats a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverRideRequestStats.getRideRequestStats (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
