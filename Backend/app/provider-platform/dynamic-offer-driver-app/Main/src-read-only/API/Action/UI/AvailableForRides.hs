{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.AvailableForRides
  ( API,
    handler,
  )
where

import qualified API.Types.UI.AvailableForRides
import qualified Control.Lens
import qualified Domain.Action.UI.AvailableForRides
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

type API = (TokenAuth :> "driver" :> "availableForRides" :> "activate" :> Post ('[JSON]) API.Types.UI.AvailableForRides.AvailableForRidesRes)

handler :: Environment.FlowServer API
handler = postDriverAvailableForRidesActivate

postDriverAvailableForRidesActivate ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.AvailableForRides.AvailableForRidesRes
  )
postDriverAvailableForRidesActivate a1 = withFlowHandlerAPI $ Domain.Action.UI.AvailableForRides.postDriverAvailableForRidesActivate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
