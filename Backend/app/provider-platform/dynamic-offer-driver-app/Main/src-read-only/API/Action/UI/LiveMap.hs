{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.LiveMap
  ( API,
    handler,
  )
where

import qualified API.Types.UI.LiveMap
import qualified Control.Lens
import qualified Domain.Action.UI.LiveMap as Domain.Action.UI.LiveMap
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

type API = (TokenAuth :> "liveMap" :> "drivers" :> Get ('[JSON]) [API.Types.UI.LiveMap.MapDriverInfoRes])

handler :: Environment.FlowServer API
handler = getLiveMapDrivers

getLiveMapDrivers ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler [API.Types.UI.LiveMap.MapDriverInfoRes]
  )
getLiveMapDrivers a1 = withFlowHandlerAPI $ Domain.Action.UI.LiveMap.getLiveMapDrivers (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
