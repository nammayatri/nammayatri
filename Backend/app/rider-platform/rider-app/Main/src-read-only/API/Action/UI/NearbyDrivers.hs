{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.NearbyDrivers
  ( API,
    handler,
  )
where

import qualified API.Types.UI.NearbyDrivers
import qualified Control.Lens
import qualified Domain.Action.UI.NearbyDrivers
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

type API = (TokenAuth :> "nearbyDrivers" :> ReqBody '[JSON] API.Types.UI.NearbyDrivers.NearbyDriverReq :> Post '[JSON] API.Types.UI.NearbyDrivers.NearbyDriverRes)

handler :: Environment.FlowServer API
handler = postNearbyDrivers

postNearbyDrivers ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.NearbyDrivers.NearbyDriverReq ->
    Environment.FlowHandler API.Types.UI.NearbyDrivers.NearbyDriverRes
  )
postNearbyDrivers a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.NearbyDrivers.postNearbyDrivers (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
