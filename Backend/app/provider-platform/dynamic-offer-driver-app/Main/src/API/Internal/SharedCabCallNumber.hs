module API.Internal.SharedCabCallNumber
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.SharedCabCallNumber as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  "sharedCab"
    :> "driver"
    :> Capture "driverId" Text
    :> "callNumber"
    :> Header "token" Text
    :> Get '[JSON] Domain.SharedCabDriverCallNumberResp

handler :: FlowServer API
handler driverId = withFlowHandlerAPI . Domain.sharedCabDriverCallNumber driverId
