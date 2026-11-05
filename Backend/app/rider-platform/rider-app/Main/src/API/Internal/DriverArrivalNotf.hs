module API.Internal.DriverArrivalNotf
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.DriverArrivalNotf as Domain
import Environment
import GHC.Base
import Kernel.Prelude (Text)
import Kernel.Types.APISuccess
import Kernel.Utils.Error
import Servant
import SharedLogic.InternalAuth

type API =
  ( "driverArrivalNotification"
      :> Header "api-key" Text
      :> ReqBody '[JSON] Domain.DANTypeValidationReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  driverArrivalNotfHandler

driverArrivalNotfHandler :: Maybe Text -> Domain.DANTypeValidationReq -> FlowHandler APISuccess
driverArrivalNotfHandler apiKey req = withFlowHandlerAPI $ do
  checkLtsKey "driverArrivalNotification" apiKey
  Domain.driverArrivalNotfHandler req
