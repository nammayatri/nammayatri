module API.Internal.DriverArrivalNotf where

import qualified Domain.Action.Internal.DriverArrivalNotf as Domain
import Environment (FlowHandler, FlowServer)
import GHC.Base
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Utils.Error
import Kernel.Utils.Error.Throwing ()
import Servant hiding (throwError)
import Tools.Error

type API =
  "driverArrivalNotification"
    :> Header "api-key" Text
    :> ReqBody '[JSON] Domain.DANTypeValidationReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = driverArrivalNotfHandler

driverArrivalNotfHandler :: Maybe Text -> Domain.DANTypeValidationReq -> FlowHandler APISuccess
driverArrivalNotfHandler apiKey req = withFlowHandlerAPI $ do
  unless (Just req.apiKey == apiKey) $ throwError $ AuthBlocked "Invalid api key"
  Domain.driverArrivalNotfHandler req
