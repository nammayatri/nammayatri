module API.Internal.FrequentLocUser where

import qualified Domain.Action.Internal.FrequentLocUser as Domain
import Environment (FlowHandler, FlowServer)
import GHC.Base
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Utils.Common (withFlowHandlerAPI)
import Kernel.Utils.Error.Throwing (throwError)
import Servant hiding (throwError)
import Tools.Error

type API =
  "frequentLocUser"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] Domain.UpdateFrLocGeohashReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = updateFrequentLocGeohashHandler

updateFrequentLocGeohashHandler :: Maybe Text -> Domain.UpdateFrLocGeohashReq -> FlowHandler APISuccess
updateFrequentLocGeohashHandler apiKey req = withFlowHandlerAPI $ do
  internalClickhouseAPIKey <- asks (.internalClickhouseAPIKey)
  unless (Just internalClickhouseAPIKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  Domain.updateFrequentLocGeohashHandler req
