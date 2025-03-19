module API.Internal.MeterRideFare
  ( API,
    handler,
  )
where

import qualified API.Types.UI.PriceBreakup
import qualified Domain.Action.Internal.MeterRideFare as Domain
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  Capture "merchantId" (Kernel.Types.Id.Id Merchant)
    :> ( "meterRideFare"
           :> Header "token" Text
           :> ReqBody '[JSON] Domain.MeterRideFareInfoReq
           :> Post '[JSON] API.Types.UI.PriceBreakup.MeterRidePriceRes
       )

handler :: FlowServer API
handler =
  getMeterRidePrice

getMeterRidePrice :: Kernel.Types.Id.Id Merchant -> Maybe Text -> Domain.MeterRideFareInfoReq -> FlowHandler API.Types.UI.PriceBreakup.MeterRidePriceRes
getMeterRidePrice merchantId apiKey = withFlowHandlerAPI . Domain.getMeterRidePrice merchantId apiKey
