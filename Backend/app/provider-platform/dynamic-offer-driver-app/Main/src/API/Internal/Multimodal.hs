module API.Internal.MultiModal
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FareCalculator
import qualified Domain.Action.UI.FareCalculator as Domain
import Domain.Types.Ride
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  Capture "merchantId" (Id Merchant)
    :> Capture "merchantCity" Context.City
    :> "calculateFare"
    :> Header "token" Text
    :> ReqBody '[JSON] CalculateFareReq
    :> Post '[JSON] API.Types.UI.FareCalculator.FareResponse

handler :: FlowServer API
handler =
  calculateFare

calculateFare :: Id Merchant -> Context.City -> Maybe Text -> CalculateFareReq -> FlowHandler API.Types.UI.FareCalculator.FareResponse
calculateFare merchantId merchantCity apiKey req = withFlowHandlerAPI $ Domain.calculateFare merchantId merchantCity apiKey req
