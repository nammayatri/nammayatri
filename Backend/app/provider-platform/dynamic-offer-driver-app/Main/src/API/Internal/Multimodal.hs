module API.Internal.Multimodal
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FareCalculator
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Action.UI.FareCalculator as Domain
import Domain.Types.Merchant
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  Capture "merchantId" (Id Merchant)
    :> Capture "merchantCity" Context.City
    :> "calculateFare"
    :> Header "token" Text
    :> ReqBody '[JSON] Domain.CalculateFareReq
    :> Post '[JSON] API.Types.UI.FareCalculator.FareResponse
    :<|> Capture "merchantId" (Id Merchant)
      :> "isInterCity"
      :> Header "token" Text
      :> ReqBody '[JSON] DSearch.IsIntercityReq
      :> Post '[JSON] DSearch.IsIntercityResp

handler :: FlowServer API
handler =
  calculateFare
    :<|> getIsInterCity

calculateFare :: Id Merchant -> Context.City -> Maybe Text -> Domain.CalculateFareReq -> FlowHandler API.Types.UI.FareCalculator.FareResponse
calculateFare merchantId merchantCity apiKey req = withFlowHandlerAPI $ Domain.calculateFare merchantId merchantCity apiKey req

getIsInterCity :: Id Merchant -> Maybe Text -> DSearch.IsIntercityReq -> FlowHandler DSearch.IsIntercityResp
getIsInterCity merchantId apiKey req = withFlowHandlerAPI $ DSearch.getIsInterCity merchantId apiKey req
