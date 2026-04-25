module API.Internal.OfferDiscount where

import qualified Domain.Action.Internal.OfferDiscount as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  "offerDiscount"
    :> Header "token" Text
    :> Capture "bppBookingId" Text
    :> ReqBody '[JSON] Domain.OfferDiscountReq
    :> Post '[JSON] Domain.OfferDiscountResp

handler :: FlowServer API
handler =
  getOfferDiscount

getOfferDiscount :: Maybe Text -> Text -> Domain.OfferDiscountReq -> FlowHandler Domain.OfferDiscountResp
getOfferDiscount token bppBookingId req = withFlowHandlerAPI $ Domain.getOfferDiscount token bppBookingId req
