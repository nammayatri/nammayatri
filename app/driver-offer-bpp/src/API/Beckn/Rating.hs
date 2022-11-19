module API.Beckn.Rating (API, handler) where

import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.Rating as Rating
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.Rating as ACL
import qualified Domain.Action.Beckn.Rating as DRating
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id)
import Servant

type API =
  Capture "merchantId" (Id Merchant)
    :> SignatureAuth "Authorization"
    :> Rating.RatingAPI

handler :: FlowServer API
handler = rating

rating ::
  Id Merchant ->
  SignatureAuthResult ->
  Rating.RatingReq ->
  FlowHandler AckResponse
rating _ (SignatureAuthResult _ subscriber _) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    logTagInfo "ratingAPI" "Received rating API call."
    dRatingReq <- ACL.buildRatingReq subscriber req

    DRating.handler dRatingReq
    pure Ack
