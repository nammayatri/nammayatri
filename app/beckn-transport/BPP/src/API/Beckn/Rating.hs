module API.Beckn.Rating (API, handler) where

import App.Types
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.Rating as API
import qualified Beckn.Types.Core.Taxi.API.Rating as Rating
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.Rating as ACL
import qualified Domain.Action.Beckn.Rating as DRating
import Domain.Types.Organization (Organization)
import EulerHS.Prelude
import Servant hiding (throwError)
import Utils.Common

type API =
  Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization"
    :> API.RatingAPI

handler :: FlowServer API
handler = ratingImpl

ratingImpl ::
  Id Organization ->
  SignatureAuthResult ->
  Rating.RatingReq ->
  FlowHandler AckResponse
ratingImpl transporterId (SignatureAuthResult _ subscriber) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "ratingAPI" "Received rating API call."
    dRatingReq <- ACL.buildRatingReq subscriber req
    DRating.ratingImpl transporterId dRatingReq
    return Ack
