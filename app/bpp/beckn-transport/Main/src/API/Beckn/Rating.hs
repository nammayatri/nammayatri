module API.Beckn.Rating (API, handler) where

import qualified Beckn.Types.Core.Taxi.API.Rating as API
import qualified Beckn.Types.Core.Taxi.API.Rating as Rating
import qualified Core.ACL.Rating as ACL
import qualified Domain.Action.Beckn.Rating as DRating
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)

type API =
  Capture "merchantId" (Id Merchant)
    :> SignatureAuth "Authorization"
    :> API.RatingAPI

handler :: FlowServer API
handler = ratingImpl

ratingImpl ::
  Id Merchant ->
  SignatureAuthResult ->
  Rating.RatingReq ->
  FlowHandler AckResponse
ratingImpl transporterId (SignatureAuthResult _ subscriber _) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "ratingAPI" "Received rating API call."
    dRatingReq <- ACL.buildRatingReq subscriber req
    Redis.whenWithLockRedis (ratingLockKey dRatingReq.bookingId.getId) 60 $ do
      DRating.ratingImpl transporterId dRatingReq
    return Ack

ratingLockKey :: Text -> Text
ratingLockKey id = "Driver:Rating:BookingId-" <> id
