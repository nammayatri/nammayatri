module Product.BecknProvider.Rating where

import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.Rating as Rating
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.Rating as ACL
import Data.Aeson (encode)
import qualified Domain.Action.Beckn.Rating as DRating
import Domain.Types.Organization (Organization)
import Environment
import EulerHS.Prelude hiding (id)
import Utils.Common

rating ::
  Id Organization ->
  SignatureAuthResult ->
  Rating.RatingReq ->
  FlowHandler AckResponse
rating _ (SignatureAuthResult signPayload subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    logTagInfo "ratingAPI" "Received rating API call."
    Esq.runTransaction $
      QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)

    dRatingReq <- ACL.buildRatingReq subscriber req

    DRating.handler dRatingReq
    pure Ack
