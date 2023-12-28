{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Rating (API, handler) where

import qualified Beckn.ACL.Rating as ACL
import qualified Beckn.Types.Core.Taxi.API.Rating as Rating
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
import Storage.Beam.SystemConfigs ()

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
rating merchantId (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    logTagInfo "ratingAPI" "Received rating API call."
    dRatingReq <- ACL.buildRatingReq subscriber req
    Redis.whenWithLockRedis (ratingLockKey dRatingReq.bookingId.getId) 60 $ do
      ride <- DRating.validateRequest dRatingReq
      fork "rating request processing" $
        Redis.whenWithLockRedis (ratingProcessingLockKey dRatingReq.bookingId.getId) 60 $
          DRating.handler merchantId dRatingReq ride
    pure Ack

ratingLockKey :: Text -> Text
ratingLockKey id = "Driver:Rating:BookingId-" <> id

ratingProcessingLockKey :: Text -> Text
ratingProcessingLockKey id = "Driver:Rating:Processing:BookingId-" <> id
