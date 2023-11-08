{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.Rating where

import Beckn.Types.Core.Taxi.Rating (RatingMessage, RatingMessageV2)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Beckn.Ack
import Kernel.Types.Beckn.ReqTypes (BecknReq)
import Kernel.Utils.Servant.JSONBS
import Servant (JSON, Post, ReqBody, (:>))

type RatingReq = BecknReq RatingMessage

type RatingReqV2 = BecknReq RatingMessageV2

type RatingRes = AckResponse

type RatingAPI =
  "rating"
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] RatingRes

type RatingAPIV2 =
  "rating"
    :> ReqBody '[JSON] RatingReqV2
    :> Post '[JSON] RatingRes

ratingAPI :: Proxy RatingAPI
ratingAPI = Proxy

ratingAPIV2 :: Proxy RatingAPIV2
ratingAPIV2 = Proxy
