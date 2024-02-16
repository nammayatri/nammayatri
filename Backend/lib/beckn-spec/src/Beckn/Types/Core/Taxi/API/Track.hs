{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.Track where

import Beckn.Types.Core.Taxi.Track
import qualified BecknV2.OnDemand.Types as Spec
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknReq)
import Kernel.Utils.Servant.JSONBS
import Servant (JSON, Post, ReqBody, (:>))

type TrackReq = BecknReq TrackMessage

type TrackReqV2 = Spec.TrackReq

type TrackRes = AckResponse

type TrackAPI =
  "track"
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] TrackRes

trackAPI :: Proxy TrackAPI
trackAPI = Proxy

type TrackAPIV1 =
  "track"
    :> ReqBody '[JSON] TrackReq
    :> Post '[JSON] TrackRes

trackAPIV1 :: Proxy TrackAPIV1
trackAPIV1 = Proxy

type TrackAPIV2 =
  "track"
    :> ReqBody '[JSON] TrackReqV2
    :> Post '[JSON] TrackRes

trackAPIV2 :: Proxy TrackAPIV2
trackAPIV2 = Proxy
