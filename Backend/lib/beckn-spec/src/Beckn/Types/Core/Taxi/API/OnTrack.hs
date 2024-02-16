{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.OnTrack where

import Beckn.Types.Core.Taxi.OnTrack
import qualified BecknV2.OnDemand.Types as Spec
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknCallbackReq)
import Kernel.Utils.Servant.JSONBS
import Servant (JSON, Post, ReqBody, (:>))

type OnTrackReq = BecknCallbackReq OnTrackMessage

type OnTrackReqV2 = Spec.OnTrackReq

type OnTrackRes = AckResponse

type OnTrackAPI =
  "on_track"
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] OnTrackRes

type OnTrackAPIV1 =
  "on_track"
    :> ReqBody '[JSON] OnTrackReq
    :> Post '[JSON] OnTrackRes

onTrackAPIV1 :: Proxy OnTrackAPIV1
onTrackAPIV1 = Proxy

type OnTrackAPIV2 =
  "on_track"
    :> ReqBody '[JSON] OnTrackReqV2
    :> Post '[JSON] OnTrackRes

onTrackAPIV2 :: Proxy OnTrackAPIV2
onTrackAPIV2 = Proxy
