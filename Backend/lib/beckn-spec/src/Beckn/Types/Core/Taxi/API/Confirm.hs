{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.Confirm where

import Beckn.Types.Core.Taxi.Confirm
import qualified BecknV2.OnDemand.Types as Spec
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknReq)
import Kernel.Utils.Servant.JSONBS
import Servant (JSON, Post, ReqBody, (:>))

type ConfirmReq = BecknReq ConfirmMessage

type ConfirmReqV2 = Spec.ConfirmReq

type ConfirmRes = AckResponse

type ConfirmAPI =
  "confirm"
    -- :> ReqBody '[JSON] ConfirmReq
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] ConfirmRes

type ConfirmAPIV1 =
  "confirm"
    :> ReqBody '[JSON] ConfirmReq
    :> Post '[JSON] ConfirmRes

type ConfirmAPIV2 =
  "confirm"
    :> ReqBody '[JSON] ConfirmReqV2
    :> Post '[JSON] ConfirmRes

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy

confirmAPIV1 :: Proxy ConfirmAPIV1
confirmAPIV1 = Proxy

confirmAPIV2 :: Proxy ConfirmAPIV2
confirmAPIV2 = Proxy
