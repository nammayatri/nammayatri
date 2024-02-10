{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.OnCancel where

import qualified BecknV2.OnDemand.Types as Spec
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Servant (JSON, Post, ReqBody, (:>))

type OnCancelReqV2 = Spec.OnCancelReq

type OnCancelRes = AckResponse

type OnCancelAPIV2 =
  "on_cancel"
    :> ReqBody '[JSON] OnCancelReqV2
    :> Post '[JSON] OnCancelRes

onCancelAPIV2 :: Proxy OnCancelAPIV2
onCancelAPIV2 = Proxy
