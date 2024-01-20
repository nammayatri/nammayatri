{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module BecknV2.FRFS.APIs where

import qualified BecknV2.FRFS.Types as Spec
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type SearchAPI =
  "search"
    :> ReqBody '[JSON] Spec.SearchReq
    :> Post '[JSON] Spec.AckResponse

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

type InitAPI =
  "init"
    :> ReqBody '[JSON] Spec.InitReq
    :> Post '[JSON] Spec.AckResponse

initAPI :: Proxy InitAPI
initAPI = Proxy

type ConfirmAPI =
  "confirm"
    :> ReqBody '[JSON] Spec.ConfirmReq
    :> Post '[JSON] Spec.AckResponse

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy

type StatusAPI =
  "status"
    :> ReqBody '[JSON] Spec.StatusReq
    :> Post '[JSON] Spec.AckResponse

statusAPI :: Proxy StatusAPI
statusAPI = Proxy
