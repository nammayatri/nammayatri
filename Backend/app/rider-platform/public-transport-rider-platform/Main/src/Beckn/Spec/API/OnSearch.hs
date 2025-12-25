{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Spec.API.OnSearch where

import Beckn.Spec.OnSearch
import Kernel.Types.Beckn.Ack (AckResponse)
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Beckn.ReqTypes
import Kernel.Utils.Servant.SignatureAuth
import Servant (JSON, Post, ReqBody, (:>))

type OnSearchAPI =
  SignatureAuth 'Domain.PUBLIC_TRANSPORT "X-Gateway-Authorization"
    :> "on_search"
    :> ReqBody '[JSON] (BecknCallbackReq OnSearchCatalog)
    :> Post '[JSON] AckResponse
