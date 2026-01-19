{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.SpecialLocationUpsert
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.SpecialLocationUpsert as Domain
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.ServantMultipart
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  Capture "merchantId" (Id Merchant)
    :> Capture "city" Context.City
    :> "specialLocation"
    :> "upsert"
    :> MultipartForm Tmp Domain.SpecialLocationUpsertReq
    :> Post '[JSON] Domain.SpecialLocationUpsertResp

handler :: FlowServer API
handler = upsertSpecialLocation

upsertSpecialLocation ::
  Id Merchant ->
  Context.City ->
  Domain.SpecialLocationUpsertReq ->
  FlowHandler Domain.SpecialLocationUpsertResp
upsertSpecialLocation merchantId city = withFlowHandlerAPI . Domain.upsertSpecialLocation merchantId city
