{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.GeometryUpdate
  ( API,
    handler,
  )
where

import Data.OpenApi (ToSchema)
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.GeometryGeom as QGEO
import Tools.Error

data GeometryUpdateReq = GeometryUpdateReq
  { region :: Text,
    newGeom :: Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

type API =
  Capture "merchantId" (Id Merchant)
    :> Capture "city" Context.City
    :> "geometry"
    :> "update"
    :> ReqBody '[JSON] GeometryUpdateReq
    :> Put '[JSON] APISuccess

handler :: FlowServer API
handler = updateGeometry

updateGeometry ::
  Id Merchant ->
  Context.City ->
  GeometryUpdateReq ->
  FlowHandler APISuccess
updateGeometry merchantId city req = withFlowHandlerAPI $ do
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchantId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantId: " <> merchantId.getId <> " ,city: " <> show city)
  let cityParam = merchantOpCity.city
      stateParam = merchantOpCity.state
  QGEO.updateGeometry cityParam stateParam req.region req.newGeom
  return Success
