{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.GeometryList
  ( API,
    handler,
  )
where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Geometry as DGEO
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id, state)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.GeometryGeom as QGEO
import Tools.Error

data GeometryAPIEntity = GeometryAPIEntity
  { region :: Text,
    state :: Context.IndianState,
    city :: Context.City,
    geom :: Maybe Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

type API =
  Capture "merchantId" (Id Merchant)
    :> Capture "city" Context.City
    :> "geometry"
    :> "list"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "allCities" Bool
    :> Get '[JSON] [GeometryAPIEntity]

handler :: FlowServer API
handler = getGeometryList

getGeometryList ::
  Id Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Bool ->
  FlowHandler [GeometryAPIEntity]
getGeometryList merchantId city mbLimit mbOffset mbAllCities = withFlowHandlerAPI $ do
  void $ CQMOC.findByMerchantIdAndCity merchantId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantId: " <> merchantId.getId <> " ,city: " <> show city)
  geoms <- case mbAllCities of
    Just True -> QGEO.findAllGeometriesForMerchant merchantId mbLimit mbOffset
    _ -> QGEO.findAllGeometries city mbLimit mbOffset
  return $ map toResponse geoms
  where
    toResponse :: DGEO.Geometry -> GeometryAPIEntity
    toResponse geom =
      GeometryAPIEntity
        { region = geom.region,
          state = geom.state,
          city = geom.city,
          geom = geom.geom
        }
