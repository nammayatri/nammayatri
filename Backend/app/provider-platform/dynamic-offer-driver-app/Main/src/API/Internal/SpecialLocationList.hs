{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.SpecialLocationList
  ( API,
    handler,
  )
where

import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.SpecialLocation as QSL
import qualified Lib.Types.SpecialLocation as SL
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

type API =
  Capture "merchantId" (Id Merchant)
    :> Capture "city" Context.City
    :> "specialLocation"
    :> "list"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "locationType" SL.SpecialLocationType
    :> Get '[JSON] [QSL.SpecialLocationFull]

handler :: FlowServer API
handler = getSpecialLocationList

getSpecialLocationList ::
  Id Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe SL.SpecialLocationType ->
  FlowHandler [QSL.SpecialLocationFull]
getSpecialLocationList merchantId city mbLimit mbOffset mbSpecialLocationType = withFlowHandlerAPI $ do
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchantId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantId: " <> merchantId.getId <> " ,city: " <> show city)
  QSL.findAllSpecialLocationsWithGeoJSON merchantOpCity.id.getId mbLimit mbOffset mbSpecialLocationType
