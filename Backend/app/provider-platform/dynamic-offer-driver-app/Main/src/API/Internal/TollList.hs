{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.TollList
  ( API,
    handler,
  )
where

import qualified Dashboard.Common.Merchant as DM
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import SharedLogic.TollDashboard
import Storage.Beam.SystemConfigs ()
import Storage.Beam.Toll ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Toll.Storage.CachedQueries.Toll as CQToll
import Tools.Error

type API =
  Capture "merchantId" (Id Merchant)
    :> Capture "city" Context.City
    :> "toll"
    :> "list"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] [DM.TollAPIEntity]

handler :: FlowServer API
handler = getTollList

getTollList ::
  Id Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  FlowHandler [DM.TollAPIEntity]
getTollList merchantId city mbLimit mbOffset = withFlowHandlerAPI $ do
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchantId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantId: " <> merchantId.getId <> " ,city: " <> show city)
  tolls <- CQToll.findAllTollsByMerchantOperatingCity merchantOpCity.id.getId
  pure $ map tollToAPIEntity $ applyPagination mbLimit mbOffset tolls
