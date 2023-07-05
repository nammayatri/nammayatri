{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.SearchRequestRoute
  ( searchRequestRoute,
  )
where

import Domain.Action.Beckn.Search
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as SearchRequest
import Domain.Types.SearchRequestRoute
import Kernel.Prelude
import Kernel.Storage.Hedis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common

searchRequestRoute :: (EncFlow m r, CoreMetrics m, HedisFlow m r) => Id SearchRequest.SearchRequest -> (Id Person.Person, Id Merchant.Merchant) -> m RouteInfo
searchRequestRoute searchRequestId (_, merchantId) = do
  let key = searchRequestKey (getId merchantId)
  hGet key (getId searchRequestId) >>= fromMaybeM (InternalError $ "Failed to get route with id = " <> show searchRequestId)
