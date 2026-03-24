{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.PreferredRoute
  ( getPreferredRoutes,
  )
where

import qualified API.Types.UI.PreferredRoute as API
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.UserPreferredRoute as DUPR
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.UserPreferredRoute as QUPR

getPreferredRoutes ::
  ( Maybe (Id Person.Person),
    Id Merchant.Merchant
  ) ->
  Flow [API.PreferredRouteResp]
getPreferredRoutes (mbPersonId, _merchantId) = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  routes <- runInReplica $ QUPR.findAllByPersonId Nothing Nothing personId
  return $ map buildPreferredRouteResp routes

buildPreferredRouteResp :: DUPR.UserPreferredRoute -> API.PreferredRouteResp
buildPreferredRouteResp DUPR.UserPreferredRoute {..} =
  API.PreferredRouteResp
    { id = getId id,
      routeName = routeName,
      fromLocationLat = fromLocationLat,
      fromLocationLon = fromLocationLon,
      fromLocationAddress = fromLocationAddress,
      toLocationLat = toLocationLat,
      toLocationLon = toLocationLon,
      toLocationAddress = toLocationAddress,
      priority = priority,
      usageCount = usageCount
    }
