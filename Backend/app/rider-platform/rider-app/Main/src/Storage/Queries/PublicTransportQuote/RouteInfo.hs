{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.PublicTransportQuote.RouteInfo where

import Domain.Types.PublicTransportQuote.RouteInfo
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.PublicTransportQuote.RouteInfo as BeamRI

create :: MonadFlow m => RouteInfo -> m ()
create = createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id RouteInfo -> m (Maybe RouteInfo)
findById routeInfoId = findOneWithKV [Se.Is BeamRI.id $ Se.Eq (getId routeInfoId)]

instance FromTType' BeamRI.RouteInfo RouteInfo where
  fromTType' BeamRI.RouteInfoT {..} = do
    pure $
      Just
        RouteInfo
          { id = Id id,
            ..
          }

instance ToTType' BeamRI.RouteInfo RouteInfo where
  toTType' RouteInfo {..} = do
    BeamRI.RouteInfoT
      { BeamRI.id = getId id,
        BeamRI.routeId = routeId,
        BeamRI.tripId = tripId,
        BeamRI.routeNo = routeNo,
        BeamRI.routeName = routeName,
        BeamRI.createdAt = createdAt
      }
