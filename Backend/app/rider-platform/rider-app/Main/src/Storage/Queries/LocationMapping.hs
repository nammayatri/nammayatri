{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.LocationMapping where

import Domain.Types.LocationMapping
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.LocationMapping as BeamLM

create :: MonadFlow m => LocationMapping -> m ()
create = createWithKV

countOrders :: MonadFlow m => Text -> m Int
countOrders entityId = findAllWithKV [Se.Is BeamLM.entityId $ Se.Eq entityId] <&> length

findByEntityId :: MonadFlow m => Text -> m [LocationMapping] --TODO : SORT BY ORDER
findByEntityId entityId = findAllWithKV [Se.Is BeamLM.entityId $ Se.Eq entityId]

instance FromTType' BeamLM.LocationMapping LocationMapping where
  fromTType' BeamLM.LocationMappingT {..} = do
    pure $
      Just
        LocationMapping
          { id = Id id,
            tag = tag,
            locationId = Id locationId,
            entityId = entityId,
            order = order,
            version = version
          }

instance ToTType' BeamLM.LocationMapping LocationMapping where
  toTType' LocationMapping {..} = do
    BeamLM.LocationMappingT
      { BeamLM.id = getId id,
        BeamLM.tag = tag,
        BeamLM.locationId = getId locationId,
        BeamLM.entityId = entityId,
        BeamLM.order = order,
        BeamLM.version = version
      }
