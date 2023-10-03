{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.PublicTransportQuote.Stop where

import Domain.Types.PublicTransportQuote.Stop
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.PublicTransportQuote.Stop as BeamPTS

create :: MonadFlow m => Stop -> m ()
create = createWithKV

findById :: MonadFlow m => Id Stop -> m (Maybe Stop)
findById stopId = findOneWithKV [Se.Is BeamPTS.id $ Se.Eq (getId stopId)]

instance FromTType' BeamPTS.Stop Stop where
  fromTType' BeamPTS.StopT {..} = do
    let gps =
          Gps
            { lat = lat,
              lon = lon
            }
        descriptor = stopName >>= \name -> Just $ Descriptor {..}
        location =
          Location
            { descriptor = descriptor,
              address = Nothing,
              ..
            }
    pure $
      Just
        Stop
          { id = Id id,
            ..
          }

instance ToTType' BeamPTS.Stop Stop where
  toTType' Stop {..} = do
    let stopName = location.descriptor >>= Just . (.name)
        lat = location.gps.lat
        lon = location.gps.lon
    BeamPTS.StopT
      { BeamPTS.id = getId id,
        BeamPTS.stopName = stopName,
        BeamPTS.lat = lat,
        BeamPTS.lon = lon,
        BeamPTS.scheduledTime = scheduledTime,
        BeamPTS.createdAt = createdAt
      }
