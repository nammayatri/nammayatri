{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.CancellationCharges where

import Domain.Types.CancellationCharges
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Beam.CancellationCharges as BeamCC

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => CancellationCharges -> m ()
create = createWithKV

instance FromTType' BeamCC.CancellationCharges CancellationCharges where
  fromTType' BeamCC.CancellationChargesT {..} = do
    pure $
      Just
        CancellationCharges
          { id = Id id,
            driverId = Id driverId,
            rideId = Id <$> rideId,
            cancellationCharges = cancellationCharges
          }

instance ToTType' BeamCC.CancellationCharges CancellationCharges where
  toTType' CancellationCharges {..} = do
    BeamCC.CancellationChargesT
      { BeamCC.id = id.getId,
        BeamCC.driverId = driverId.getId,
        BeamCC.rideId = getId <$> rideId,
        BeamCC.cancellationCharges = cancellationCharges
      }
