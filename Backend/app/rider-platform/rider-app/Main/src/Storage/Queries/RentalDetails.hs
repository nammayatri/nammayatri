{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.RentalDetails where

import Domain.Types.RentalDetails
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RentalDetails as BeamRS

createRentalDetails :: MonadFlow m => RentalDetails -> m ()
createRentalDetails = createWithKV

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id RentalDetails -> m (Maybe RentalDetails)
findById rentalDetailsId = findOneWithKV [Se.Is BeamRS.id $ Se.Eq (getId rentalDetailsId)]

instance FromTType' BeamRS.RentalDetails RentalDetails where
  fromTType' BeamRS.RentalDetailsT {..} = do
    let nightShiftInfo = ((,,) <$> nightShiftCharge <*> nightShiftStart <*> nightShiftEnd) <&> \(nightShiftCharge', nightShiftStart', nightShiftEnd') -> NightShiftInfo nightShiftCharge' Nothing nightShiftStart' nightShiftEnd'
    pure $
      Just
        RentalDetails
          { id = Id id,
            ..
          }

instance ToTType' BeamRS.RentalDetails RentalDetails where
  toTType' RentalDetails {..} = do
    BeamRS.RentalDetailsT
      { BeamRS.id = getId id,
        BeamRS.baseFare = baseFare,
        BeamRS.perHourCharge = perHourCharge,
        BeamRS.perExtraMinRate = perExtraMinRate,
        BeamRS.perExtraKmRate = perExtraKmRate,
        BeamRS.includedKmPerHr = includedKmPerHr,
        BeamRS.plannedPerKmRate = plannedPerKmRate,
        BeamRS.nightShiftCharge = (.nightShiftCharge) <$> nightShiftInfo,
        BeamRS.nightShiftStart = (.nightShiftStart) <$> nightShiftInfo,
        BeamRS.nightShiftEnd = (.nightShiftEnd) <$> nightShiftInfo
      }
