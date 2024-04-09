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

createRentalDetails :: (MonadFlow m, EsqDBFlow m r) => RentalDetails -> m ()
createRentalDetails = createWithKV

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id RentalDetails -> m (Maybe RentalDetails)
findById rentalDetailsId = findOneWithKV [Se.Is BeamRS.id $ Se.Eq (getId rentalDetailsId)]

instance FromTType' BeamRS.RentalDetails RentalDetails where
  fromTType' BeamRS.RentalDetailsT {..} = do
    let nightShiftInfo =
          ((,,) <$> nightShiftCharge <*> nightShiftStart <*> nightShiftEnd) <&> \(nightShiftCharge', nightShiftStart', nightShiftEnd') -> do
            let nightShiftCharge_ = mkPriceWithDefault nightShiftChargeAmount currency nightShiftCharge'
            NightShiftInfo nightShiftCharge_ Nothing nightShiftStart' nightShiftEnd'
    pure $
      Just
        RentalDetails
          { id = Id id,
            baseFare = mkPriceWithDefault baseFareAmount currency baseFare,
            perHourCharge = mkPriceWithDefault perHourChargeAmount currency perHourCharge,
            perExtraMinRate = mkPriceWithDefault perExtraMinRateAmount currency perExtraMinRate,
            perExtraKmRate = mkPriceWithDefault perExtraKmRateAmount currency perExtraKmRate,
            plannedPerKmRate = mkPriceWithDefault plannedPerKmRateAmount currency plannedPerKmRate,
            includedDistancePerHr = mkDistanceWithDefaultMeters distanceUnit includedDistancePerHrValue $ kilometersToMeters includedKmPerHr,
            ..
          }

instance ToTType' BeamRS.RentalDetails RentalDetails where
  toTType' RentalDetails {..} = do
    let distanceUnit = Just includedDistancePerHr.unit -- should be the same for all fields
    BeamRS.RentalDetailsT
      { BeamRS.id = getId id,
        BeamRS.baseFare = baseFare.amountInt,
        BeamRS.perHourCharge = perHourCharge.amountInt,
        BeamRS.perExtraMinRate = perExtraMinRate.amountInt,
        BeamRS.perExtraKmRate = perExtraKmRate.amountInt,
        BeamRS.baseFareAmount = Just baseFare.amount,
        BeamRS.perHourChargeAmount = Just perHourCharge.amount,
        BeamRS.perExtraMinRateAmount = Just perExtraMinRate.amount,
        BeamRS.perExtraKmRateAmount = Just perExtraKmRate.amount,
        BeamRS.includedKmPerHr = metersToKilometers $ distanceToMeters includedDistancePerHr,
        BeamRS.includedDistancePerHrValue = Just $ distanceToHighPrecDistance distanceUnit includedDistancePerHr,
        BeamRS.distanceUnit = distanceUnit,
        BeamRS.plannedPerKmRate = plannedPerKmRate.amountInt,
        BeamRS.nightShiftCharge = (.nightShiftCharge.amountInt) <$> nightShiftInfo,
        BeamRS.plannedPerKmRateAmount = Just plannedPerKmRate.amount,
        BeamRS.nightShiftChargeAmount = (.nightShiftCharge.amount) <$> nightShiftInfo,
        BeamRS.currency = Just baseFare.currency,
        BeamRS.nightShiftStart = (.nightShiftStart) <$> nightShiftInfo,
        BeamRS.nightShiftEnd = (.nightShiftEnd) <$> nightShiftInfo
      }
