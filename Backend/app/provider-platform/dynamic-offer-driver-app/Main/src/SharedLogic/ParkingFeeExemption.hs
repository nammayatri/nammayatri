{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Vehicle side of the parking fee exemption. The zone side lives on
--   'FullFarePolicy.parkingFeeExemptionEnabled'; both must be true before
--   'FareCalculator.calculateFareParametersHandler' drops the parking charge, so
--   callers should resolve this only when the zone has already opted in.
module SharedLogic.ParkingFeeExemption
  ( isParkingFeeExemptByVehicleNumber,
    isParkingFeeExemptForDriver,
  )
where

import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Vehicle as QVehicle

-- | For a ride that already exists, keyed on the vehicle number snapshotted onto
--   ride_details at assignment, so the exemption follows the vehicle that actually did the ride.
isParkingFeeExemptByVehicleNumber :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe Text -> m Bool
isParkingFeeExemptByVehicleNumber Nothing = pure False
isParkingFeeExemptByVehicleNumber (Just vehicleNumber) =
  maybe False (fromMaybe False . (.exemptParkingFee)) <$> QVehicle.findByRegistrationNo vehicleNumber

-- | Before a ride exists (driver quote, fare re-quote), resolved through the vehicle the
--   driver currently has linked.
isParkingFeeExemptForDriver :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> m Bool
isParkingFeeExemptForDriver driverId =
  maybe False (fromMaybe False . (.exemptParkingFee)) <$> QVehicle.findById driverId
