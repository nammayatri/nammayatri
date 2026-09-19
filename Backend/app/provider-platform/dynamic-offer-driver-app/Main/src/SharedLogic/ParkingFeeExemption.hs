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
  ( isParkingFeeExemptByRcId,
    isParkingFeeExemptForDriver,
  )
where

import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DriverRCAssociationExtra as QDRCA
import qualified Storage.Queries.VehicleRegistrationCertificate as QVRC

-- | For a ride that already exists, keyed on the RC snapshotted onto ride_details at
--   assignment, so the exemption follows the vehicle that actually did the ride.
isParkingFeeExemptByRcId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe Text -> m Bool
isParkingFeeExemptByRcId Nothing = pure False
isParkingFeeExemptByRcId (Just rcId) =
  maybe False (fromMaybe False . (.exemptParkingFee))
    <$> QVRC.findById (Id rcId :: Id DVRC.VehicleRegistrationCertificate)

-- | Before a ride exists (driver quote, fare re-quote), resolved through the driver's
--   currently linked RC.
isParkingFeeExemptForDriver :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> m Bool
isParkingFeeExemptForDriver driverId = do
  now <- getCurrentTime
  QDRCA.findLatestLinkedByDriverId driverId now
    >>= maybe (pure False) (\assoc -> isParkingFeeExemptByRcId (Just assoc.rcId.getId))
