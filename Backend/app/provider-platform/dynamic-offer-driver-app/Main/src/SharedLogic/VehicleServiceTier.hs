{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.VehicleServiceTier where

import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Vehicle as DV
import qualified Domain.Types.VehicleServiceTier as DVST
import Kernel.Prelude

selectVehicleTierForDriver :: DP.Person -> DI.DriverInformation -> DV.Vehicle -> [DVST.VehicleServiceTier] -> [DVST.VehicleServiceTier]
selectVehicleTierForDriver person driverInfo vehicle cityVehicleServiceTiers = do
  let vehicleServiceTiersWithUsageRestriction = selectVehicleTierForDriverWithUsageRestriction person driverInfo vehicle cityVehicleServiceTiers
  map fst $ filter (not . snd) vehicleServiceTiersWithUsageRestriction

selectVehicleTierForDriverWithUsageRestriction :: DP.Person -> DI.DriverInformation -> DV.Vehicle -> [DVST.VehicleServiceTier] -> [(DVST.VehicleServiceTier, Bool)]
selectVehicleTierForDriverWithUsageRestriction person driverInfo vehicle cityVehicleServiceTiers =
  map mapUsageRestriction $ filter filterVehicleTier cityVehicleServiceTiers
  where
    mapUsageRestriction :: DVST.VehicleServiceTier -> (DVST.VehicleServiceTier, Bool)
    mapUsageRestriction vehicleServiceTier = do
      let _seatingCapacityCheck = compareNumber vehicle.capacity vehicleServiceTier.seatingCapacity
      let luggageCapacityCheck = compareNumber vehicle.luggageCapacity vehicleServiceTier.luggageCapacity
      let airConditionedCheck =
            (compareNumber vehicleServiceTier.airConditioned driverInfo.airConditionScore)
              && (isNothing vehicleServiceTier.airConditioned || vehicle.airConditioned /= Just False)
      let driverRatingCheck = compareNumber person.rating vehicleServiceTier.driverRating
      let vehicleRatingCheck = compareNumber vehicle.vehicleRating vehicleServiceTier.vehicleRating

      let usageRestricted = not (luggageCapacityCheck && airConditionedCheck && driverRatingCheck && vehicleRatingCheck) -- && seatingCapacityCheck)
      (vehicleServiceTier, usageRestricted)

    filterVehicleTier vehicleServiceTier = vehicle.variant `elem` vehicleServiceTier.allowedVehicleVariant

    compareNumber :: Ord a => Maybe a -> Maybe a -> Bool
    compareNumber mbX mbY =
      case (mbX, mbY) of
        (Just x, Just y) -> x >= y
        _ -> True
