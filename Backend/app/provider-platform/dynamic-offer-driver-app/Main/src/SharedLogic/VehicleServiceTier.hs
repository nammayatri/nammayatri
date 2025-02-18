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
import qualified Domain.Types.Vehicle as DV
import qualified Domain.Types.VehicleCategory as VC
import qualified Domain.Types.VehicleServiceTier as DVST
import Kernel.Prelude

selectVehicleTierForDriverWithUsageRestriction :: Bool -> DI.DriverInformation -> DV.Vehicle -> [DVST.VehicleServiceTier] -> [(DVST.VehicleServiceTier, Bool)]
selectVehicleTierForDriverWithUsageRestriction onlyAutoSelected driverInfo vehicle cityVehicleServiceTiers =
  map mapUsageRestriction $ filter filterVehicleTier cityVehicleServiceTiers
  where
    mapUsageRestriction :: DVST.VehicleServiceTier -> (DVST.VehicleServiceTier, Bool)
    mapUsageRestriction vehicleServiceTier = do
      let _seatingCapacityCheck = compareNumber vehicle.capacity vehicleServiceTier.seatingCapacity
          luggageCapacityCheck = compareNumber vehicle.luggageCapacity vehicleServiceTier.luggageCapacity
          airConditionedCheck =
            (vehicleServiceTier.vehicleCategory == Just VC.AMBULANCE)
              || ( (compareNumber vehicleServiceTier.airConditionedThreshold driverInfo.airConditionScore)
                     && (isNothing vehicleServiceTier.airConditionedThreshold || vehicle.airConditioned /= Just False)
                 )
          driverRatingCheck = compareNumber Nothing vehicleServiceTier.driverRating -- driverStats.rating (Fix this later, removed due to perf issues)
          vehicleRatingCheck = compareNumber vehicle.vehicleRating vehicleServiceTier.vehicleRating

      let usageRestricted = not (luggageCapacityCheck && airConditionedCheck && driverRatingCheck && vehicleRatingCheck) -- && seatingCapacityCheck)
      (vehicleServiceTier, usageRestricted)

    filterVehicleTier vehicleServiceTier = vehicle.variant `elem` (if onlyAutoSelected then vehicleServiceTier.autoSelectedVehicleVariant else vehicleServiceTier.allowedVehicleVariant)

    compareNumber :: Ord a => Maybe a -> Maybe a -> Bool
    compareNumber mbX mbY =
      case (mbX, mbY) of
        (Just x, Just y) -> x >= y
        _ -> True
