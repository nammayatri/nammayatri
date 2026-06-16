{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.VehicleServiceTier where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Time (diffDays, utctDay)
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.TransporterConfig as DTPC
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

-- | Mirrors Driver.hs's default-tier derivation. Picks the highest-AC tier
-- whose `defaultForVehicleVariant` contains the driver's variant *and* whose
-- serviceTierType is in the city's supportedServiceTiers list. Falls back to
-- any default-for-variant tier if the intersection is empty. Also returns
-- whether AC is currently working for this driver (using the picked tier's
-- threshold) and whether the vehicle is supported at all in this city.
getDriverDefaultServiceTier ::
  DV.Vehicle ->
  DI.DriverInformation ->
  DTPC.TransporterConfig ->
  [DTC.ServiceTierType] ->
  [DVST.VehicleServiceTier] ->
  UTCTime ->
  (Bool, Maybe DVST.VehicleServiceTier, Bool)
getDriverDefaultServiceTier vehicle driverInfo transporterConfig supportedServiceTiers cityServiceTiers now =
  let allVehicleSupportedDefaultServiceTiers =
        sortOn (fmap Down . (.airConditionedThreshold)) $
          filter
            (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant && vst.serviceTierType `elem` supportedServiceTiers)
            cityServiceTiers
      isVehicleSupported = not $ null allVehicleSupportedDefaultServiceTiers
      mbDefaultServiceTierItem =
        if null allVehicleSupportedDefaultServiceTiers
          then find (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant) cityServiceTiers
          else listToMaybe allVehicleSupportedDefaultServiceTiers
      checkIfACWorking =
        case mbDefaultServiceTierItem >>= (.airConditionedThreshold) of
          Nothing -> False
          Just acThreshold ->
            fromMaybe 0 driverInfo.airConditionScore <= acThreshold
              && maybe True (\lastCheckedAt -> fromInteger (diffDays (utctDay now) (utctDay lastCheckedAt)) >= transporterConfig.acStatusCheckGap) driverInfo.lastACStatusCheckedAt
   in (checkIfACWorking, mbDefaultServiceTierItem, isVehicleSupported)
