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
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Vehicle as DV
import qualified Domain.Types.VehicleCategory as VC
import qualified Domain.Types.VehicleServiceTier as DVST
import Domain.Utils (getVehicleAge)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error

selectVehicleTierForDriverWithUsageRestriction :: Bool -> DI.DriverInformation -> DV.Vehicle -> [DVST.VehicleServiceTier] -> Maybe Centesimal -> UTCTime -> [(DVST.VehicleServiceTier, Bool)]
selectVehicleTierForDriverWithUsageRestriction onlyAutoSelected driverInfo vehicle cityVehicleServiceTiers mbDriverRating now =
  map mapUsageRestriction $ filter filterVehicleTier cityVehicleServiceTiers
  where
    vehicleAgeInMonths = getVehicleAge vehicle.mYManufacturing now

    mapUsageRestriction :: DVST.VehicleServiceTier -> (DVST.VehicleServiceTier, Bool)
    mapUsageRestriction vehicleServiceTier = do
      let _seatingCapacityCheck = compareNumber vehicle.capacity vehicleServiceTier.seatingCapacity
          luggageCapacityCheck = compareNumber vehicle.luggageCapacity vehicleServiceTier.luggageCapacity
          airConditionedCheck =
            (vehicleServiceTier.vehicleCategory == Just VC.AMBULANCE)
              || ( (compareNumber vehicleServiceTier.airConditionedThreshold driverInfo.airConditionScore)
                     && (isNothing vehicleServiceTier.airConditionedThreshold || vehicle.airConditioned /= Just False)
                 )
          driverRatingCheck = compareNumber' mbDriverRating vehicleServiceTier.driverRating
          vehicleRatingCheck = compareNumber' vehicle.vehicleRating vehicleServiceTier.vehicleRating
          mfcCheck = case (vehicleAgeInMonths, vehicleServiceTier.vehicleAgeThreshold) of
            (Just (Months age), Just (Months threshold)) -> age < threshold
            _ -> True

      let usageRestricted = not (luggageCapacityCheck && airConditionedCheck && driverRatingCheck && vehicleRatingCheck && mfcCheck)
      (vehicleServiceTier, usageRestricted)

    filterVehicleTier vehicleServiceTier = vehicle.variant `elem` (if onlyAutoSelected then vehicleServiceTier.autoSelectedVehicleVariant else vehicleServiceTier.allowedVehicleVariant)

    compareNumber :: Ord a => Maybe a -> Maybe a -> Bool
    compareNumber mbX mbY =
      case (mbX, mbY) of
        (Just x, Just y) -> x >= y
        _ -> True

    compareNumber' :: Ord a => Maybe a -> Maybe a -> Bool
    compareNumber' mbX mbY =
      case (mbX, mbY) of
        (Just x, Just y) -> x >= y
        (Nothing, Just _) -> False
        _ -> True

fetchVehicleTierForDriverWithUsageRestriction ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Bool ->
  Maybe DI.DriverInformation ->
  Maybe DV.Vehicle ->
  Maybe (Maybe Centesimal) ->
  Maybe [DVST.VehicleServiceTier] ->
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  m [(DVST.VehicleServiceTier, Bool)]
fetchVehicleTierForDriverWithUsageRestriction onlyAutoSelected mbDriverInfo mbVehicle mbRating mbCityServiceTiers personId merchantOpCityId = do
  driverInfo <- maybe (QDI.findById personId >>= fromMaybeM DriverInfoNotFound) pure mbDriverInfo
  vehicle <- maybe (QVehicle.findById personId >>= fromMaybeM (VehicleNotFound personId.getId)) pure mbVehicle
  rating <- maybe ((>>= (.rating)) <$> QDriverStats.findById vehicle.driverId) pure mbRating
  cityServiceTiers <- maybe (CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing Nothing) pure mbCityServiceTiers
  now <- getCurrentTime
  pure $ selectVehicleTierForDriverWithUsageRestriction onlyAutoSelected driverInfo vehicle cityServiceTiers rating now
