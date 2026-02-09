{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CityDetector
  ( NearestOperatingAndSourceCity (..),
    CityState (..),
    getNearestOperatingAndSourceCity,
    checkForIntercityOrCrossCity,
    getDestinationCity,
  )
where

import Data.List (sortBy)
import Data.Ord
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.TransporterConfig as DTMT
import Kernel.Beam.Functions as B
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Geofencing
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Storage.CachedQueries.InterCityTravelCities as CQITC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantState as CQMS
import qualified Storage.Queries.Geometry as QGeometry
import Tools.Error

data NearestOperatingAndSourceCity = NearestOperatingAndSourceCity
  { nearestOperatingCity :: CityState,
    sourceCity :: CityState
  }

data CityState = CityState
  { city :: Context.City,
    state :: Context.IndianState
  }

getNearestOperatingAndSourceCity :: ServiceFlow m r => DM.Merchant -> LatLong -> m NearestOperatingAndSourceCity
getNearestOperatingAndSourceCity merchant pickupLatLong = do
  let geoRestriction = merchant.geofencingConfig.origin
  let merchantCityState = CityState {city = merchant.city, state = merchant.state}
  case geoRestriction of
    Unrestricted -> do
      pure $ NearestOperatingAndSourceCity {nearestOperatingCity = merchantCityState, sourceCity = merchantCityState}
    Regions regions -> do
      {-
        Below logic is to find the nearest operating city for the pickup location.
        If the pickup location is in the operating city, then return the city.
        If the pickup location is not in the city, then return the nearest city for that state else the merchant default city.
      -}
      geoms <- B.runInReplica $ QGeometry.findGeometriesContaining pickupLatLong regions
      case filter (\geom -> geom.city /= Context.AnyCity) geoms of
        [] ->
          find (\geom -> geom.city == Context.AnyCity) geoms & \case
            Just anyCityGeom -> do
              cities <- CQMOC.findAllByMerchantIdAndState merchant.id anyCityGeom.state >>= mapM (\m -> return (distanceBetweenInMeters pickupLatLong m.location, m.city))
              let nearestOperatingCity = maybe merchantCityState (\p -> CityState {city = snd p, state = anyCityGeom.state}) (listToMaybe $ sortBy (comparing fst) cities)
              return $ NearestOperatingAndSourceCity {sourceCity = CityState {city = anyCityGeom.city, state = anyCityGeom.state}, nearestOperatingCity}
            Nothing -> do
              logError $ "No geometry found for pickupLatLong: " <> show pickupLatLong <> " for regions: " <> show regions
              throwError RideNotServiceable
        (g : _) -> do
          -- Nearest operating city and source city are same
          let operatingCityState = CityState {city = g.city, state = g.state}
          return $ NearestOperatingAndSourceCity {nearestOperatingCity = operatingCityState, sourceCity = operatingCityState}

getDestinationCity :: ServiceFlow m r => DM.Merchant -> LatLong -> m (CityState, Maybe Text)
getDestinationCity merchant dropLatLong = do
  let geoRestriction = merchant.geofencingConfig.destination
  case geoRestriction of
    Unrestricted -> return (CityState {city = merchant.city, state = merchant.state}, Nothing)
    Regions regions -> do
      geoms <- B.runInReplica $ QGeometry.findGeometriesContaining dropLatLong regions
      case filter (\geom -> geom.city /= Context.AnyCity) geoms of
        [] ->
          find (\geom -> geom.city == Context.AnyCity) geoms & \case
            Just anyCityGeom -> do
              interTravelCities <- CQITC.findByMerchantIdAndState merchant.id anyCityGeom.state >>= mapM (\m -> return (distanceBetweenInMeters dropLatLong (LatLong {lat = m.lat, lon = m.lng}), m.cityName))
              mbNearestCity <-
                if null interTravelCities
                  then do
                    operatingCities <- CQMOC.findAllByMerchantIdAndState merchant.id anyCityGeom.state >>= mapM (\m -> return (distanceBetweenInMeters dropLatLong m.location, show m.city))
                    return $ snd <$> listToMaybe (sortBy (comparing fst) operatingCities)
                  else do
                    intercityTravelAreas <- CQITC.findInterCityAreasContainingGps dropLatLong
                    return $ (.cityName) <$> listToMaybe intercityTravelAreas
              return (CityState {city = anyCityGeom.city, state = anyCityGeom.state}, mbNearestCity)
            Nothing -> do
              logError $ "No geometry found for dropLatLong: " <> show dropLatLong <> " for regions: " <> show regions
              throwError RideNotServiceable
        (g : _) -> return (CityState {city = g.city, state = g.state}, Just $ show g.city)

checkForIntercityOrCrossCity :: ServiceFlow m r => DTMT.TransporterConfig -> Maybe LatLong -> CityState -> DM.Merchant -> m (Bool, Bool, Maybe Text)
checkForIntercityOrCrossCity transporterConfig mbDropLocation sourceCity merchant = do
  case mbDropLocation of
    Just dropLoc -> do
      (destinationCityState, mbDestinationTravelCityName) <- getDestinationCity merchant dropLoc -- This checks for destination serviceability too
      if destinationCityState.city == sourceCity.city && destinationCityState.city /= Context.AnyCity
        then return (False, False, Nothing)
        else do
          mbMerchantState <- CQMS.findByMerchantIdAndState merchant.id sourceCity.state
          let allowedStates = maybe [sourceCity.state] (.allowedDestinationStates) mbMerchantState
          -- Destination states should be in the allowed states of the origin state
          if destinationCityState.state `elem` allowedStates
            then do
              if destinationCityState.city `elem` transporterConfig.crossTravelCities
                then return (True, True, mbDestinationTravelCityName)
                else return (True, False, mbDestinationTravelCityName)
            else throwError (RideNotServiceableInState $ show destinationCityState.state)
    Nothing -> pure (False, False, Nothing)
