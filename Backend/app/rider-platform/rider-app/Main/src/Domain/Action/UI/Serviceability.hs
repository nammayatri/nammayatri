{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Serviceability
  ( checkServiceability,
    getNearestOperatingAndCurrentCity,
    ServiceabilityRes (..),
    NearestOperatingAndCurrentCity (..),
    CityState (..),
    getNearestOperatingCityHelper,
  )
where

import API.UI.HotSpot
import Data.List (sortBy)
import Data.Ord
import qualified Domain.Types.HotSpot as DHotSpot
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.Person as Person
import qualified Kernel.Beam.Functions as B
import Kernel.External.Maps.Types hiding (geometry)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Esqueleto.Transactionable as Esq
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.Person as CQP
import Storage.Queries.Geometry (findGeometriesContaining)
import Tools.Error

data ServiceabilityRes = ServiceabilityRes
  { serviceable :: Bool,
    city :: Maybe Context.City,
    currentCity :: Maybe Context.City,
    specialLocation :: Maybe QSpecialLocation.SpecialLocationFull,
    geoJson :: Maybe Text,
    hotSpotInfo :: [DHotSpot.HotSpotInfo],
    blockRadius :: Maybe Int,
    isMetroServiceable :: Maybe Bool,
    isSubwayServiceable :: Maybe Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

checkServiceability ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    MonadFlow m,
    EsqDBFlow m r
  ) =>
  (GeofencingConfig -> GeoRestriction) ->
  (Id Person.Person, Id Merchant.Merchant) ->
  LatLong ->
  Bool ->
  Bool ->
  m ServiceabilityRes
checkServiceability settingAccessor (personId, merchantId) location shouldUpdatePerson isOrigin = do
  DHotSpot.HotSpotResponse {..} <- getHotspot location merchantId
  mbNearestOpAndCurrentCity <- getNearestOperatingAndCurrentCity' settingAccessor (personId, merchantId) shouldUpdatePerson location
  person <- CQP.findCityInfoById personId >>= fromMaybeM (PersonNotFound personId.getId)
  case mbNearestOpAndCurrentCity of
    Just (NearestOperatingAndCurrentCity {nearestOperatingCity, currentCity}) -> do
      let city = Just nearestOperatingCity.city
      specialLocationBody <- Esq.runInReplica $ QSpecialLocation.findSpecialLocationByLatLongFull location
      let filteredSpecialLocationBody = QSpecialLocation.filterGates specialLocationBody isOrigin
      riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
      now <- getCurrentTime
      let isOutsideMetroBusinessHours = FRFSUtils.isOutsideBusinessHours riderConfig.qrTicketRestrictionStartTime riderConfig.qrTicketRestrictionEndTime now riderConfig.timeDiffFromUtc
      let isOutsideSubwayBusinessHours = FRFSUtils.isOutsideBusinessHours riderConfig.subwayRestrictionStartTime riderConfig.subwayRestrictionEndTime now riderConfig.timeDiffFromUtc
      return
        ServiceabilityRes
          { serviceable = True,
            currentCity = Just currentCity.city,
            specialLocation = filteredSpecialLocationBody,
            geoJson = (.geoJson) =<< filteredSpecialLocationBody,
            isMetroServiceable = Just (not isOutsideMetroBusinessHours),
            isSubwayServiceable = Just (not isOutsideSubwayBusinessHours),
            ..
          }
    Nothing ->
      return
        ServiceabilityRes
          { city = Nothing,
            currentCity = Nothing,
            serviceable = False,
            specialLocation = Nothing,
            geoJson = Nothing,
            isMetroServiceable = Nothing,
            isSubwayServiceable = Nothing,
            ..
          }

data NearestOperatingAndCurrentCity = NearestOperatingAndCurrentCity
  { nearestOperatingCity :: CityState,
    currentCity :: CityState
  }
  deriving (Eq, Show)

data CityState = CityState
  { city :: Context.City,
    state :: Context.IndianState
  }
  deriving (Eq, Show)

getNearestOperatingAndCurrentCity ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    MonadFlow m,
    EsqDBFlow m r
  ) =>
  (GeofencingConfig -> GeoRestriction) ->
  (Id Person.Person, Id Merchant.Merchant) ->
  Bool ->
  LatLong ->
  m NearestOperatingAndCurrentCity
getNearestOperatingAndCurrentCity settingAccessor (personId, merchantId) shouldUpdatePerson latLong = do
  mbNearestOpAndCurrentCity <- getNearestOperatingAndCurrentCity' settingAccessor (personId, merchantId) shouldUpdatePerson latLong
  case mbNearestOpAndCurrentCity of
    Just a -> return a
    Nothing -> throwError RideNotServiceable

getNearestOperatingAndCurrentCity' ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    MonadFlow m,
    EsqDBFlow m r
  ) =>
  (GeofencingConfig -> GeoRestriction) ->
  (Id Person.Person, Id Merchant.Merchant) ->
  Bool ->
  LatLong ->
  m (Maybe NearestOperatingAndCurrentCity)
getNearestOperatingAndCurrentCity' settingAccessor (personId, merchantId) shouldUpdatePerson latLong = do
  merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  let geoRestriction = settingAccessor (merchant.geofencingConfig)
  let merchantCityState = CityState {city = merchant.defaultCity, state = merchant.defaultState}
  mbNearestOpAndCurrentCity <- getNearestOperatingCityHelper merchant geoRestriction latLong merchantCityState
  whenJust mbNearestOpAndCurrentCity $ \NearestOperatingAndCurrentCity {nearestOperatingCity} -> do
    upsertPersonCityInformation personId merchantId shouldUpdatePerson (Just nearestOperatingCity.city)
  return mbNearestOpAndCurrentCity

getNearestOperatingCityHelper :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Merchant.Merchant -> GeoRestriction -> LatLong -> CityState -> m (Maybe NearestOperatingAndCurrentCity)
getNearestOperatingCityHelper merchant geoRestriction latLong merchantCityState = do
  case geoRestriction of
    Unrestricted -> do
      return $ Just $ NearestOperatingAndCurrentCity {nearestOperatingCity = merchantCityState, currentCity = merchantCityState}
    Regions regions -> do
      {-
        Below logic is to find the nearest operating city for the pickup location.
        If the pickup location is in the operating city, then return the city.
        If the pickup location is not in the city, then return the nearest city for that state else the merchant default city.
      -}
      geoms <- B.runInReplica $ findGeometriesContaining latLong regions
      case filter (\geom -> geom.city /= Context.AnyCity) geoms of
        [] ->
          find (\geom -> geom.city == Context.AnyCity) geoms & \case
            Just anyCityGeom -> do
              cities <- CQMOC.findAllByMerchantIdAndState merchant.id anyCityGeom.state >>= mapM (\m -> return (distanceBetweenInMeters latLong (LatLong m.lat m.long), m.city))
              let nearestOperatingCity = maybe merchantCityState (\p -> CityState {city = snd p, state = anyCityGeom.state}) (listToMaybe $ sortBy (comparing fst) cities)
              return $ Just $ NearestOperatingAndCurrentCity {currentCity = CityState {city = anyCityGeom.city, state = anyCityGeom.state}, nearestOperatingCity}
            Nothing -> do
              logError $ "No geometry found for latLong: " <> show latLong <> " for regions: " <> show regions
              return Nothing
        (g : _) -> do
          -- Nearest operating city and source city are same
          let operatingCityState = CityState {city = g.city, state = g.state}
          return $ Just $ NearestOperatingAndCurrentCity {nearestOperatingCity = operatingCityState, currentCity = operatingCityState}

upsertPersonCityInformation :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person.Person -> Id Merchant.Merchant -> Bool -> Maybe Context.City -> m ()
upsertPersonCityInformation personId merchantId shouldUpdatePerson mbCity = when shouldUpdatePerson $
  whenJust mbCity $ \city' -> do
    personCityInfo <- CQP.findCityInfoById personId >>= fromMaybeM (PersonCityInformationDoesNotExist $ "personId:- " <> personId.getId)
    when (personCityInfo.currentCity /= city') $ do
      merchantOperatingCity <-
        CQMOC.findByMerchantIdAndCity merchantId city'
          >>= fromMaybeM
            ( MerchantOperatingCityNotFound $
                "merchantId:- " <> merchantId.getId <> " ,city:- " <> show city'
            )
      CQP.updateCityInfoById personId city' merchantOperatingCity.id
