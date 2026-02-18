module Domain.Action.UI.Places (postPlaces) where

import qualified API.Types.UI.Places as API
-- import Data.Time (UTCTime, getCurrentTime)

import qualified Domain.Action.UI.MultimodalConfirm as DAUM
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.MultimodalPreferences as DMP
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.RecentLocation as DRecntLoc
import qualified Environment as Env
import EulerHS.Prelude hiding (id, null)
import Kernel.External.Maps.Google.MapsClient.Types
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.MultiModal.Interface as MultiModal
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils as JMU
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PopularLocation as QPopularLocation
import qualified Storage.Queries.RecentLocation as QRecentLocation
import Tools.Error
import qualified Tools.MultiModal as TMultiModal

mkWayPointV2 :: Double -> Double -> WayPointV2
mkWayPointV2 lat lon =
  WayPointV2
    { location =
        LocationV2
          { latLng = LatLngV2 {latitude = lat, longitude = lon}
          }
    }

getMultiModalModes :: API.PlacesRequest -> LatLong -> Id.Id DMerchant.Merchant -> DPerson.Person -> Id.Id MerchantOperatingCity -> Env.Flow (Maybe MultiModal.MultiModalResponse)
getMultiModalModes req toLatLong merchantId person merchantOperatingCityId = do
  riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) >>= fromMaybeM (RiderConfigNotFound merchantOperatingCityId.getId)
  userPref <- DAUM.getMultimodalUserPreferences (Just person.id, merchantId)
  let transitRoutesReq =
        MultiModal.GetTransitRoutesReq
          { origin = mkWayPointV2 req.userLat req.userLon,
            destination = mkWayPointV2 toLatLong.lat toLatLong.lon,
            arrivalTime = Nothing,
            departureTime = Nothing,
            mode = Nothing,
            transitPreferences = Nothing,
            transportModes = Nothing,
            minimumWalkDistance = riderConfig.minimumWalkDistance,
            permissibleModes = fromMaybe [] riderConfig.permissibleModes,
            maxAllowedPublicTransportLegs = riderConfig.maxAllowedPublicTransportLegs,
            sortingType = JMU.convertSortingType $ fromMaybe DMP.FASTEST userPref.journeyOptionsSortingType,
            walkSpeed = Nothing
          }
  transitServiceReq <- TMultiModal.getTransitServiceReq merchantId person.merchantOperatingCityId
  MultiModal.getTransitRoutes (Just person.id.getId) transitServiceReq transitRoutesReq

mkRecentLocation :: Maybe MultiModal.MultiModalResponse -> DRecntLoc.RecentLocation -> Maybe API.MultiModalLocation
mkRecentLocation multimodalRoutes recentLoc =
  case recentLoc.entityType of
    DRecntLoc.MULTIMODAL -> do
      recentLoc.address <&> \address ->
        API.MultiModalLocation
          { name = address,
            address = address,
            lat = recentLoc.toLatLong.lat,
            lon = recentLoc.toLatLong.lon,
            multimodalRoutes = multimodalRoutes,
            mode = Just recentLoc.entityType,
            rating = Nothing,
            type_ = Nothing,
            toStationCode = Nothing,
            fromStationCode = Nothing,
            routeCode = Nothing,
            recentLocationId = Just recentLoc.id,
            fare = recentLoc.fare
          }
    _ -> do
      case (recentLoc.toStopCode, recentLoc.fromStopCode) of
        (Just toStopCode, Just fromStopCode) -> do
          Just $
            API.MultiModalLocation
              { name = fromMaybe "" recentLoc.address, -- not request for other modes
                address = fromMaybe "" recentLoc.address,
                lat = recentLoc.toLatLong.lat,
                lon = recentLoc.toLatLong.lon,
                multimodalRoutes = Nothing,
                mode = Just recentLoc.entityType,
                rating = Nothing,
                type_ = Nothing,
                toStationCode = Just toStopCode,
                fromStationCode = Just fromStopCode,
                routeCode = recentLoc.routeCode,
                recentLocationId = Just recentLoc.id,
                fare = recentLoc.fare
              }
        _ -> Nothing

postPlaces ::
  ( (Kernel.Prelude.Maybe (Id.Id DPerson.Person), Id.Id DMerchant.Merchant) ->
    API.PlacesRequest ->
    Env.Flow API.PlacesResponse
  )
postPlaces (mbPersonId, merchantId) req = do
  personId <- fromMaybeM (PersonNotFound "No person found") mbPersonId
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  popularLocations <- QPopularLocation.findAllByMerchantOperatingCityId person.merchantOperatingCityId
  recentLocations <- do
    recentJourneys <- QRecentLocation.findRecentLocations personId person.merchantOperatingCityId
    forM recentJourneys $ \recentJourney -> do
      case recentJourney.entityType of
        DRecntLoc.MULTIMODAL -> do
          multimodalRoutes <- getMultiModalModes req recentJourney.toLatLong merchantId person person.merchantOperatingCityId
          return $ mkRecentLocation multimodalRoutes recentJourney
        _ -> return $ mkRecentLocation Nothing recentJourney

  popularLocationsWithDistance <-
    if null recentLocations
      then do
        forM popularLocations $ \loc -> do
          multimodalRoutes <- getMultiModalModes req (LatLong loc.lat loc.lon) merchantId person person.merchantOperatingCityId
          return $
            API.MultiModalLocation
              { name = loc.name,
                address = loc.address,
                lat = loc.lat,
                lon = loc.lon,
                multimodalRoutes = multimodalRoutes,
                mode = Nothing,
                rating = loc.rating,
                type_ = Just loc.type_,
                fromStationCode = Nothing,
                toStationCode = Nothing,
                routeCode = Nothing,
                recentLocationId = Nothing,
                fare = Nothing
              }
      else return []
  pure
    API.PlacesResponse
      { popularLocations = popularLocationsWithDistance,
        recentLocations = catMaybes recentLocations
      }
