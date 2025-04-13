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
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Google.MapsClient.Types
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.MultiModal.Interface as MultiModal
import Kernel.Prelude
import qualified Kernel.Types.Error as Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import qualified Kernel.Utils.Common as Utils
import qualified Lib.JourneyModule.Utils as JMU
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRiderConfig
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PopularLocation as QPopularLocation
import qualified Storage.Queries.RecentLocation as QRecentLocation
import qualified Storage.Queries.Station as QS
import qualified Tools.MultiModal as TMultiModal

-- TODO: COMPLETED THIS AND MOVE THIS TO SOME OTHER FILE
-- filterOTPBuses :: [MultiModal.MultiModalRoute] -> Env.Flow [MultiModal.MultiModalRoute]
-- filterOTPBuses routes _ = do
--   currentTime <- L.runIO getCurrentTime
--   -- Filter routes that have bus legs
--   filterM (hasAvailableBusInRoute currentTime) routes
--   where
--     hasAvailableBusInRoute :: Utils.UTCTime -> MultiModal.MultiModalRoute -> Env.Flow Bool
--     hasAvailableBusInRoute _ route = do
--       -- Extract all bus legs from the route
--       let busLegs = filter (\leg -> leg.mode == MultiModal.Bus) route.legs

--       -- If no bus legs, keep the route
--       if null busLegs
--         then pure True
--         else do
--           -- Check if all bus legs have available buses
--           busAvailabilities <- forM busLegs $ \_ -> do
--             return True
--           pure (and busAvailabilities)

getMultiModalModes :: API.PlacesRequest -> DRecntLoc.RecentLocation -> Id.Id DMerchant.Merchant -> DPerson.Person -> Id.Id MerchantOperatingCity -> Env.Flow (Maybe API.MultiModalLocation)
getMultiModalModes req recentLoc merchantId person merchantOperatingCityId = do
  riderConfig <- CQRiderConfig.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= Utils.fromMaybeM (Error.InternalError "Rider config not found")
  userPref <- DAUM.getMultimodalUserPreferences (Just person.id, merchantId)
  if (isNothing recentLoc.stopLat || isNothing recentLoc.stopLon || isNothing recentLoc.address)
    then return Nothing
    else do
      let transitRoutesReq =
            MultiModal.GetTransitRoutesReq
              { origin =
                  WayPointV2
                    { location =
                        LocationV2
                          { latLng =
                              LatLngV2
                                { latitude = req.userLat,
                                  longitude = req.userLon
                                }
                          }
                    },
                destination =
                  WayPointV2
                    { location =
                        LocationV2
                          { latLng =
                              LatLngV2
                                { latitude = fromJust recentLoc.stopLat,
                                  longitude = fromJust recentLoc.stopLon
                                }
                          }
                    },
                arrivalTime = Nothing,
                departureTime = Nothing,
                mode = Nothing,
                transitPreferences = Nothing,
                transportModes = Nothing,
                minimumWalkDistance = riderConfig.minimumWalkDistance,
                permissibleModes = fromMaybe [] riderConfig.permissibleModes,
                maxAllowedPublicTransportLegs = riderConfig.maxAllowedPublicTransportLegs,
                sortingType = JMU.convertSortingType $ fromMaybe DMP.FASTEST userPref.journeyOptionsSortingType
              }
      transitServiceReq <- TMultiModal.getTransitServiceReq merchantId person.merchantOperatingCityId
      multimodalRoutes <- MultiModal.getTransitRoutes transitServiceReq transitRoutesReq >>= Utils.fromMaybeM (Error.InternalError "routes dont exist")
      (pure . Just) $ mkRecentLocation (Just multimodalRoutes) recentLoc req Nothing

mkRecentLocation :: Maybe MultiModal.MultiModalResponse -> DRecntLoc.RecentLocation -> API.PlacesRequest -> Maybe Text -> API.MultiModalLocation
mkRecentLocation multimodalRoutes recentLoc req toStopName =
  API.MultiModalLocation
    { name = fromMaybe "" recentLoc.address,
      address = fromMaybe "" recentLoc.address,
      lat = recentLoc.lat,
      lon = recentLoc.lon,
      distance =
        Just $
          realToFrac $
            distanceBetweenInMeters
              (LatLong req.userLat req.userLon)
              (LatLong recentLoc.lat recentLoc.lon),
      multimodalRoutes = multimodalRoutes,
      mode = Just recentLoc.entityType,
      rating = Nothing,
      type_ = Nothing,
      toStationCode = recentLoc.stopCode,
      fromStationCode = recentLoc.fromStopCode,
      fromStationName = recentLoc.fromStopName,
      routeCode = recentLoc.routeCode,
      routeId = recentLoc.routeId,
      recentLocationId = Just recentLoc.id,
      toStationName = toStopName
    }

postPlaces ::
  ( (Kernel.Prelude.Maybe (Id.Id DPerson.Person), Id.Id DMerchant.Merchant) ->
    API.PlacesRequest ->
    Env.Flow API.PlacesResponse
  )
postPlaces (mbPersonId, merchantId) req = do
  -- Get popular locations based on merchantOperatingCityId
  personId <- Utils.fromMaybeM (Error.PersonNotFound "No person found") mbPersonId
  person <- QPerson.findById personId >>= Utils.fromMaybeM (Error.PersonNotFound "No person found")
  popularLocations <- QPopularLocation.findAllByMerchantOperatingCityId person.merchantOperatingCityId

  -- Get rider config for multimodal settings
  riderConfig <-
    CQRiderConfig.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing
      >>= Utils.fromMaybeM (Error.InternalError "Rider config not found")

  let searchRadius = 100 -- Need to move it config
  recentLocations <- do
    recentJourneys <- QRecentLocation.getRecentLocationByLatLon (req.userLat) (req.userLon) searchRadius personId person.merchantOperatingCityId

    forM recentJourneys $ \recentJourney -> do
      case recentJourney.entityType of
        DRecntLoc.MULTIMODAL -> getMultiModalModes req recentJourney merchantId person person.merchantOperatingCityId
        DRecntLoc.BUS -> do
          if ( isNothing recentJourney.address || isNothing recentJourney.routeId || isNothing recentJourney.routeCode || isNothing recentJourney.stopCode
                 || isNothing recentJourney.fromStopCode
                 || isNothing recentJourney.fromStopName
             )
            then pure Nothing
            else do
              toStop <- QS.findByStationCode (fromJust recentJourney.stopCode) req.integratedBppConfigId >>= Utils.fromMaybeM (Error.InternalError "Stop information not found")
              pure $ Just $ mkRecentLocation Nothing recentJourney req (Just toStop.name)
        _ -> pure $ Just $ mkRecentLocation Nothing recentJourney req Nothing

  popularLocationsWithDistance <- forM popularLocations $ \loc -> do
    let transitRoutesReq =
          MultiModal.GetTransitRoutesReq
            { origin =
                WayPointV2
                  { location =
                      LocationV2
                        { latLng =
                            LatLngV2
                              { latitude = req.userLat,
                                longitude = req.userLon
                              }
                        }
                  },
              destination =
                WayPointV2
                  { location =
                      LocationV2
                        { latLng =
                            LatLngV2
                              { latitude = loc.lat,
                                longitude = loc.lon
                              }
                        }
                  },
              arrivalTime = Nothing,
              departureTime = Nothing,
              mode = Nothing,
              transitPreferences = Nothing,
              transportModes = Nothing,
              minimumWalkDistance = riderConfig.minimumWalkDistance,
              permissibleModes = fromMaybe [] riderConfig.permissibleModes,
              maxAllowedPublicTransportLegs = riderConfig.maxAllowedPublicTransportLegs,
              sortingType = JMU.convertSortingType DMP.FASTEST
            }
    transitServiceReq <- TMultiModal.getTransitServiceReq merchantId person.merchantOperatingCityId
    multimodalRoutes <- MultiModal.getTransitRoutes transitServiceReq transitRoutesReq >>= Utils.fromMaybeM (Error.InternalError "routes dont exist")

    pure $
      API.MultiModalLocation
        { name = loc.name,
          address = loc.address,
          lat = loc.lat,
          lon = loc.lon,
          distance =
            Just $
              realToFrac $
                distanceBetweenInMeters
                  (LatLong req.userLat req.userLon)
                  (LatLong loc.lat loc.lon),
          multimodalRoutes = Just multimodalRoutes,
          mode = Nothing,
          rating = loc.rating,
          type_ = Just loc.type_,
          toStationCode = Nothing,
          fromStationCode = Nothing,
          fromStationName = Nothing,
          routeCode = Nothing,
          routeId = Nothing,
          recentLocationId = Nothing,
          toStationName = Nothing
        }

  -- Sort by distance and return response
  let sortedPopularLocations = sortOn (.distance) popularLocationsWithDistance
  let sortedRecentLocations = sortOn (.distance) (catMaybes recentLocations)

  pure
    API.PlacesResponse
      { popularLocations = sortedPopularLocations,
        recentLocations = sortedRecentLocations
      }
