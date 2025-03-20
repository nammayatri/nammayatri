module Domain.Action.UI.Places (postPlaces) where

import qualified API.Types.UI.Places as API
-- import Data.Time (UTCTime, getCurrentTime)

-- import qualified EulerHS.Language as L

import qualified Domain.Action.UI.MultimodalConfirm as DAUM
import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.MultimodalPreferences as DMP
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.RecentLocation as DRecntLoc
import qualified Domain.Types.SearchRequest as DSearch
import qualified Environment as Env
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Google.MapsClient.Types
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.MultiModal.Interface as MultiModal
import qualified Kernel.Prelude
import qualified Kernel.Types.Error as Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import qualified Kernel.Utils.Common as Utils
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRiderConfig
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.PopularLocation as QPopularLocation
import qualified Storage.Queries.RecentLocation as QRecentLocation
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Tools.MultiModal as TMultiModal

mkRouteKey :: Text -> Text
mkRouteKey routeId = "route:" <> routeId

-- TODO: COMPLETED THIS AND MOVE THIS TO SOME OTHER FILE
filterOTPBuses :: [MultiModal.MultiModalRoute] -> Env.Flow [MultiModal.MultiModalRoute]
filterOTPBuses routes _ = do
  currentTime <- L.runIO getCurrentTime
  -- Filter routes that have bus legs
  filterM (hasAvailableBusInRoute currentTime) routes
  where
    hasAvailableBusInRoute :: UTCTime -> MultiModal.MultiModalRoute -> Env.Flow Bool
    hasAvailableBusInRoute _ route = do
      -- Extract all bus legs from the route
      let busLegs = filter (\leg -> leg.mode == MultiModal.Bus) route.legs

      -- If no bus legs, keep the route
      if null busLegs
        then pure True
        else do
          -- Check if all bus legs have available buses
          busAvailabilities <- forM busLegs $ \_ -> do
            return True
          pure (and busAvailabilities)

convertSortingType :: DMP.JourneyOptionsSortingType -> MultiModal.SortingType
convertSortingType sortType = case sortType of
  DMP.FASTEST -> MultiModal.Fastest
  DMP.MINIMUM_TRANSITS -> MultiModal.Minimum_Transits
  _ -> MultiModal.Fastest -- Default case for any other values

getMultiModalModes :: API.PlacesRequest -> DRecntLoc.RecentLocation -> Id.Id DMerchant.Merchant -> Env.Flow (Maybe API.RecentLocation)
getMultiModalModes req recentLoc merchantId = do
  riderConfig <- CQRiderConfig.findByMerchantOperatingCityId req.merchantOperatingCityId Nothing >>= Utils.fromMaybeM (Error.InternalError "Rider config not found")
  journey' <- QJourney.findByPrimaryKey (Id.Id recentLoc.entityId)
  userPref <- DAUM.getMultimodalUserPreferences (Just req.personId, merchantId)
  case journey' of
    Just journey -> do
      srId <- QSearchRequest.findById journey.searchRequestId
      case srId of
        Just sr -> do
          let transitRoutesReq =
                MultiModal.GetTransitRoutesReq
                  { origin =
                      WayPointV2
                        { location =
                            LocationV2
                              { latLng =
                                  LatLngV2
                                    { latitude = sr.fromLocation.lat,
                                      longitude = sr.fromLocation.lon
                                    }
                              }
                        },
                    destination =
                      WayPointV2
                        { location =
                            LocationV2
                              { latLng =
                                  LatLngV2
                                    { latitude = maybe sr.fromLocation.lat (.lat) sr.toLocation,
                                      longitude = maybe sr.fromLocation.lon (.lon) sr.toLocation
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
                    sortingType = convertSortingType $ fromMaybe DMP.FASTEST userPref.journeyOptionsSortingType
                  }
          transitServiceReq <- TMultiModal.getTransitServiceReq merchantId req.merchantOperatingCityId
          multimodalRoutes <- MultiModal.getTransitRoutes transitServiceReq transitRoutesReq >>= Utils.fromMaybeM (Error.InternalError "routes dont exist")

          -- Filter routes that have available buses
          -- filteredMultimodalRoutes <- do
          --   filteredRoutes <- filterOTPBuses multimodalRoutes.routes merchantId
          --   pure multimodalRoutes{routes = filteredRoutes}

          -- Get journey legs for the past booking if journeyId exists

          let filteredRoutes =
                if null journey.modes
                  then multimodalRoutes.routes
                  else
                    let routesWithPastLeg =
                          filter
                            ( \route ->
                                any
                                  ( \routeLeg ->
                                      any
                                        ( \pastLeg ->
                                            let pastMode = case pastLeg of
                                                  DCommon.Metro -> MultiModal.MetroRail
                                                  DCommon.Bus -> MultiModal.Bus
                                                  DCommon.Walk -> MultiModal.Walk
                                                  DCommon.Taxi -> MultiModal.Unspecified
                                                  DCommon.Subway -> MultiModal.Subway
                                             in pastMode == routeLeg.mode
                                        )
                                        journey.modes
                                  )
                                  route.legs
                            )
                            multimodalRoutes.routes
                     in if null routesWithPastLeg
                          then multimodalRoutes.routes
                          else routesWithPastLeg

          (pure . Just) $ mkRecentLocation sr (Just (MultiModal.MultiModalResponse {routes = filteredRoutes})) recentLoc req
        Nothing -> pure Nothing
    Nothing -> pure Nothing

mkRecentLocation :: DSearch.SearchRequest -> Maybe MultiModal.MultiModalResponse -> DRecntLoc.RecentLocation -> API.PlacesRequest -> API.RecentLocation
mkRecentLocation sr multimodalRoutes recentLoc req =
  API.RecentLocation
    { name = fromMaybe "" sr.fromLocation.address.building,
      address =
        mconcat
          [ fromMaybe "" sr.fromLocation.address.building,
            ", ",
            fromMaybe "" sr.fromLocation.address.street,
            ", ",
            fromMaybe "" sr.fromLocation.address.city,
            ", ",
            fromMaybe "" sr.fromLocation.address.state,
            ", ",
            fromMaybe "" sr.fromLocation.address.country
          ],
      lat = sr.fromLocation.lat,
      lon = sr.fromLocation.lon,
      distance =
        Just $
          realToFrac $
            distanceBetweenInMeters
              (LatLong req.userLat req.userLon)
              (LatLong sr.fromLocation.lat sr.fromLocation.lon),
      multimodalRoutes = multimodalRoutes,
      mode = recentLoc.entityType
    }

postPlaces ::
  ( (Kernel.Prelude.Maybe (Id.Id DPerson.Person), Id.Id DMerchant.Merchant) ->
    API.PlacesRequest ->
    Env.Flow API.PlacesResponse
  )
postPlaces (_, merchantId) req = do
  -- Get popular locations based on merchantOperatingCityId
  popularLocations <- QPopularLocation.findAllByMerchantOperatingCityId req.merchantOperatingCityId

  -- Get rider config for multimodal settings
  riderConfig <-
    CQRiderConfig.findByMerchantOperatingCityId req.merchantOperatingCityId Nothing
      >>= Utils.fromMaybeM (Error.InternalError "Rider config not found")

  let searchRadius = 100 -- Need to move it config
  recentLocations <- do
    let personId = req.personId
    recentJourneys <- QRecentLocation.getRecentLocationByLatLon (req.userLat) (req.userLon) searchRadius personId req.merchantOperatingCityId

    forM recentJourneys $ \recentJourney -> do
      case recentJourney.entityType of
        DRecntLoc.MULTIMODAL -> getMultiModalModes req recentJourney merchantId
        _ -> do
          pastJourney <- QJourney.findByPrimaryKey (Id.Id recentJourney.entityId) >>= Utils.fromMaybeM (Error.InternalError "Journey not found")
          sreq <- QSearchRequest.findById pastJourney.searchRequestId >>= Utils.fromMaybeM (Error.InternalError "Search request not found")
          pure $ Just $ mkRecentLocation sreq Nothing recentJourney req
  userPref <- DAUM.getMultimodalUserPreferences (Just req.personId, merchantId)

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
              sortingType = convertSortingType $ fromMaybe DMP.FASTEST userPref.journeyOptionsSortingType
            }
    transitServiceReq <- TMultiModal.getTransitServiceReq merchantId req.merchantOperatingCityId
    multimodalRoutes <- MultiModal.getTransitRoutes transitServiceReq transitRoutesReq >>= Utils.fromMaybeM (Error.InternalError "routes dont exist")

    pure $
      API.PopularLocation
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
          rating = loc.rating,
          type_ = loc.type_,
          multimodalRoutes = multimodalRoutes
        }

  -- Sort by distance and return response
  let sortedPopularLocations = sortOn (.distance) popularLocationsWithDistance
  let sortedRecentLocations = sortOn (.distance) (catMaybes recentLocations)

  pure
    API.PlacesResponse
      { popularLocations = sortedPopularLocations,
        recentLocations = sortedRecentLocations
      }
