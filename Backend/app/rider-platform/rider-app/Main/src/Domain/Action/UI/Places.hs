{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.Places (postPlaces) where

import qualified API.Types.UI.Places as API
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DPerson
import qualified Environment as Env
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Google.MapsClient.Types
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.MultiModal.Interface as MultiModal
import qualified Kernel.Prelude
import Kernel.Types.Common (Meters (..))
import qualified Kernel.Types.Error as Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import qualified Kernel.Utils.Common as Utils
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRiderConfig
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.PopularLocation as QPopularLocation
import qualified Tools.MultiModal as TMultiModal

postPlaces ::
  ( (Kernel.Prelude.Maybe (Id.Id DPerson.Person), Id.Id DMerchant.Merchant) ->
    API.PlacesRequest ->
    Env.Flow API.PlacesResponse
  )
postPlaces (mbPersonId, merchantId) req = do
  -- Get popular locations based on merchantOperatingCityId
  popularLocations <- QPopularLocation.findAllByMerchantOperatingCityId req.merchantOperatingCityId

  -- Get rider config for multimodal settings
  riderConfig <-
    CQRiderConfig.findByMerchantOperatingCityId req.merchantOperatingCityId Nothing
      >>= Utils.fromMaybeM (Error.InternalError "Rider config not found")

  -- Get recent locations within 100m radius of user's current location
  let searchRadius = Meters 100
  recentLocations <- case mbPersonId of
    Just personId -> do
      -- Get completed bookings for the person in this MOC
      bookings <-
        QBooking.findAllByRiderIdAndStatusAndMOCId
          personId
          DBooking.COMPLETED
          req.merchantOperatingCityId

      -- Filter bookings within radius and convert to RecentLocation format
      let bookingsInRadius =
            filter
              ( \booking ->
                  let distance =
                        distanceBetweenInMeters
                          (LatLong req.userLat req.userLon)
                          (LatLong booking.fromLocation.lat booking.fromLocation.lon)
                   in (realToFrac distance :: Double) <= (realToFrac searchRadius :: Double)
              )
              bookings

      -- Convert bookings to RecentLocation format with multimodal routes
      forM bookingsInRadius $ \booking -> do
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
                                  { latitude = booking.fromLocation.lat,
                                    longitude = booking.fromLocation.lon
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
                  maxAllowedPublicTransportLegs = riderConfig.maxAllowedPublicTransportLegs
                }
        transitServiceReq <- TMultiModal.getTransitServiceReq merchantId req.merchantOperatingCityId
        multimodalRoutes <- MultiModal.getTransitRoutes transitServiceReq transitRoutesReq >>= Utils.fromMaybeM (Error.InternalError "routes dont exist")

        -- Get journey legs for the past booking if journeyId exists
        pastJourneyLegs <- case booking.journeyId of
          Just journeyId -> QJourneyLeg.findAllByJourneyId journeyId
          Nothing -> pure []

        let filteredRoutes =
              if null pastJourneyLegs
                then multimodalRoutes.routes
                else
                  let routesWithPastLeg =
                        filter
                          ( \route ->
                              any
                                ( \routeLeg ->
                                    any
                                      ( \pastLeg ->
                                          let pastMode = case pastLeg.mode of
                                                DCommon.Metro -> MultiModal.MetroRail
                                                DCommon.Bus -> MultiModal.Bus
                                                DCommon.Walk -> MultiModal.Walk
                                                DCommon.Taxi -> MultiModal.Unspecified
                                                DCommon.Subway -> MultiModal.Subway
                                           in pastMode == routeLeg.mode
                                                && pastLeg.agency == routeLeg.agency
                                      )
                                      pastJourneyLegs
                                )
                                route.legs
                          )
                          multimodalRoutes.routes
                   in if null routesWithPastLeg
                        then multimodalRoutes.routes
                        else routesWithPastLeg

        pure $
          API.RecentLocation
            { name = fromMaybe "" booking.fromLocation.address.building,
              address =
                mconcat
                  [ fromMaybe "" booking.fromLocation.address.building,
                    ", ",
                    fromMaybe "" booking.fromLocation.address.street,
                    ", ",
                    fromMaybe "" booking.fromLocation.address.city,
                    ", ",
                    fromMaybe "" booking.fromLocation.address.state,
                    ", ",
                    fromMaybe "" booking.fromLocation.address.country
                  ],
              lat = booking.fromLocation.lat,
              lon = booking.fromLocation.lon,
              distance =
                Just $
                  realToFrac $
                    distanceBetweenInMeters
                      (LatLong req.userLat req.userLon)
                      (LatLong booking.fromLocation.lat booking.fromLocation.lon),
              multimodalRoutes = MultiModal.MultiModalResponse {routes = filteredRoutes}
            }
    Nothing -> pure []

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
              maxAllowedPublicTransportLegs = riderConfig.maxAllowedPublicTransportLegs
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
  let sortedRecentLocations = sortOn (.distance) recentLocations

  pure
    API.PlacesResponse
      { popularLocations = sortedPopularLocations,
        recentLocations = sortedRecentLocations
      }
