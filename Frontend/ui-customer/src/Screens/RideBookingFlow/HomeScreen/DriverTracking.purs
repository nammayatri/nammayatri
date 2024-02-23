module Screens.HomeScreen.DriverTracking where

import Constants.Configs (getPolylineAnimationConfig)
import Screens.RideBookingFlow.HomeScreen.Config (metersToKm, specialLocationConfig, specialLocationIcons)
import Services.API (GetDriverLocationResp(..), GetRouteResp(..), LatLong(..), RideAPIEntity(..), RideBookingRes(..), Route(..), Snapped(..))
import Accessor (_id, _lat, _lon)
import Common.Resources.Constants (zoomLevel)
import Data.Array (any, length, (!!))
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn2)
import Data.Int (pow, toNumber)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Uncurried (runEffectFn1)
import Engineering.Helpers.Commons (flowRunner, getValueFromIdMap, liftFlow, updatePushInIdMap, updateIdMap)
import Helpers.Pooling (delay)
import JBridge (animateCamera, drawRoute, enableMyLocation, getExtendedPath, isCoordOnPath, removeAllPolylines, updateRoute, updateRouteConfig)
import Prelude (Unit, bind, discard, map, pure, unit, void, when, ($), (&&), (*), (+), (/), (/=), (==), (>), (>=))
import Presto.Core.Types.Language.Flow (Flow, getState, modifyState)
import Screens.HomeScreen.Controller (Action(..))
import Screens.HomeScreen.Transformer (getActiveBooking, getDriverInfo)
import Screens.Types (SearchResultType(..), Stage(..))
import Services.Backend (getDriverLocation, getRoute, getRouteMarkers, makeGetRouteReq, rideBooking, walkCoordinate, walkCoordinates)
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore, isLocalStageOn)
import Types.App (GlobalState(..))

rideStatusPolling :: (Action -> Effect Unit) -> Number -> Int -> String -> Int -> Flow GlobalState Unit
rideStatusPolling push duration id routeType exp = do
  (GlobalState globalState) <- getState
  let state = globalState.homeScreen
  if state.props.bookingId /= "" 
    then do
      liftFlow $ startDriverTracking (GlobalState globalState) -- Starting the driver tracking in seperate flow. 
      rideStatus <- liftFlow $ runEffectFn1 getValueFromIdMap "RideStatusPolling"
      when (rideStatus.id == id) $ do -- Chceking current polling flow is the latest flow we started.
        void $ pure $ runFn2 updatePushInIdMap "RideStatusPolling" false
        eiBookingResp <- rideBooking (state.props.bookingId)
        case eiBookingResp of
          Right respBooking -> do 
            handleBookingResp respBooking
            void $ delay $ Milliseconds duration
            rideStatusPolling push duration id routeType exp
          Left _ -> do
            void $ delay $ Milliseconds $ getDuration state.data.config.driverLocationPolling.retryExpFactor exp duration
            rideStatusPolling push duration id routeType $ getExp state.data.config.driverLocationPolling.maxPower exp
    else do
      updateBookingDetailsFromList state -- Getting the booking data from ride/list if bookingId is ""
      gState <- getState
      liftFlow $ startDriverTracking gState
      void $ delay $ Milliseconds duration
      rideStatusPolling push duration id routeType exp
  where
    startDriverTracking gState = do
      driverTracking <- runEffectFn1 getValueFromIdMap "RideTracking"
      when (driverTracking.shouldPush) $ void $ launchAff $ flowRunner gState $ rideTracking push duration driverTracking.id routeType 0
    handleBookingResp (RideBookingRes respBooking) = do
      let bookingStatus = respBooking.status
      case bookingStatus of
        "REALLOCATED" -> do
            liftFlow $ push $ UpdateCurrentStage bookingStatus
        _             -> do
            case (respBooking.rideList !! 0) of
              Just (RideAPIEntity res) -> do
                let rideStatus = res.status
                liftFlow $ push $ UpdateCurrentStage rideStatus
                if (res.driverArrivalTime /= Nothing  && (getValueToLocalStore DRIVER_ARRIVAL_ACTION) == "TRIGGER_DRIVER_ARRIVAL" ) then 
                  liftFlow $ push $ DriverArrivedAction (fromMaybe "" res.driverArrivalTime)
                else pure unit
              Nothing -> pure unit
    updateBookingDetailsFromList state = do
      mbResp <- getActiveBooking
      case mbResp of
        Nothing -> do 
          void $ delay $ Milliseconds $ getDuration state.data.config.driverLocationPolling.retryExpFactor exp duration -- Should not happend
          rideStatusPolling push duration id routeType $ getExp state.data.config.driverLocationPolling.maxPower exp
        Just resp -> do
          void $ modifyState \(GlobalState globalState) -> GlobalState $ globalState { homeScreen {props{bookingId = resp ^._id}, data{driverInfoCardState = getDriverInfo state.data.specialZoneSelectedVariant resp (state.data.currentSearchResultType == QUOTES)}}}
          void $ liftFlow $ push $ UpdateBookingDetails resp
          handleBookingResp resp


rideTracking :: (Action -> Effect Unit) -> Number -> Int -> String -> Int -> Flow GlobalState Unit
rideTracking push duration id routeType exp = do
  (GlobalState gs) <- getState
  let state = gs.homeScreen
  driverTracking <- liftFlow $ runEffectFn1 getValueFromIdMap "RideTracking"
  when (driverTracking.id == id) $ do
    void $ pure $ runFn2 updatePushInIdMap "RideTracking" false
    if (state.data.currentSearchResultType == QUOTES) && (isLocalStageOn RideAccepted) then do -- Special Zone case showing only source marker
        addSpecialZoneMarker state
        void $ delay $ Milliseconds duration
        rideTracking push duration id routeType exp
        else do
          response <- getDriverLocation state.data.driverInfoCardState.rideId -- Normal driver location API call 
          case response of
            Right (GetDriverLocationResp resp) -> do
              tracking <- liftFlow $ runEffectFn1 getValueFromIdMap "RideTracking"
              when (tracking.id == id) $ do
                let srcLat = resp ^. _lat
                    srcLon = resp ^. _lon
                    isRideStarted = any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver]
                    dstLat = if isRideStarted then state.data.driverInfoCardState.sourceLat else state.data.driverInfoCardState.destinationLat
                    dstLon = if isRideStarted then state.data.driverInfoCardState.sourceLng else state.data.driverInfoCardState.destinationLng
                    trackingType = if isRideStarted then Remote.DRIVER_TRACKING else Remote.RIDE_TRACKING
                    markers = getRouteMarkers state.data.driverInfoCardState.vehicleVariant state.props.city trackingType 
                    sourceSpecialTagIcon = specialLocationIcons state.props.zoneType.sourceTag
                    destSpecialTagIcon = specialLocationIcons state.props.zoneType.destinationTag
                void $ pure $ runFn2 updatePushInIdMap "RideTracking" false
                case state.data.route of
                  Just (Route route) -> do
                        locationResp <- liftFlow $ isCoordOnPath (walkCoordinates route.points) (resp ^. _lat) (resp ^. _lon) (state.data.speed) -- Checking the current lat long exist in the route or not.
                        if locationResp.isInPath then do
                          let newPoints = { points : locationResp.points}
                              specialLocationTag =  if (any (\stage -> isLocalStageOn stage) [ RideAccepted, ChatWithDriver]) then
                                                      specialLocationConfig "" sourceSpecialTagIcon true getPolylineAnimationConfig
                                                    else
                                                      specialLocationConfig "" destSpecialTagIcon false getPolylineAnimationConfig
                          -- Updating to the current lat long which exist in the route.
                          liftFlow $ runEffectFn1 updateRoute updateRouteConfig { json = newPoints, destMarker =  markers.destMarker, eta =  (metersToKm locationResp.distance (state.props.currentStage == RideStarted)), srcMarker =  markers.srcMarker, specialLocation = specialLocationTag, zoomLevel = zoomLevel} 
                          
                          liftFlow $ push $ UpdateETA locationResp.eta locationResp.distance
                          void $ delay $ Milliseconds duration
                          rideTracking push duration id routeType exp
                        else do
                          void $ modifyState \(GlobalState globalState) -> GlobalState $ globalState { homeScreen {data{route = Nothing}}}
                          rideTracking push duration id routeType exp
                  Nothing -> do
                      let specialLocationTag =  if isRideStarted then
                                        specialLocationConfig destSpecialTagIcon sourceSpecialTagIcon true getPolylineAnimationConfig
                                      else
                                        specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon false getPolylineAnimationConfig
                      routeResponse <- getRoute routeType $ makeGetRouteReq srcLat srcLon dstLat dstLon
                      case routeResponse of
                        Right (GetRouteResp routeResp) -> do
                          case ((routeResp) !! 0) of
                            Just (Route routes) -> do
                              _ <- pure $ removeAllPolylines ""
                              let Snapped routePoints = routes.points
                                  newPoints = 
                                    if length routePoints > 1 
                                      then
                                        getExtendedPath (walkCoordinates routes.points)
                                      else
                                        walkCoordinate srcLat srcLon dstLat dstLon
                                  newRoute = routes { points = Snapped (map (\item -> LatLong { lat: item.lat, lon: item.lng }) newPoints.points) }
                              liftFlow $ drawRoute newPoints "LineString" "#323643" true markers.srcMarker markers.destMarker 8 "DRIVER_LOCATION_UPDATE" "" (metersToKm routes.distance (state.props.currentStage == RideStarted)) specialLocationTag
                              liftFlow $ push $ UpdateETA routes.duration routes.distance
                              void $ delay $ Milliseconds duration
                              void $ modifyState \(GlobalState globalState) -> GlobalState $ globalState { homeScreen {data { route = Just (Route newRoute), speed = routes.distance / routes.duration } }}
                              rideTracking push duration id routeType exp
                            Nothing -> do
                              void $ delay $ Milliseconds $ getDuration state.data.config.driverLocationPolling.retryExpFactor exp duration
                              rideTracking push duration id routeType $ getExp state.data.config.driverLocationPolling.maxPower exp
                        Left _ -> do
                          void $ delay $ Milliseconds $ getDuration state.data.config.driverLocationPolling.retryExpFactor exp duration
                          rideTracking push duration id routeType $ getExp state.data.config.driverLocationPolling.maxPower exp
            Left _ -> do
              void $ delay $ Milliseconds $ getDuration state.data.config.driverLocationPolling.retryExpFactor exp duration
              rideTracking push duration id routeType $ getExp state.data.config.driverLocationPolling.maxPower exp
    where
      addSpecialZoneMarker state = do
        void $ pure $ enableMyLocation true
        void $ pure $ removeAllPolylines ""
        void $ liftFlow $ animateCamera state.data.driverInfoCardState.sourceLat state.data.driverInfoCardState.sourceLng zoomLevel "ZOOM"

getDuration :: Int -> Int -> Number -> Number
getDuration factor counter duration = duration * (toNumber $ pow factor counter)

getExp :: Int -> Int -> Int
getExp maxPower exp = if exp >= maxPower then 0 else exp + 1


resetTracking :: Effect Unit
resetTracking = do 
  void $ runEffectFn1 updateIdMap "RideStatusPolling"
  void $ runEffectFn1 updateIdMap "RideTracking"