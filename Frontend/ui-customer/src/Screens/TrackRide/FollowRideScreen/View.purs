{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.FollowRideScreen.View where

import Accessor (_lat, _lon)
import Animation (fadeIn, fadeOut, screenAnimation)
import Common.Resources.Constants (pickupZoomLevel, zoomLevel)
import Common.Types.App (LazyCheck(..), Paths)
import Components.DriverInfoCard.Common.View (addressShimmerView, driverDetailsView, driverInfoShimmer, sourceDestinationView)
import Constants.Configs (getPolylineAnimationConfig)
import Data.Array (any, head, length, mapWithIndex, null, (!!))
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn2)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff)
import Effect.Uncurried (runEffectFn1)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag, getValueFromIdMap, liftFlow, os, updatePushInIdMap)
import Helpers.Utils (FetchImageFrom(..), fetchImage, storeCallBackCustomer)
import JBridge (addAndUpdateRideSafeOverlay, addAndUpdateSOSRipples, addNavigateMarker, animateCamera, drawRoute, enableMyLocation, getExtendedPath, isCoordOnPath, removeAllPolylines, removeMarker, removeSOSRipples, showMap, updateRoute, updateRouteConfig)
import Mobility.Prelude (boolToVisibility)
import Prelude
import PrestoDOM (PrestoDOM, Screen, onAnimationEnd, onBackPressed, onClick)
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout, frameLayout, imageView, linearLayout, relativeLayout, scrollView, textView)
import PrestoDOM.Properties (accessibility, alpha, background, clickable, color, cornerRadii, cornerRadius, enableShift, gradient, gravity, height, id, imageWithFallback, margin, orientation, padding, peakHeight, stroke, text, visibility, weight, width)
import PrestoDOM.Types.DomAttributes (Accessiblity(..), Corners(..), Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..))
import Screens.FollowRideScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.FollowRideScreen.ScreenData (mockDriverInfo, mockDriverLocation, mockRoute)
import Screens.FollowRideScreen.Config (SOSOverlayConfig, genericHeaderConfig, getCurrentFollower, getDriverDetails, getFollowerName, getPeekHeight, getSosOverlayConfig, getTripDetails, primaryButtonConfig)
import Services.API (GetDriverLocationResp(..), GetRouteResp(..), LatLong(..), RideBookingListRes(..), RideBookingRes, Route(..), Snapped(..))
import Services.Backend (TrackingType(..), drawMapRoute, getDriverLocation, getRoute, getRouteMarkers, makeGetRouteReq, normalRoute, rideBooking, rideBookingList, walkCoordinate, walkCoordinates)
import Types.App (GlobalState(..), defaultGlobalState)
import Common.Types.App as Common
import Components.GenericHeader as GenericHeader
import Data.String (take)
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Presto.Core.Types.Language.Flow (Flow, delay, getState, modifyState)
import PrestoDOM.Animation as PrestoAnim
import Screens.RideBookingFlow.HomeScreen.Config as HSConfig
import Screens.Types (DriverInfoCard, EmAudioPlayStatus(..), FollowRideScreenStage(..), FollowRideScreenState, Followers)
import Styles.Colors as Color
import Screens.HomeScreen.Transformer (getDriverInfo)
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Components.PrimaryButton as PrimaryButton


screen :: FollowRideScreenState -> GlobalState -> Screen Action FollowRideScreenState ScreenOutput
screen initialState globalState =
  { initialState
  , view : view globalState
  , name: "FollowRideScreen"
  , globalEvents:
      [ ( \push -> do
            case initialState.data.currentStage of
              FollowingRide -> do
                _ <- pure $ enableMyLocation false
                tracking <- runEffectFn1 getValueFromIdMap "FollowsRide"
                storeCallBackCustomer push NotificationListener "FollowRideScreen"
                when tracking.shouldPush $ void $ launchAff $ flowRunner globalState $ driverLocationTracking push UpdateStatus 5000.0 tracking.id "trip"
              _ -> pure unit
            pure $ pure unit
        )
      ]
  , eval:
      \action state -> do
        let
          _ = spy "FollowRideScreen action " action
        let
          _ = spy "FollowRideScreen state " state
        eval action state
  }

type Layout w
  = PrestoDOM (Effect Unit) w

view ::
  forall w.
  GlobalState ->
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
view globalState push state =
  screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity CENTER
        , orientation VERTICAL
        , onBackPressed push $ const BackPressed
        ]
    $ case state.data.currentStage of
        PersonList -> [ followersListView push state ]
        _ -> [ followingRideView globalState push state ]

followingRideView ::
  forall w.
  GlobalState ->
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
followingRideView globalState push state =
  frameLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    ]
    $ [ PrestoAnim.animationSet
        [ fadeIn state.props.startMapAnimation
        ] $ linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , id $ getNewIDWithTag "FollowRideMap"
          , onAnimationEnd
              getPush
              (const NoAction)
          ]
          []
      , bottomSheetView push state
      , rideCompletedView push state
      ]
    <> if state.data.currentStage == RideCompletedStage 
        then [] 
        else 
          case state.data.sosStatus of
          Just status -> case status of
            Common.NotResolved -> [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state) ]
            _ -> [ sosOverlayView push (getSosOverlayConfig state status) ]
          Nothing -> [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state) ]
  where
    getPush = (\action -> do
                void $ showMap (getNewIDWithTag "FollowRideMap") true "satellite" pickupZoomLevel push MapReady
                when (state.data.currentStage == MockFollowRide) $ void $ launchAff $ flowRunner defaultGlobalState $ updateMockData push
                push action)

followersListView ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
followersListView push state =
  let
    len = length state.data.followers
  in
    linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      ]
      [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
      , scrollView
          [ height MATCH_PARENT
          , width MATCH_PARENT
          ]
          [ linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , background Color.white900
              ]
              ( mapWithIndex
                  ( \idx item ->
                      linearLayout
                        [ height WRAP_CONTENT
                        , width MATCH_PARENT
                        , orientation VERTICAL
                        ]
                        $ [ followerItem item push ]
                        <> if idx /= (len - 1) then [ separatorView ] else []
                  )
                  state.data.followers
              )
          ]
      ]


rideCompletedView ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
rideCompletedView push state =
  let currentFollower = getCurrentFollower state.data.currentFollower
      name = getFollowerName currentFollower state
  in
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.blackLessTrans
  , visibility $ boolToVisibility $ state.data.currentStage == RideCompletedStage
  , gravity BOTTOM
  ][  linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      , gravity CENTER
      , cornerRadii $ Corners 24.0 true true false false
      ][  imageView
          [ height $ V 82
          , width $ V 82
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_green_tick"
          , margin $ MarginTop 24
          ]
        , textView $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ getString $ REACHED_DESTINATION_SAFELY name
          , color Color.black900
          , margin $ MarginTop 24
          , gravity CENTER
          ] <> FontStyle.h1 TypoGraphy
        , linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , margin $ MarginTop 32
          ][ sourceDestinationView push (getTripDetails state Color.blue600)]
        , PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)
      ]
  ]


followerItem :: forall w. Followers -> (Action -> Effect Unit) -> Layout w
followerItem item push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 16 16 16 16
    , background Color.white900
    , onClick push $ const (PersonSelected item)
    , gravity CENTER
    ]
    [ linearLayout
        [ height $ V 32
        , width $ V 32
        , gravity CENTER
        , cornerRadius 16.0
        , background Color.blue800
        ]
        [ textView
            $ [ text $ take 1 $ fromMaybe item.mobileNumber item.name
              , color Color.white900
              , width WRAP_CONTENT
              , height WRAP_CONTENT
              ]
            <> FontStyle.tags TypoGraphy
        ]
    , textView
        $ [ text $ fromMaybe item.mobileNumber item.name
          , color Color.black800
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , margin $ MarginLeft 8
          ]
        <> FontStyle.tags TypoGraphy
    , linearLayout
        [ weight 1.0
        , height WRAP_CONTENT
        ]
        []
    , imageView
        [ height $ V 20
        , width $ V 20
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right"
        ]
    ]

bottomSheetView ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
bottomSheetView push state =
  coordinatorLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ bottomSheetLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , peakHeight $ getPeekHeight state
        , enableShift false
        , orientation VERTICAL
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                , visibility $ boolToVisibility $ any (_ == state.data.emergencyAudioStatus) [ STARTED, RESTARTED ]
                , padding $ PaddingBottom 10
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , background Color.white900
                    , cornerRadius 32.0
                    , padding $ Padding 20 12 20 12
                    , onClick push $ const StopAudioPlayer
                    ]
                    [ imageView
                        [ height $ V 20
                        , width $ V 20
                        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_alarm_new"
                        ]
                    , textView
                        $ [ text $ getString TURN_OFF_ALARM
                          , color Color.black800
                          ]
                        <> FontStyle.tags TypoGraphy
                    ]
                ]
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , background Color.grey700
                , orientation VERTICAL
                , gravity CENTER
                , cornerRadii $ Corners 24.0 true true false false
                ]
                [ linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , orientation VERTICAL
                    , gravity CENTER
                    , id $ getNewIDWithTag "FollowRideHeaderView"
                    ]
                    [ knobView
                    , headerView push state
                    , emergencyActionsView push state
                    ]
                , addressView push state
                , driverInfoView push state
                ]
            ]
        ]
    ]

sosOverlayView :: forall w. (Action -> Effect Unit) -> SOSOverlayConfig -> Layout w
sosOverlayView push config =
  let currentGradient = if os == "IOS" then Linear 270.0 [ Color.white900, Color.white900, Color.white900,  Color.transparent ] else Linear 180.0 [Color.white900, Color.white900, Color.white900,  Color.transparent ]
  in
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding $ Padding 16 10 16 70
    , gradient currentGradient
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity RIGHT
        , onClick push $ const BackPressed
        ]
        [ imageView
            [ height $ V 24
            , width $ V 24
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close"
            ]
        ]
    , textView
        $ [ width MATCH_PARENT
          , height WRAP_CONTENT
          , text $ config.title
          , color config.color
          , gravity CENTER
          ]
        <> FontStyle.h2 TypoGraphy
    , textView
        $ [ width MATCH_PARENT
          , height WRAP_CONTENT
          , text $ config.subTitle
          , gravity CENTER
          , margin $ MarginTop 6
          , color config.color
          ]
        <> FontStyle.body1 TypoGraphy
    ]


knobView :: forall w. Layout w
knobView =
  linearLayout
    [ height $ V 4
    , width $ V 34
    , background Color.transparentGrey
    , margin $ MarginTop 8
    , cornerRadius 4.0
    ]
    []

driverInfoView ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
driverInfoView push state =
  let
    isRideData = isJust state.data.driverInfoCardState
  in
    relativeLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , padding $ PaddingTop 0
      ]
      [ PrestoAnim.animationSet [ fadeIn isRideData ]
          $ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , visibility $ boolToVisibility isRideData
              , padding $ PaddingBottom 16
              ]
              [ driverDetailsView (getDriverDetails state) "FollowRideDriverDetailsView"
              ]
      , PrestoAnim.animationSet [ fadeOut isRideData ]
          $ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , visibility $ boolToVisibility $ not isRideData
              ]
              [ driverInfoShimmer
              ]
      ]

addressView ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
addressView push state =
  let
    isRideData = isJust state.data.driverInfoCardState
  in
    relativeLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , id $ getNewIDWithTag "FollowRideSourceDestinationView"
      ]
      [ PrestoAnim.animationSet [ fadeIn isRideData ]
          $ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , visibility $ boolToVisibility isRideData
              ]
              [ sourceDestinationView push (getTripDetails state Color.white900)
              ]
      , PrestoAnim.animationSet [ fadeOut isRideData ]
          $ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , visibility $ boolToVisibility $ not isRideData
              ]
              [ addressShimmerView
              ]
      ]

headerView ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
headerView push state =
  let
    currentFollower = getCurrentFollower state.data.currentFollower
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , padding $ Padding 16 12 16 6
      , gravity CENTER
      ]
      [ textView
          $ [ text $ (getFollowerName currentFollower state) <> " " <> getString IS_ON_THE_WAY
            , weight 1.0
            , height WRAP_CONTENT
            , color Color.black900
            ]
          <> FontStyle.body7 TypoGraphy
      , linearLayout
          []
          []
      , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , gravity CENTER
          , cornerRadius if os == "IOS" then 20.0 else 32.0
          , background Color.green100
          , onClick push $ const $ MessageEmergencyContact
          , accessibility ENABLE
          , alpha $ if state.data.currentStage == MockFollowRide then 0.5 else 1.0
          , clickable $ state.data.currentStage /= MockFollowRide
          -- , visibility $ boolToVisibility $ currentFollower.priority == 0 needed when chat is added
          , padding $ Padding 20 10 20 10
          ]
          [ imageView
              [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_call"
              , height $ V 20
              , width $ V 20
              ]
          ]
      ]


emergencyActionsView ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
emergencyActionsView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , padding $ Padding 16 12 16 12
    , margin $ Margin 16 6 16 12
    , cornerRadius 8.0
    ]
    [ buttonView push state
    ]

buttonView ::
  forall w.
  (Action -> Effect Unit) ->
  FollowRideScreenState ->
  Layout w
buttonView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , stroke $ "1," <> Color.grey900
    , cornerRadius 24.0
    , gravity CENTER
    , padding $ Padding 16 12 16 12
    , onClick push $ const CallPolice
    , alpha $ if state.data.currentStage == MockFollowRide then 0.5 else 1.0
    , clickable $ state.data.currentStage /= MockFollowRide
    ]
    [ imageView
        [ width $ V 20
        , height $ V 20
        , imageWithFallback ""
        ]
    , textView
        $ [ text $ getString CALL_POLICE
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          , color Color.black800
          ]
        <> FontStyle.tags TypoGraphy
    ]

driverLocationTracking :: (Action -> Effect Unit) -> (RideBookingRes -> Action) -> Number -> Int -> String -> Flow GlobalState Unit
driverLocationTracking push action duration id routeState = do
  (GlobalState gs) <- getState
  let state = gs.followRideScreen
  trackingId <- liftFlow $ runEffectFn1 getValueFromIdMap "FollowsRide"
  when (id == trackingId.id) $ do
    let _ = runFn2 updatePushInIdMap "FollowsRide" false
    when (isJust state.data.currentFollower)
      $ do
          let follower = getCurrentFollower state.data.currentFollower
          resp <- rideBooking follower.bookingId
          case resp of
            Right respBooking -> do
              liftFlow $ push $ action respBooking
              when ((getPeekHeight state) == 300) $ liftFlow $ push $ UpdatePeekHeight
            Left err -> pure unit
    case state.data.driverInfoCardState of
      Nothing -> pure unit
      Just ride -> do
        response <- getDriverLocation ride.rideId
        case response of
          Right resp -> do
            trackingId <- liftFlow $ runEffectFn1 getValueFromIdMap "FollowsRide"
            if id == trackingId.id then do
              case state.data.route of
                Nothing -> routeNotExist resp ride state
                Just route -> routeExist resp route ride state
              else pure unit
          Left _ -> do
            void $ delay $ Milliseconds $ duration * 2.0
            resetRoute
            driverLocationTracking push action duration id routeState
  where
  resetRoute :: Flow GlobalState Unit
  resetRoute =
    void
      $ modifyState \(GlobalState globalState) ->
          GlobalState
            $ globalState
                { followRideScreen
                  { data
                    { route = Nothing
                    }
                  }
                }
  routeNotExist :: GetDriverLocationResp -> DriverInfoCard -> FollowRideScreenState -> Flow GlobalState Unit
  routeNotExist (GetDriverLocationResp resp) ride state = do
    let srcLat = (resp ^. _lat)
        srcLon = (resp ^. _lon)
        dstLat = ride.destinationLat
        dstLon = ride.destinationLng
        destination = ride.destination
        markers = getRouteMarkers ride.vehicleVariant state.props.city RIDE_TRACKING
        sourceSpecialTagIcon = HSConfig.specialLocationIcons state.data.zoneType.sourceTag
        destSpecialTagIcon = HSConfig.specialLocationIcons state.data.zoneType.destinationTag
        specialLocationTag = HSConfig.specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon false getPolylineAnimationConfig
    routeResponse <- getRoute routeState $ makeGetRouteReq srcLat srcLon dstLat dstLon
    case routeResponse of
      Right (GetRouteResp routeResp) -> do
        case ((routeResp) !! 0) of
          Just (Route routes) -> do
            void $ pure $ removeAllPolylines ""
            void $ liftFlow $ removeSOSRipples ""
            let
              (Snapped routePoints) = routes.points
              newPoints =
                if length routePoints > 1 then
                  getExtendedPath (walkCoordinates routes.points)
                else
                  walkCoordinate srcLat srcLon dstLat dstLon
              newRoute = routes { points = Snapped (map (\item -> LatLong { lat: item.lat, lon: item.lng }) newPoints.points) }
              point = {lat :srcLat , lng : srcLon} 
            addSosMarkers state.data.sosStatus point
            liftFlow $ drawRoute newPoints "LineString" "#323643" true markers.srcMarker markers.destMarker 8 "DRIVER_LOCATION_UPDATE" "" ride.destination specialLocationTag
            liftFlow $ animateCamera srcLat srcLon 17.0 "ZOOM"
            void $ delay $ Milliseconds duration
            void
              $ modifyState \(GlobalState globalState) ->
                  GlobalState
                    $ globalState
                        { followRideScreen
                          { data
                            { route = Just (Route newRoute)
                            , speed = routes.distance / routes.duration
                            }
                          }
                        }
            driverLocationTracking push action duration id routeState
          Nothing -> do
            void $ delay $ Milliseconds $ duration * 2.0
            driverLocationTracking push action duration id routeState
      Left _ -> do
        void $ delay $ Milliseconds $ duration * 2.0
        driverLocationTracking push action duration id routeState
  routeExist :: GetDriverLocationResp -> Route -> DriverInfoCard -> FollowRideScreenState -> Flow GlobalState Unit
  routeExist (GetDriverLocationResp resp) (Route route) ride state = do
    let srcLat = (resp ^. _lat)
        srcLon = (resp ^. _lon)
        dstLat = ride.destinationLat
        dstLon = ride.destinationLng
        markers = getRouteMarkers ride.vehicleVariant state.props.city RIDE_TRACKING
        sourceSpecialTagIcon = HSConfig.specialLocationIcons state.data.zoneType.sourceTag
        destSpecialTagIcon = HSConfig.specialLocationIcons state.data.zoneType.destinationTag
        specialLocationTag = HSConfig.specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon false getPolylineAnimationConfig
    locationResp <- liftFlow $ isCoordOnPath (walkCoordinates route.points) (resp ^. _lat) (resp ^. _lon) (state.data.speed)
    if locationResp.isInPath then do
      let newPoints = { points: locationResp.points }
          mbPoint = head locationResp.points
      liftFlow
        $ runEffectFn1 updateRoute
            updateRouteConfig { json = newPoints, destMarker = markers.destMarker, eta = (HSConfig.metersToKm locationResp.distance true), srcMarker = markers.srcMarker, specialLocation = specialLocationTag, zoomLevel = zoomLevel, autoZoom = false }
      case mbPoint of
        Just point -> addSosMarkers state.data.sosStatus point
        Nothing -> pure unit
      liftFlow $ animateCamera srcLat srcLon 17.0 "ZOOM"
      void $ delay $ Milliseconds duration
      driverLocationTracking push action duration id routeState
    else do
      resetRoute
      driverLocationTracking push action duration id routeState
  addSosMarkers :: Maybe Common.SosStatus -> Paths -> Flow GlobalState Unit
  addSosMarkers mbStatus point = 
    case mbStatus of
      Nothing -> pure unit
      Just status -> case status of
          Common.Resolved -> do
            _ <- pure $ removeMarker "SOSMarkerLabel"
            liftFlow $ addAndUpdateRideSafeOverlay point
          Common.Pending -> do
            liftFlow $ addAndUpdateSOSRipples point
            liftFlow $ addNavigateMarker point push OnNavigate
          _ -> pure unit

separatorView :: forall w. PrestoDOM (Effect Unit) w
separatorView =
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.greySmoke
    ]
    []


updateMockData :: (Action -> Effect Unit) -> Flow GlobalState Unit
updateMockData push = do
  _ <- pure $ spy "updateMockData" "updateMockData"
  rideBookingListResponse <- rideBookingList "1" "0" "true"
  case rideBookingListResponse of 
    Right (RideBookingListRes listResp) -> do
      if null listResp.list 
        then defaultMockFlow
        else do
          let mbRideBooking = head listResp.list
          case mbRideBooking of
            Nothing -> defaultMockFlow
            Just ride -> do
              let driverInfoCardState = getDriverInfo Nothing ride false
              eiResp <- getDriverLocation driverInfoCardState.rideId
              case eiResp of
                Right resp -> currentRideDataMockFlow driverInfoCardState resp
                Left _ -> defaultMockFlow
    Left _ -> defaultMockFlow
  where
    defaultMockFlow :: Flow GlobalState Unit
    defaultMockFlow = do 
      let srcPoint = getPoint mockDriverLocation
      pushAction $ UpdateMockData mockDriverInfo
      drawDriverRoute mockDriverInfo srcPoint $ Just mockRoute
      pushSOSStatus Common.Pending
      localDelay 10000.0
      liftFlow $ addAndUpdateRideSafeOverlay srcPoint
      pushSOSStatus Common.Resolved
      localDelay 10000.0
      pushAction BackPressed
    currentRideDataMockFlow :: DriverInfoCard -> GetDriverLocationResp -> Flow GlobalState Unit
    currentRideDataMockFlow ride resp = do
      let srcPoint = getPoint resp
      pushAction $ UpdateMockData ride
      drawDriverRoute ride srcPoint Nothing
      pushSOSStatus Common.Pending
      localDelay 10000.0
      liftFlow $ addAndUpdateRideSafeOverlay srcPoint
      pushSOSStatus Common.Resolved
      localDelay 10000.0
      pushAction $ UpdateCurrentStage RideCompletedStage
    pushAction :: Action ->  Flow GlobalState Unit
    pushAction action = liftFlow $ push $ action
    localDelay :: Number -> Flow GlobalState Unit
    localDelay seconds = void $ delay $ Milliseconds seconds
    pushSOSStatus :: Common.SosStatus -> Flow GlobalState Unit
    pushSOSStatus status = liftFlow $ push $ UpdateMockSOSStatus status
    drawDriverRoute :: DriverInfoCard -> Paths -> Maybe Route -> Flow GlobalState Unit
    drawDriverRoute ride srcPoint route = do
      let srcLat = srcPoint.lat
          srcLon = srcPoint.lng
          dstLat = ride.destinationLat
          dstLon = ride.destinationLng
      void $ runExceptT $ runBackT $ drawMapRoute srcLat srcLon dstLat dstLon (normalRoute "") "NORMAL" (getString SOS_LOCATION) (getString DROP) route "trip" $ (HSConfig.specialLocationConfig "" "" false getPolylineAnimationConfig){autoZoom = false}
      liftFlow $ addAndUpdateSOSRipples srcPoint
      liftFlow $ animateCamera srcLat srcLon 17.0 "ZOOM"
    getPoint :: GetDriverLocationResp -> Paths
    getPoint (GetDriverLocationResp resp) = {lat :resp ^. _lat , lng : resp ^. _lon} 

