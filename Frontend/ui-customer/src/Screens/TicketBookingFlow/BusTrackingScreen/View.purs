{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TicketBookingFlow.BusTrackingScreen.View where

import Data.Ord(min, max)
import Debug
import Prelude
import PrestoDOM
import Screens.TicketBookingFlow.BusTrackingScreen.Controller
import Screens.TicketBookingFlow.BusTrackingScreen.ScreenData
import Accessor (_lat, _lon)
import Animation as Anim
import Animation.Config (translateFullYAnimWithDurationConfig, translateYAnimConfig, Direction(..), AnimConfig, animConfig)
import CarouselHolder as CarouselHolder
import Common.Resources.Constants as CRC
import Common.Types.App as CTA
import Components.BannerCarousel as BannerCarousel
import Components.BoxContainer as BoxContainer
import Components.DropDownWithHeader as DropDownWithHeader
import Components.GenericHeader as GenericHeader
import Components.InfoBox as InfoBox
import Components.PrimaryButton as PrimaryButton
import Constants.Configs (getPolylineAnimationConfig)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array as DA
import Data.String as DS
import Data.Lens ((^.))
import Data.Maybe as Mb
import Effect (Effect)
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.BackTrack (liftFlowBT)
import Helpers.API as HelpersAPI
import Font.Style as FontStyle
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToInvisibility, boolToVisibility)
import Screens.NammaSafetyFlow.Components.HelperViews (layoutWithWeight)
import Mobility.Prelude as MP
import Presto.Core.Types.Language.Flow (Flow)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout)
import PrestoDOM.List (ListItem, preComputeListItem)
import Data.Time.Duration (Milliseconds(..))
import PrestoDOM.Properties (sheetState, cornerRadii)
import Presto.Core.Types.Language.Flow (doAff, Flow, delay)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.RideBookingFlow.HomeScreen.Config as HSConfig
import Screens.Types as ST
import Services.API as API
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (GlobalState(..), defaultGlobalState)
import Components.SeparatorView.View as SeparatorView
import Types.EndPoint (shareRide)
import Screens.TicketBookingFlow.BusTrackingScreen.ComponentConfig
import Control.Monad.Trans.Class (lift)
import Data.Either
import Storage (getValueToLocalStore, KeyStore(..))
import Data.Traversable (traverse, for)
import Data.Int as DI
import Data.Function.Uncurried (runFn2, runFn3)
import LocalStorage.Cache
import Data.Tuple as DT
import Data.Map as DM
import Screens.TicketBookingFlow.BusTrackingScreen.Transformer

screen :: ST.BusTrackingScreenState -> Screen Action ST.BusTrackingScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "BusTrackingScreen"
  , globalEvents:
      [ ( \push -> do
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
              $ do
                  let city = getValueToLocalStore CUSTOMER_LOCATION
                  if (DA.null initialState.data.stopsList) then do
                    getMetroStationResp <- Remote.getMetroStationBT "BUS" city initialState.data.busRouteCode "" (show initialState.props.srcLat <> "," <> show initialState.props.srcLon)
                    liftFlowBT $ push $ UpdateStops getMetroStationResp
                  else
                    liftFlowBT $ push $ UpdateStops (API.GetMetroStationResponse initialState.data.stopsList)
                  let _ = runFn2 setInCache "BUS_LOCATION_TRACKING" initialState.data.busRouteCode
                  lift $ lift $ busLocationTracking 3000.0 0 initialState.data.busRouteCode push
                  pure unit
            pure $ pure unit
        )
      ]
  , eval:
      ( \action state -> do
          let
            _ = spy "BusTrackingScreen state -----" state

            _ = spy "BusTrackingScreen--------action" action
          eval action state
      )
  }

view :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , onBackPressed push $ const BackPressed
        , background Color.white900
        ]
        [ PrestoAnim.animationSet
            [ Anim.fadeIn true
            ]
            $ linearLayout
                [ width MATCH_PARENT
                , height MATCH_PARENT
                , id $ EHC.getNewIDWithTag "BusTrackingScreenMap"
                , onAnimationEnd
                    ( \action -> do
                        void $ JB.showMap (EHC.getNewIDWithTag "BusTrackingScreenMap") true "satellite" 14.0 state.props.srcLat state.props.srcLon push MapReady
                        push action
                    )
                    (const NoAction)
                ]
                []
        , relativeLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            ]
            [ bottomSheetView push state
            ] 
        , linearLayout
            [ height $ V 48
            , width $ V 48
            , cornerRadius 25.0
            , gravity CENTER
            , background Color.white900
            , margin $ Margin 16 EHC.safeMarginTop 0 0
            , onClick push $ const BackPressed
            ]
            [ imageView
                [ width $ V 24
                , height $ V 24
                , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_chevron_left"
                , rippleColor Color.rippleShade
                ]
            ]
        ]

journeyLegTitleView :: forall w. Boolean -> String -> ST.BusTrackingScreenState -> PrestoDOM (Effect Unit) w
journeyLegTitleView isSource name state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginVertical 8 (if state.props.showRouteDetailsTab then 0 else 8)
    , background Color.white900
    , visibility $ boolToVisibility $ state.props.individualBusTracking && DA.length state.data.stopsList > 2
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , cornerRadius 25.0
        , padding $ Padding 5 5 5 5
        , background if isSource then Color.green900 else Color.red
        ]
        [ imageView
            [ height $ V 10
            , width $ V 10
            , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_circle_white"
            ]
        ]
    , textView
        $ [ text name
          , margin $ MarginLeft 16
          ]
        <> FontStyle.body1 CTA.TypoGraphy
    ]


verticalLineView :: forall w. (Action -> Effect Unit) -> String -> Boolean -> Mb.Maybe (Array Number) -> PrestoDOM (Effect Unit) w --ST.BusTrackingScreenState -> Boolean -> PrestoDOM (Effect Unit) w
verticalLineView push idTag showOnlyBullet vehicles =  
  relativeLayout
    [ height MATCH_PARENT
    , width $ V 20
    , clipChildren false
    , visibility $ boolToVisibility showOnlyBullet
    , background Color.white900 
    ]
    $ [ linearLayout
          [ height MATCH_PARENT
          , width $ V 2
          , margin $ MarginLeft 9
          , background Color.black900
          , id $ EHC.getNewIDWithTag idTag
          ]
          []
      ]
    <> case vehicles of
        Mb.Just v -> (map (\p -> busMarkerView p) v)
        _ -> []
  where
  

  busMarkerView :: Number -> PrestoDOM (Effect Unit) w
  busMarkerView percent = do
    let
      lineViewBounds = JB.getLayoutBounds $ EHC.getNewIDWithTag idTag
      lineViewHeight = max 32 (HU.getDefaultPixelSize lineViewBounds.height) 
      totalDistance = 50.0
      currentDistance = 5.0
      marginTop = min 12 (DI.round (percent * (DI.toNumber lineViewHeight))) 
    linearLayout 
      [height MATCH_PARENT
      , width MATCH_PARENT
      ]
      [ imageView
        [ width MATCH_PARENT
        , height $ V 20
        , margin $ MarginTop $ marginTop
        , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET "ny_ic_bus_marker_with_arrow"
        ]
      ]

knobView :: forall w. PrestoDOM (Effect Unit) w
knobView =
  linearLayout
    [ height $ V 4
    , width $ V 34
    , background Color.transparentGrey
    , margin $ MarginVertical 4 4
    , cornerRadius 4.0
    ]
    []

bottomSheetView :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> PrestoDOM (Effect Unit) w
bottomSheetView push state =  
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , alignParentBottom "true,-1"
    , orientation VERTICAL
    ]
    [ coordinatorLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , cornerRadii $ Corners 24.0 true true false false
        ]
        [ bottomSheetLayout
            ( [ height WRAP_CONTENT
              , width MATCH_PARENT
              , peakHeight 200
              , enableShift false
              , cornerRadii $ Corners 24.0 true true false false
              ]
                <> if state.props.expandStopsView then  [ sheetState EXPANDED ] else []
            ) 
            [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                [ passengerBoardConfirmationPopup state push
                , linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    , gravity CENTER
                    , background if state.props.showRouteDetailsTab then Color.white900 else Color.grey700
                    , cornerRadii $ Corners 24.0 true true false false
                    ]
                    [ bottomSheetContentView push state
                    ]
                ]
            ]
        ]
        ,  linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , gravity CENTER
            , visibility $ boolToVisibility state.props.showRouteDetailsTab
            , background Color.white900
            ]
            [ separatorView Color.grey900 (MarginTop 10)
            , PrimaryButton.view (push <<< BookTicketButtonAction) (primaryButtonConfig state)
            ]
    ]

bottomSheetContentView :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> PrestoDOM (Effect Unit) w
bottomSheetContentView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.transparent
    , orientation VERTICAL
    , gravity CENTER
    , cornerRadii $ Corners 24.0 true true false false
    ]
    [ knobView
    , linearLayout
        ( [ height WRAP_CONTENT
          , width MATCH_PARENT
          , background Color.white900
          , orientation VERTICAL
          , gravity CENTER
          , cornerRadii $ Corners 24.0 true true (not state.props.showRouteDetailsTab) (not state.props.showRouteDetailsTab)
          ]
            <> if state.props.showRouteDetailsTab then
                []
              else
                [ margin $ Margin 16 8 16 16 ]
        )
        [ busDetailsView push state
        , busStopsView push state
        
        ]
    ]

separatorConfig :: SeparatorView.Config
separatorConfig =
  { orientation: VERTICAL
  , count: 50
  , height: V 10
  , width: V 2
  , layoutWidth: V 2
  , layoutHeight: V 100
  , color: Color.black500
  }

actionListVIew :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> PrestoDOM (Effect Unit) w
actionListVIew push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    []

busDetailsView :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> PrestoDOM (Effect Unit) w
busDetailsView push state =
  linearLayout
    ( [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ]
        <> if state.props.showRouteDetailsTab then
            []
          else
            [ padding $ Padding 16 8 16 16 ]
    )
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        ]
        [ linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , weight 1.0
            , gravity if state.props.showRouteDetailsTab then CENTER else LEFT
            ]
            [ textView
                $ [ text $ getString $ BUS_NO state.data.routeShortName
                  , margin $ MarginLeft 8
                  , color Color.black800
                  ]
                <> FontStyle.h2 CTA.TypoGraphy
            , textView
                $ [ text $ getString $ TOWARDS_STATION (Mb.maybe "" (\(API.FRFSStationAPI item) -> MP.spaceSeparatedPascalCase item.name) (DA.last state.data.stopsList))
                  , margin $ MarginLeft 8
                  , color Color.black700
                  ]
                <> FontStyle.body3 CTA.TypoGraphy
            ]
        , textView
            $ [ text $ getString VIEW_TICKET
              , cornerRadius 8.0
              , color Color.blue800
              , background Color.blue600
              , padding $ Padding 12 8 12 8
              , visibility $ boolToVisibility $ not state.props.showRouteDetailsTab
              , onClick push $ const ViewTicket
              ]
            <> FontStyle.tags CTA.TypoGraphy
        ]
    , separatorView Color.grey900 (MarginTop 10)
    ]

busStopsView :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> PrestoDOM (Effect Unit) w
busStopsView push state = do
  let
    lb = JB.getLayoutBounds $ EHC.getNewIDWithTag "stopListView"

    scrollViewHeight = if state.props.showShimmer then 300 
                       else if lb.height > 300 then 300 
                       else lb.height
  scrollView
    [ height $ V scrollViewHeight
    , width MATCH_PARENT      
    , nestedScrollView true
    ]
    [ 
      relativeLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      ]
      [ 
        linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , margin $ MarginHorizontal 16 16
        , visibility $ boolToVisibility $ not state.props.showShimmer
        ]
        [ journeyLegTitleView true (Mb.maybe "" (\(API.FRFSStationAPI item) -> item.name) (DA.head state.data.stopsList)) state
        , linearLayout 
          [ width MATCH_PARENT
          , height WRAP_CONTENT 
          ] 
          [ stopListView push state true
          , stopListView push state false 
          ]
        , journeyLegTitleView false (Mb.maybe "" (\(API.FRFSStationAPI item) -> item.name) (DA.last state.data.stopsList)) state --fromMaybe ""  (state.data.destinationStation <#> ._stationName) --"KSR Bengaluru Railway Station, M.G. Railway Colony, Sevashrama, Bengaluru, Karnataka 560023"
        ]
      , shimmerView state 
      ]
    ]

stopListView :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> Boolean -> PrestoDOM (Effect Unit) w
stopListView push state showOnlyBullet =
  linearLayout
    ( [ height WRAP_CONTENT
      , orientation VERTICAL
      , id $ EHC.getNewIDWithTag $ "stopListView"  <> if showOnlyBullet then "onlyBullet" else ""
      , margin $ MarginHorizontal 0 16
      ]
        <> if showOnlyBullet then
            [ width $ V 20 ]
          else
            [ weight 1.0 ]
              <> if not state.props.showRouteDetailsTab && state.props.individualBusTracking && DA.length state.data.stopsList > 2 then
                  [ cornerRadius 12.0
                  , stroke $ "1," <> Color.grey900
                  , padding $ PaddingBottom $ if state.props.expandStopsView then 12 else 0]
                else
                  []
    )
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ]
        [ verticalLineView push ("verticalLineView2" <> show showOnlyBullet <> "0000") showOnlyBullet Mb.Nothing
        , stopsViewHeader push state showOnlyBullet
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , visibility $ boolToVisibility $ state.props.showRouteDetailsTab || state.props.expandStopsView
        ]
        (DA.mapWithIndex (\index item@(API.FRFSStationAPI station) -> stopView item showOnlyBullet stopMarginTop state push index (getStopType station.code index state) (findNextBusETA index)) (if state.props.individualBusTracking && DA.length state.data.stopsList > 2 then stops else state.data.stopsList))
    ]
  where
  stopMarginTop = if state.props.showRouteDetailsTab then 32 else 12
  
  stops = if DA.length state.data.stopsList <= 2 then [] else DA.slice 1 (DA.length state.data.stopsList - 1) state.data.stopsList

  findNextBusETA index = Mb.maybe Mb.Nothing (\stop -> (findStopInVehicleData stop state) >>= (\item -> item.nextStopTravelTime)) (stops DA.!! (index - 1))

stopsViewHeader :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> Boolean -> PrestoDOM (Effect Unit) w
stopsViewHeader push state showOnlyBullet =
  linearLayout
    [ weight 1.0
    , height $ V 50
    , orientation VERTICAL
    , visibility $ boolToVisibility $ state.props.individualBusTracking && not state.props.showRouteDetailsTab && DA.length state.data.stopsList > 2
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , visibility $ boolToVisibility $ not state.props.showRouteDetailsTab
        , padding $ Padding 16 16 16 16
        ]
        [ textView
            $ [ text $ show (DA.length state.data.stopsList - 1) <> " stops left"
              ]
            <> FontStyle.tags CTA.TypoGraphy
        , imageView
            [ height $ V 4
            , width $ V 4
            , margin $ MarginHorizontal 4 4
            , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET "ny_ic_dot"
            ]
        , textView
            $ [ text "16 min"
              ]
            <> FontStyle.tags CTA.TypoGraphy
        , layoutWithWeight
        , imageView
            [ imageWithFallback
                $ HU.fetchImage HU.FF_ASSET
                    if state.props.expandStopsView then
                      "ny_ic_chevron_up"
                    else
                      "ny_ic_chevron_down"
            , height $ V 12
            , width $ V 12
            , margin $ MarginLeft 6
            , onClick push $ const ToggleStops
            ]
        ]
    , if state.props.expandStopsView then
        separatorView Color.grey900 (MarginTop 0)
      else
        MP.noView
    ]

stopView :: forall w. API.FRFSStationAPI -> Boolean -> Int -> ST.BusTrackingScreenState -> (Action -> Effect Unit) -> Int -> StopType -> Mb.Maybe Int -> PrestoDOM (Effect Unit) w
stopView (API.FRFSStationAPI stop) showOnlyBullet marginTop state push index stopType etaTime =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ height $ if stopType == SOURCE_STOP && index /=0 && Mb.isJust etaTime then WRAP_CONTENT else V 32
        , width MATCH_PARENT
        , visibility $ boolToVisibility $ index /= 0
        ]
        [ verticalLineView push ("verticalLineView1" <> show showOnlyBullet <> show index) showOnlyBullet $ DM.lookup stop.code state.data.vehicleTrackingData
        , if stopType == SOURCE_STOP && index /=0 
            then showETAView push state index (API.FRFSStationAPI stop) showOnlyBullet etaTime
            else 
              MP.noView
        ]
    , linearLayout
        [ height $ V 32
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        ]
        [ relativeLayout
            [ height MATCH_PARENT
            , gravity CENTER
            -- , width WRAP_CONTENT
            ]
            [ verticalLineView push ("verticalLineView2" <> show showOnlyBullet <> show index) (if stopType == NORMAL_STOP then showOnlyBullet else false) Mb.Nothing
            , linearLayout
              [ height MATCH_PARENT
              , gravity CENTER
              , width WRAP_CONTENT
              ]
              [ imageView $
                [ height $ V imageDimension
                , width $ V imageDimension
                , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET (getStopImage stopType)
                , visibility $ boolToVisibility showOnlyBullet
                ] <> if EHC.os == "IOS" then [gravity CENTER] else [margin $ MarginLeft (if stopType == NORMAL_STOP then 6 else 2)]
              ]
            ]
          , textView
            $ [ text $ MP.spaceSeparatedPascalCase stop.name
              , margin $ MarginLeft if showOnlyBullet || not state.props.showRouteDetailsTab then 8 else 0
              , visibility $ boolToVisibility $ not showOnlyBullet
              , width $ if showOnlyBullet then V 0 else WRAP_CONTENT
              , singleLine true
              , ellipsize true
              , color Color.black800
              ]
            <> (if DA.elem stopType [SOURCE_STOP, DESTINATION_STOP] then FontStyle.body4 else FontStyle.body1) CTA.TypoGraphy
        ]
    ]
  where
    totalHeight = if (Mb.isJust $ findStopInVehicleData (API.FRFSStationAPI stop) state) then 72 else 40
    imageDimension = if stopType == NORMAL_STOP then 8 else 16

separatorView :: forall w. String -> Margin -> PrestoDOM (Effect Unit) w
separatorView color' margin' =
  linearLayout
    [ height (V 1)
    , width MATCH_PARENT
    , margin margin'
    , background color'
    ]
    []

busLocationTracking :: Number -> Int -> String -> (Action -> Effect Unit) -> Flow GlobalState Unit
busLocationTracking duration id routeCode push = do
  let
    trackingId = runFn3 getFromCache "BUS_LOCATION_TRACKING" Mb.Nothing Mb.Just
  if Mb.isJust trackingId && trackingId /= Mb.Just routeCode then
    pure unit
  else do
    EHC.liftFlow $ JB.getCurrentPositionWithTimeout push CurrentLocationCallBack 3500 true
    resp <- HelpersAPI.callApi $ API.BusTrackingRouteReq routeCode
    case resp of
      Right (API.BusTrackingRouteResp resp) -> do
        trackingInfo <-
          for resp.vehicleTrackingInfo
            $ \(API.VehicleInfo item) -> do
                let
                  (API.VehicleInfoForRoute m) = item.vehicleInfo
                  (API.RouteStopMapping nextStop) = item.nextStop
                  lat = Mb.fromMaybe 0.0 m.latitude
                  lon = Mb.fromMaybe 0.0 m.longitude
                  markerConfig = JB.defaultMarkerConfig { markerId = item.vehicleId, pointerIcon = "ny_ic_bus_marker" }
                  (API.LatLong nextStopPosition) = nextStop.stopPoint
                void $ EHC.liftFlow $ JB.showMarker markerConfig lat lon 160 0.5 0.9 (EHC.getNewIDWithTag "BusTrackingScreenMap")
                pure { vehicleId: item.vehicleId, nextStop: nextStop.stopCode, nextStopDistance: HU.getDistanceBwCordinates lat lon nextStopPosition.lat nextStopPosition.lon, vehicleLat: lat, vehicleLon: lon, nextStopLat: nextStopPosition.lat, nextStopLon: nextStopPosition.lon, nextStopTravelTime : item.nextStopTravelTime, nextStopSequence : nextStop.sequenceNum}
        EHC.liftFlow $ push $ UpdateTracking trackingInfo
        void $ delay $ Milliseconds $ duration
        busLocationTracking duration id routeCode push
      Left err -> do
        void $ delay $ Milliseconds $ duration
        busLocationTracking duration id routeCode push


showETAView :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> Int -> API.FRFSStationAPI -> Boolean -> Mb.Maybe Int -> PrestoDOM (Effect Unit) w
showETAView push state index (API.FRFSStationAPI stop) showOnlyHeight nextStopTravelTime =
  linearLayout
  ([ height if showOnlyHeight then V (HU.getDefaultPixelSize lb.height) else WRAP_CONTENT
  , width  MATCH_PARENT
  , orientation HORIZONTAL
  , stroke $ "1," <> Color.grey900
  , padding $ Padding 8 8 8 8
  , cornerRadius 12.0
  , visibility $ boolToVisibility $ Mb.isJust nextStopTravelTime
  
  ] <> if showOnlyHeight then [] else [id $ EHC.getNewIDWithTag $ "ETAVIEW" <> show index])
  [ textView
    $ [ text $ "Next Bus in " <> (show $ (Mb.fromMaybe 0 nextStopTravelTime) / 60) <> " min"
      , margin $ MarginLeft 8
  , visibility $ boolToVisibility $ not showOnlyHeight
      ]
    <> FontStyle.body1 CTA.TypoGraphy
  , linearLayout [weight 1.0] []
  -- This was dummy code to show the time in the UI, should be properly handled as per backend response
  -- , textView
  --   $ [ text $  extractTimeInHHMMA $ Mb.maybe (EHC.getCurrentUTC "") (\item ->  Mb.fromMaybe (EHC.getCurrentUTC "") (show <$> item.nextStopTravelTime)) $ findStopInVehicleData (API.FRFSStationAPI stop) state
  -- , visibility $ boolToVisibility $ not showOnlyHeight
  --     ]
  --   <> FontStyle.body1 CTA.TypoGraphy
  ]
  where
    extractTimeInHHMMA timeStr = EHC.convertUTCtoISC timeStr "hh" <> ":" <> EHC.convertUTCtoISC timeStr "mm" <> " " <> EHC.convertUTCtoISC timeStr "a"
    lb = JB.getLayoutBounds $ EHC.getNewIDWithTag $ "ETAVIEW" <> show index

findStopInVehicleData :: API.FRFSStationAPI -> ST.BusTrackingScreenState -> Mb.Maybe ST.VehicleData
findStopInVehicleData (API.FRFSStationAPI stop) state = DA.find (\item -> item.nextStop == stop.code) state.data.vehicleData -- && Mb.isJust item.nextStopTravelTime

shimmerView :: forall w. ST.BusTrackingScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
  shimmerFrameLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.transparent
    , visibility $ boolToVisibility state.props.showShimmer
    ] 
    [ 
      linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , visibility $ boolToVisibility state.props.showShimmer
        ]
        ( map
            ( \_ ->
                linearLayout
                  [ height $ V 40
                  , background Color.greyDark
                  , cornerRadius 12.0
                  , width MATCH_PARENT
                  , stroke $ "1," <> Color.grey900
                  , margin $ Margin 16 16 16 16
                  ]
                  []
            )
            (1 DA... 4)
        )
    ]

passengerBoardConfirmationPopup :: forall w. ST.BusTrackingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
passengerBoardConfirmationPopup state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 16 16 16 16
    , margin $ Margin 8 8 8 8
    , background Color.black900
    , cornerRadius 16.0
    , visibility $ boolToVisibility $ Mb.isJust state.props.busNearSourceData && (not $ DS.null state.data.bookingId) && not state.props.individualBusTracking
    ]
    [ textView $
      [ text $ getString BUS_BOARDED_CONFIRMATION
      , color Color.white900
      ] <> FontStyle.body1 CTA.TypoGraphy
    , layoutWithWeight
    , textView $
      [text $ getString YES
      , onClick push $ const UserBoarded
      , color Color.yellow800
      ] <> FontStyle.body1 CTA.TypoGraphy
    ]
