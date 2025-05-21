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
import RemoteConfig.Utils as RCU
import Common.RemoteConfig.Utils as RU
import Common.Types.App as CT
import Accessor (_code)
import Data.Lens ((^.))
import Effect.Uncurried (runEffectFn3)
import Animation (fadeInWithDelay)
import Data.Foldable (minimum, maximum)

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
                  let _ = runFn2 setInCache "POLLING_ID" initialState.data.busRouteCode
                  if (showPreBookingTracking "BUS") 
                    then lift $ lift $ busLocationTracking 3000.0 0 initialState push 0
                    else pure unit
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
                , padding $ PaddingBottom bottomSheetHeight
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
            , margin $ Margin 16 16 0 0
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
  where
    bottomSheetHeight = Mb.fromMaybe 25 $ maximum [25, (JB.getLayoutBounds $ EHC.getNewIDWithTag "busStopsView").height -100 ]

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
    , width $ V 22
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
      lineViewHeight = max 36 (HU.getDefaultPixelSize lineViewBounds.height) 
      totalDistance = 50.0
      currentDistance = 5.0
      marginTop = min 2 (DI.round (percent * (DI.toNumber lineViewHeight))) 
    linearLayout 
      [height MATCH_PARENT
      , width MATCH_PARENT
      , visibility $ boolToVisibility $ showPreBookingTracking "BUS"
      ]
      [ imageView
        [ width MATCH_PARENT
        , height $ V 26
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
              , peakHeight 250
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
                       else if lb.height < 100 then 150
                       else lb.height
  scrollView
    [ height $ V scrollViewHeight
    , width MATCH_PARENT      
    , nestedScrollView true
    , id $ EHC.getNewIDWithTag "busStopsView"
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
          , gravity CENTER 
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
  let etaTimeAndTimeStampTuple = calculateMinEtaTimeWithDelay state.data.vehicleData
      mbEtaTime = DT.fst etaTimeAndTimeStampTuple
      mbTimestamp = DT.snd etaTimeAndTimeStampTuple
  in 
    linearLayout
    ( [ height WRAP_CONTENT
      , orientation VERTICAL
      , id $ EHC.getNewIDWithTag $ "stopListView"  <> if showOnlyBullet then "onlyBullet" else ""
      , margin $ MarginHorizontal 0 16
      -- , layoutGravity "center_horizontal"
      -- , gravity CENTER
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
        ]
        (DA.mapWithIndex (\index item@(API.FRFSStationAPI station) -> stopView item showOnlyBullet stopMarginTop state push index (getStopType station.code index state) state.props.minimumEtaDistance mbEtaTime state.props.isMinimumEtaDistanceAvailable mbTimestamp state.data.vehicleData) (if state.props.individualBusTracking && DA.length state.data.stopsList > 2 then stops else state.data.stopsList))
    ]
  where
  stopMarginTop = if state.props.showRouteDetailsTab then 36 else 12
  
  stops = if DA.length state.data.stopsList <= 2 then [] else DA.slice 1 (DA.length state.data.stopsList - 1) state.data.stopsList
  -- findNextBusETA index = Mb.maybe Mb.Nothing (\stop -> (findStopInVehicleData stop state) >>= (\item -> item.nextStopTravelTime)) (stops DA.!! (index - 1))

  -- findETADistance stop = 
  --   if (Mb.isJust $ state.data.nearestStopFromCurrentLoc <#> (\(API.FRFSStationAPI nearestStop) -> nearestStop.code == stop.code))
  --     then state.props.minimumEtaDistance
  --     else Mb.Nothing

  findNextBusDistance index =
    Mb.maybe 
      Mb.Nothing
      (\stop -> 
        case state.props.individualBusTracking, findStopInVehicleData stop state of
          true, _ -> Mb.Nothing
          _, Mb.Nothing -> Mb.Nothing
          false, Mb.Just vehicleStopData -> Mb.Just $ DI.floor $ vehicleStopData.nextStopDistance
      ) 
      (stops DA.!! (index - 1))

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
        -- , textView
        --     $ [ text "16 min"
        --       ]
        --     <> FontStyle.tags CTA.TypoGraphy
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

stopView :: forall w. API.FRFSStationAPI -> Boolean -> Int -> ST.BusTrackingScreenState -> (Action -> Effect Unit) -> Int -> StopType -> Mb.Maybe Int -> Mb.Maybe Int -> Mb.Maybe Boolean -> Mb.Maybe String -> Array ST.VehicleData -> PrestoDOM (Effect Unit) w
stopView (API.FRFSStationAPI stop) showOnlyBullet marginTop state push index stopType etaDistance mbEtaTime isMinimumEtaDistanceAvailable mbTimestamp vehicleData =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ height $ if index /=0 && checkPreviousStopIsSource && (DA.length state.data.stopsList /= 2 || Mb.isNothing state.props.vehicleTrackingId) then WRAP_CONTENT else V 36
        , width MATCH_PARENT
        , visibility $ boolToVisibility $ index /= 0
        ]
        [ verticalLineView push ("verticalLineView1" <> show showOnlyBullet <> show index) showOnlyBullet $ DM.lookup stop.code state.data.vehicleTrackingData
        -- , if stopType == SOURCE_STOP && index /=0 && (showPreBookingTracking "BUS") && (Mb.isNothing state.props.vehicleTrackingId)
        , if (index /=0 && checkPreviousStopIsSource && (showPreBookingTracking "BUS") && (Mb.isNothing state.props.vehicleTrackingId))
            then showETAView push state index (API.FRFSStationAPI stop) showOnlyBullet etaDistance mbEtaTime isMinimumEtaDistanceAvailable mbTimestamp vehicleData
            else 
              MP.noView
        ]
    , linearLayout
        [ height $ V 36
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        ]
        [ relativeLayout
            [ height MATCH_PARENT
            , gravity CENTER
            ]
            [ verticalLineView push ("verticalLineView2" <> show showOnlyBullet <> show index) (if stopType == NORMAL_STOP then showOnlyBullet else false) Mb.Nothing
            , linearLayout
              [ height MATCH_PARENT
              , gravity CENTER
              ]
              [ imageView
                [ height $ V imageDimension
                , width $ V imageDimension
                , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET (getStopImage stopType state.data.rideType)
                , margin $ MarginLeft (if stopType == NORMAL_STOP then 6 else 2)
                , visibility $ boolToVisibility showOnlyBullet
                ]
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
    checkPreviousStopIsSource =
      case state.data.stopsList DA.!! (index - 1) of
        Mb.Just (API.FRFSStationAPI prevStop) ->
          (getStopType prevStop.code (index - 1) state) == SOURCE_STOP
        _ -> false

separatorView :: forall w. String -> Margin -> PrestoDOM (Effect Unit) w
separatorView color' margin' =
  linearLayout
    [ height (V 1)
    , width MATCH_PARENT
    , margin margin'
    , background color'
    ]
    []

busLocationTracking :: Number -> Int -> ST.BusTrackingScreenState -> (Action -> Effect Unit) -> Int -> Flow GlobalState Unit
busLocationTracking duration id state push count = do
  let
    routeCode = state.data.busRouteCode
    trackingId = runFn3 getFromCache "POLLING_ID" Mb.Nothing Mb.Just
  if Mb.isJust trackingId && trackingId /= Mb.Just routeCode then
    pure unit
  else do
    EHC.liftFlow $ JB.getCurrentPositionWithTimeout push CurrentLocationCallBack 3500 true
    eitherRespOrError <- Remote.trackRouteVehicles $ API.BusTrackingRouteReq routeCode
    case eitherRespOrError of
      Right (API.BusTrackingRouteResp resp) -> do
        EHC.liftFlow $ push $ UpdateTracking (API.BusTrackingRouteResp resp) count $ JB.getKeyInSharedPrefKeys $ show ONBOARDED_VEHICLE_INFO
        void $ delay $ Milliseconds $ duration
        busLocationTracking duration id state push (count + 1)
      Left err -> do
        void $ delay $ Milliseconds $ duration
        busLocationTracking duration id state push (count + 1)


showETAView :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> Int -> API.FRFSStationAPI -> Boolean -> Mb.Maybe Int -> Mb.Maybe Int -> Mb.Maybe Boolean -> Mb.Maybe String -> Array ST.VehicleData -> PrestoDOM (Effect Unit) w
showETAView push state index (API.FRFSStationAPI stop) showOnlyHeight mbETADistance mbETATime isMinimumEtaDistanceAvailable mbTimestamp vehicleData =
  PrestoAnim.animationSet [fadeInWithDelay 0 true]
  $ linearLayout
  ([ height if showOnlyHeight then V (HU.getDefaultPixelSize lb.height) else WRAP_CONTENT
  , width  MATCH_PARENT
  , orientation VERTICAL
  , stroke $ "1," <> Color.grey900
  , padding $ Padding 8 8 8 8
  , cornerRadius 12.0
  , visibility $ boolToVisibility $ not state.props.individualBusTracking
  , onAnimationEnd 
      ( \_ ->
          runEffectFn3 JB.scrollToChildInScrollView 
            (EHC.getNewIDWithTag "busStopsView") 
            (EHC.getNewIDWithTag ("verticalLineView1false17")) 
            (show $ Mb.maybe 0 (\(API.FRFSStationAPI stop) -> Mb.fromMaybe 0 $ minimum [(DA.length state.data.stopsList) - 3, ((Mb.fromMaybe 0 stop.sequenceNum) - 1)]) mbPickupStop)
      )
      (const $ if isNearToLastStop == Mb.Just true then UpdateToExpandView else NoAction)
  ] <> if showOnlyHeight then [] else [id $ EHC.getNewIDWithTag $ "ETAVIEW" <> show index])
  [ linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    , visibility $ boolToVisibility $ not state.props.individualBusTracking
    , margin $ MarginBottom 4
    ]
    [ imageView
      [ height $ V 16
      , width $ V 16
      , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET "ny_ic_blue_location"
      , margin $ Margin 6 2 0 0
      ]
    , textView
      $ [ text $ if Mb.isJust state.data.sourceStation then "Pickup Stop" else "Nearest Stop"
        , margin $ MarginLeft 4
        , color Color.blue900
        ]
      <> FontStyle.body1 CTA.TypoGraphy
    ]
  , textView
    $ [ text etaTextInMinutesOrDistance
      , margin $ MarginLeft 8
      , visibility $ boolToVisibility $ not showOnlyHeight && not state.props.individualBusTracking && (Mb.isJust isMinimumEtaDistanceAvailable) && etaTextInMinutesOrDistance /= ""
      ]
    <> FontStyle.body1 CTA.TypoGraphy
  -- , linearLayout [weight 1.0] []
  -- This was dummy code to show the time in the UI, should be properly handled as per backend response
  -- , textView
  --   $ [ text $  extractTimeInHHMMA $ Mb.maybe (EHC.getCurrentUTC "") (\item ->  Mb.fromMaybe (EHC.getCurrentUTC "") (show <$> item.mbNextStopTravelDistance)) $ findStopInVehicleData (API.FRFSStationAPI stop) state
  -- , visibility $ boolToVisibility $ not showOnlyHeight
  --     ]
  --   <> FontStyle.body1 CTA.TypoGraphy
  ]
  where
    extractTimeInHHMMA :: String -> String
    extractTimeInHHMMA timeStr = EHC.convertUTCtoISC timeStr "hh" <> ":" <> EHC.convertUTCtoISC timeStr "mm" <> " " <> EHC.convertUTCtoISC timeStr "a"
    
    lb = JB.getLayoutBounds $ EHC.getNewIDWithTag $ "ETAVIEW" <> show index

    mbPickupStop = 
      case state.data.sourceStation of
        Mb.Just sourceStation -> DA.find (\(API.FRFSStationAPI item) -> item.code == sourceStation.stationCode) state.data.stopsList
        Mb.Nothing -> state.data.nearestStopFromCurrentLoc
    
    etaTextInMinutesOrDistance = 
      case isMinimumEtaDistanceAvailable, mbETATime of
        Mb.Just true, Mb.Just etaTimeInSeconds -> 
          let secondsTohhmm = HU.secondsToHms etaTimeInSeconds
          in "Next bus in " <> (if secondsTohhmm == "--" then "less than 1 min" else secondsTohhmm)
        Mb.Just true, Mb.Nothing ->
          let 
            timestampPart =
              case mbTimestamp of
                Mb.Just timeStamp -> " (Updated at: " <> (EHC.convertUTCtoISC timeStamp "hh:mm a") <> ")"
                Mb.Nothing -> ""
          in "Bus is " <> (JB.fromMetersToKm $ Mb.fromMaybe 0 mbETADistance) <> " away." <> timestampPart
        Mb.Just false, Mb.Nothing -> "No bus is coming towards your stop"
        _, _ -> ""
    
    isNearToLastStop = mbPickupStop <#> (\(API.FRFSStationAPI stop) -> (DA.length state.data.stopsList) - 3 > ((Mb.fromMaybe 0 stop.sequenceNum) - 1))
    
    -- roundedNextStopDistance :: String
    -- roundedNextStopDistance = 
    --   let nextStopDistance = Mb.fromMaybe 0 mbNextStopDistance
    --   in show $ if nextStopDistance > 1000 then (show $ nextStopDistance / 1000) <> " kms away" else (show nextStopDistance) <> " meters away"

    -- metersToKminNumber :: Number -> String
    -- metersToKminNumber distance =
    --   if (distance < 1000.0) then (HU.toStringJSON distance <> " m " <> (getString AWAY_C)) else (HU.parseFloat (distance / 1000.0)) 2 <> " km " <> (getString AWAY_C)

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
    [ linearLayout
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
      , onClick push $ const $ UserBoarded Mb.Nothing
      , color Color.yellow800
      ] <> FontStyle.body1 CTA.TypoGraphy
    ]


showPreBookingTracking :: String -> Boolean
showPreBookingTracking _ = 
      let busConfig = RCU.getBusFlowConfigs (getValueToLocalStore CUSTOMER_LOCATION)
      in busConfig.showPreBookingTracking

checkCurrentBusIsOnboarded :: ST.BusTrackingScreenState -> String -> Boolean
checkCurrentBusIsOnboarded state vehicleId = 
  let cachedBusOnboardingInfo = extractBusOnboardingInfo $ JB.getKeyInSharedPrefKeys $ show ONBOARDED_VEHICLE_INFO
  in (DA.null cachedBusOnboardingInfo || (not state.props.individualBusTracking) || (Mb.isJust $ DA.find (\busInfo -> busInfo.vehicleId == vehicleId) cachedBusOnboardingInfo))