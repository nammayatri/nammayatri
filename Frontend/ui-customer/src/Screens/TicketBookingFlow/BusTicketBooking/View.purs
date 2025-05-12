module Screens.TicketBookingFlow.BusTicketBooking.View 
  ( busTicketBookingScreen
  , view
  )
  where

import Prelude

import Animation as Anim
import Animation.Config (translateYAnimHomeConfig, Direction(..))
import Common.Types.App (LazyCheck(..))
import Common.Resources.Constants (zoomLevel)
import Components.ChooseVehicle.View as ChooseVehicle
import Components.GenericHeader.View as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination.View as SourceToDestinationView
import Components.PopUpModal as PopUpModal
import Constants (languageKey)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Function.Uncurried as DFU
import Data.Foldable (maximum, minimum)
import Data.Maybe (maybe, fromMaybe, Maybe(..), isNothing, isJust)
import Data.String as DS
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_, launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.CommonView (dummyView)
import Helpers.Utils (FetchImageFrom(..), fetchImage, decodeError, storeCallBackCustomer)
import JBridge as JB
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Locale.Utils (getLanguageLocale)
import Mobility.Prelude (boolToVisibility)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..), Shadow(..), background, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, relativeLayout, scrollView, shimmerFrameLayout, onBackPressed, alignParentBottom, singleLine, accessibilityHint,accessibility,accessibilityHint, Accessiblity(..), id, afterRender, layoutGravity, rippleColor, maxLines, ellipsize, onAnimationEnd, shadow, Gradient(..), gradient, peakHeight, enableShift)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout)
import PrestoDOM.Properties (alpha, cornerRadii, lineHeight, minWidth, sheetState, nestedScrollView, scrollBarY)
import PrestoDOM.Types.DomAttributes (Corners(..), BottomSheetState(..))
import Presto.Core.Types.Language.Flow (Flow, doAff, delay)
import Screens.TicketBookingFlow.BusTicketBooking.Controller (Action(..), ScreenOutput, eval)
import Screens.TicketBookingFlow.BusTicketBooking.ComponentConfig (genericHeaderConfig, sourceToDestinationConfig)
import Screens.HomeScreen.ScreenData (dummyRideBooking)
import Screens.Types as ST
import Services.API
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (GlobalState, defaultGlobalState)
import Helpers.CommonView
import DecodeUtil as DU
import Helpers.Pooling as HP
import Data.Time.Duration (Milliseconds(..))
import Helpers.FrfsUtils
import RemoteConfig.Utils as RCU
import Storage (getValueToLocalStore, KeyStore(..))
import Services.API as API
import Engineering.Helpers.BackTrack (liftFlowBT)
import Data.Function.Uncurried (runFn2, runFn3)
import LocalStorage.Cache
import MapUtils as MU
import Data.Int as DI
import Common.RemoteConfig.Utils as CRU

busTicketBookingScreen :: ST.BusTicketBookingState -> Screen Action ST.BusTicketBookingState ScreenOutput
busTicketBookingScreen initialState =
  { initialState
  , view
  , name: "BusTicketBookingScreen"
  , globalEvents: [(\push -> do
      void $ launchAff_ $ void $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do 
        -- Calling TicketBookingList API to show all previous tickets being booked
        getTicketBookingListEvent push
     
      let getNearbyDriversEventPollingConfig = CRU.pollingConfig "getNearbyDriversEvent"
      when (not getNearbyDriversEventPollingConfig.disable) do
        let pollingId = EHC.getCurrentUTC ""
            _ = runFn2 setInCache "POLLING_ID" pollingId
            wmbFlowConfig = CRU.fetchWmbFlowConfig FunctionCall
        void $ launchAff $ EHC.flowRunner defaultGlobalState $ getNearbyDriversEvent pollingId wmbFlowConfig getNearbyDriversEventPollingConfig initialState push wmbFlowConfig.defaultRadiusForFindingBus false 
      pure $ pure unit
  )]
  , eval:
      \action state -> do
        let _ = spy "BusTicketBookingScreen action " action
        let _ = spy "BusTicketBookingScreen state " state
        eval action state
  }
  where
    getTicketBookingListEvent push =
      if (isNothing initialState.data.ticketDetailsState) then do
        (GetMetroBookingListResp resp) <- Remote.getMetroBookingStatusListBT (show initialState.data.ticketServiceType) (Just "5") (Just "0")
        lift $ lift $ doAff do liftEffect $ push $ BusTicketBookingListRespAC resp
      else pure unit

    getNearbyDriversEvent pollingId wmbFlowConfig getNearbyDriversEventPollingConfig state push radius gotClosestBus = do
      let busLocationTrackingPollingId = runFn3 getFromCache "POLLING_ID" Nothing Just
          isMapReady = (runFn3 getFromCache "MAP_READY" Nothing Just) == Just "true"
      case (busLocationTrackingPollingId == Just pollingId), isMapReady of
        true, false -> do
          void $ delay $ Milliseconds 1000.0 -- Delay for 1 second in the case when polling is started but map is not yet ready
          getNearbyDriversEvent pollingId wmbFlowConfig getNearbyDriversEventPollingConfig state push radius gotClosestBus
        true, _ -> do
          eitherRespOrError <- Remote.postNearbyDrivers $ 
            API.NearbyDriverReq 
              { location: API.LatLong 
                  { lat: state.props.srcLat
                  , lon: state.props.srcLong
                  }
              , vehicleVariants: Just ["BUS_AC", "BUS_NON_AC"]
              , radius: radius
              }
          case eitherRespOrError of
            Right (API.NearbyDriverRes resp) -> do
              if (DA.null resp.buckets && radius < wmbFlowConfig.maxRadiusCanBeSearched) then do
                void $ delay $ Milliseconds $ DI.toNumber $ getNearbyDriversEventPollingConfig.pollingIntervalInMilliSecond
                getNearbyDriversEvent pollingId wmbFlowConfig getNearbyDriversEventPollingConfig state push (DI.toNumber $ DI.floor $ radius * wmbFlowConfig.radiusMultiplier) gotClosestBus
              else do
                if gotClosestBus
                  then do
                    doAff do liftEffect $ push $ NearbyDriverRespAC $ API.NearbyDriverRes resp
                    void $ delay $ Milliseconds $ DI.toNumber $ getNearbyDriversEventPollingConfig.pollingIntervalInMilliSecond
                    getNearbyDriversEvent pollingId wmbFlowConfig getNearbyDriversEventPollingConfig state push radius gotClosestBus
                  else do
                    doAff do liftEffect $ push $ NearbyDriverRespAC $ API.NearbyDriverRes resp
                    let closestBusBeforeMultiplier = fromMaybe wmbFlowConfig.minimumRadiusForFindingBus $ minimum $ map (\(API.NearByDriversBucket item) -> getClosestDistance item.driverInfo (API.LatLong {lat: state.props.srcLat, lon: state.props.srcLong})) resp.buckets
                        closestBusDistance = closestBusBeforeMultiplier * wmbFlowConfig.radiusMultiplier
                    doAff do liftEffect $ push $ UpdateClosestBusZoomLevel closestBusDistance
                    EHC.liftFlow $ JB.animateCamera state.props.srcLat state.props.srcLong (MU.getZoomLevel closestBusDistance) "ZOOM"
                    if (wmbFlowConfig.updatePollingRadiusToClosestBus)
                      then getNearbyDriversEvent pollingId wmbFlowConfig getNearbyDriversEventPollingConfig state push (DI.toNumber $ DI.floor $ closestBusDistance) true
                      else getNearbyDriversEvent pollingId wmbFlowConfig getNearbyDriversEventPollingConfig state push radius true
            Left err -> do
              void $ delay $ Milliseconds $ DI.toNumber $ getNearbyDriversEventPollingConfig.pollingIntervalInMilliSecond
              getNearbyDriversEvent pollingId wmbFlowConfig getNearbyDriversEventPollingConfig state push radius gotClosestBus
        _, _ -> pure unit
    
    getClosestDistance driverInfo currLoc =
      fromMaybe 1000.0 $ minimum $ map 
        (\(API.DriverInfo item) -> 
          MU.haversineDistance currLoc (API.LatLong {lat: item.lat, lon: item.lon})
        ) driverInfo


view :: forall w. (Action -> Effect Unit) -> ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
    linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push $ const GoBack
    , background Color.white900
    ] $
    [ headerView push state 
    , linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ]
      [ mapIllustrationView push state
      ]
    ]

shimmerView :: forall w . ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
shimmerView state =
  shimmerFrameLayout[ 
    width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , visibility $ boolToVisibility $ isNothing state.data.ticketDetailsState
  ][linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin (MarginTop 15)
    ] (DA.mapWithIndex 
        (\index item ->
            linearLayout
              [ width MATCH_PARENT
              , height (V 220)
              , margin (Margin 16 16 16 0)
              , cornerRadius 12.0
              , background Color.greyDark
              ][]
        ) (1 DA... 2)
      )
    ]

headerView :: forall w. (Action -> Effect Unit) -> ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
headerView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  , padding $ Padding 18 12 18 12
  ]
  [ imageView
    [ width $ V 40
    , height $ V 40
    , onClick push $ const GoBack
    , padding $ Padding 4 4 4 4
    , imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_chevron_left"
    , rippleColor Color.rippleShade
    , margin $ MarginRight 10
    ]
  , textView $
    [ text "Bus"
    , color Color.black900
    , gravity CENTER_VERTICAL
    , singleLine true
    , maxLines 1
    , textSize $ FontSize.a_18
    , padding $ PaddingBottom 4
    ] <> FontStyle.subHeading3 TypoGraphy
  , linearLayout [weight 1.0] [] 
  , linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    , onClick push $ const TicketIconClicked
    , padding $ Padding 10 6 10 6
    , gravity CENTER_VERTICAL
    , cornerRadius 12.0
    , background Color.blue600
    , rippleColor Color.rippleShade
    ]
    [ imageView
      [ width $ V 20
      , height $ V 20
      , gravity RIGHT
      , margin $ MarginRight 8
      , imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_ticket_icon_blue"
      ]
    , textView
      [ text $ getString TICKETS
      , color Color.blue800
      , gravity RIGHT
      , singleLine true
      , maxLines 1
      ]
    ]
  ]

mapIllustrationView :: forall w. (Action -> Effect Unit) -> ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
mapIllustrationView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ relativeLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER
      ]
      [ 
        mapView' push state
      , relativeLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        ]
        [ bottomSheetView push state
        ] 
      ]
    ]

bottomSheetView :: forall w. (Action -> Effect Unit) -> ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
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
            , peakHeight $ 450
            , enableShift false
            ]
              <> if state.props.expandTicketsView then  [ sheetState EXPANDED ] else []
          ) 
          [ relativeLayout 
               [ height WRAP_CONTENT
               , width MATCH_PARENT
               , orientation VERTICAL
               ][ linearLayout
                   [ height $ V 40
                   , width MATCH_PARENT
                   , gradient (Linear 0.0 [Color.white900, Color.transparent])
                   ][]
               ]
            , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , background Color.transparent
              ]
              [recenterButtonView push state]
            , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , background Color.white900
              , margin $ MarginTop 40
              ]
              [ linearLayout
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , orientation VERTICAL
                  ]
                  [
                   searchRouteButton push state
                   , textView $
                     [ text $ getString RECENT_TICKETS
                     , color Color.black800
                     , singleLine true
                     , maxLines 1
                     , padding $ Padding 16 0 16 0
                     , margin $ MarginBottom 4
                     , textSize $ FontSize.a_14
                     ] <> FontStyle.body6 TypoGraphy
                   , linearLayout
                     [ height WRAP_CONTENT
                     , width MATCH_PARENT
                     , orientation VERTICAL
                     , gravity CENTER
                     , visibility $ boolToVisibility $ (DA.null $ getAllBusTickets state) && (isJust state.data.ticketDetailsState)
                     , margin $ Margin 16 16 16 0
                     ][ imageView
                         [ height $ V 350
                         , width MATCH_PARENT
                         , imageWithFallback $ fetchImage FF_ASSET "ny_ic_bus_ticket_illustration"
                         ]
                     ]
                  ]
              , scrollView
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , nestedScrollView true
                , scrollBarY false
                ]
                [ linearLayout
                  [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , padding $ PaddingBottom 28
                , visibility $ boolToVisibility $ (not $ DA.null $ getAllBusTickets state) || (isNothing state.data.ticketDetailsState)
                ]
                [ shimmerView state
                , recentTicketsView push state
                --, recentSearchesView push state -- To be done in v2                  
                , viewMoreButton push state
                ]
              ]
            ]
          ]
       ]
    ]
searchRouteButton :: forall w. (Action -> Effect Unit) -> ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
searchRouteButton push state =
  let buttonPadding = if EHC.os == "IOS" then Padding 16 16 16 12 else Padding 16 16 16 16
  in 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ Margin 16 16 16 16
    , background Color.black900
    , padding buttonPadding
    , onClick push $ const SearchButtonClick
    , cornerRadius 12.0
    , rippleColor Color.rippleShade
    , gravity CENTER_VERTICAL
    , accessibilityHint "Enter Bus No/Destination : Button"
    ]
    [ imageView
      [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_search_yellow"
      , height $ V 16
      , width $ V 16
      , gravity CENTER_VERTICAL
      , margin $ MarginRight 12
      ]
    , textView $ 
      [ text "Enter Bus No./Destination"
      , color Color.yellow900
      , singleLine false
      , gravity CENTER_VERTICAL
      , textSize $ FontSize.a_18
      ] <> FontStyle.subHeading3 TypoGraphy
    ]

recentTicketsView :: forall w. (Action -> Effect Unit) -> ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
recentTicketsView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ Margin 16 0 16 16
  , orientation VERTICAL
  , visibility $ boolToVisibility $ isJust state.data.ticketDetailsState
  ]
  [ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ] $
    map (\ticketData -> ticketCardView push ticketData) $ showAllOrTrimmed $ getAllBusTickets state
  ]
  where 
    showAllOrTrimmed :: Array ST.MetroTicketCardData -> Array ST.MetroTicketCardData
    showAllOrTrimmed ticketData = if state.props.showAllTickets then 
                                    if DA.length ticketData > 4 then DA.take 5 ticketData 
                                    else ticketData
                                  else DA.take 3 ticketData

ticketCardView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketCardData -> PrestoDOM (Effect Unit) w
ticketCardView push ticketData =
  let (TicketStatus ticketStatus) = getTicketStatus ticketData
  in
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , stroke $ "1," <> Color.grey900
  , padding $ Padding 16 16 16 0
  , margin $ MarginTop 12
  , orientation VERTICAL
  , background Color.white900
  , onClick push $ const $ TicketPressed ticketData.metroTicketStatusApiResp
  , rippleColor Color.rippleShade
  , cornerRadius 24.0
  ]
  [ linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    ]
    [ imageView
      [ width $ V 42
      , height $ V 42
      , margin $ MarginRight 12
      , imageWithFallback $ fetchImage GLOBAL_COMMON_ASSET "ny_ic_bus_icon_light_blue"
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation VERTICAL
      ]
      [ textView $
        [ text $ getString BUS_TICKET
        , color Color.black800
        , singleLine true
        , maxLines 1
        , margin $ MarginBottom 2
        ] <> FontStyle.body1 TypoGraphy
      , textView $
        [ text $ getString TICKET_NUMBER <> ": " <> extractTicketNumber 
        , color Color.black700
        , singleLine true
        , maxLines 1
        , margin $ MarginBottom 2
        ] <> FontStyle.body3 TypoGraphy
      ]
    , linearLayout [weight 1.0] []
    , textView $
      [ text $ (show ticketStatus.status)
      , color $ ticketStatus.textColor
      , background $ ticketStatus.statusColor
      , padding $ Padding 12 6 12 6
      , cornerRadius 20.0
      , layoutGravity "right"
      ] <> FontStyle.tags TypoGraphy
    ]
  , linearLayout [ margin $ MarginVertical 18 18 ] [ horizontalDottedSeparatorView ]
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER_VERTICAL
    , margin $ MarginBottom 18
    ]
    [ singleStopView push ticketData true
    , textView $
      [ height $ V 18
      , width $ V 2
      , background Color.grey900
      , margin $ MarginLeft 2 
      ]
    , singleStopView push ticketData false
    ]
  , linearLayout 
    [ visibility $ boolToVisibility $ DA.any (_ == ticketStatus.status) [FRFS_EXPIRED, FRFS_FAILED]
    ] 
    [ horizontalDottedSeparatorView ]
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER
    , visibility $ boolToVisibility $ DA.any (_ == ticketStatus.status) [FRFS_EXPIRED, FRFS_FAILED]
    , padding $ Padding 16 12 16 16
    , onClick push $ const $ RepeatRideClicked ticketData.metroTicketStatusApiResp
    , rippleColor Color.rippleShade
    ]
    [ imageView
      [ width $ V 16
      , height $ V 16
      , imageWithFallback $ fetchImage GLOBAL_COMMON_ASSET "ny_ic_repeat_icon_blue"
      , gravity CENTER_VERTICAL
      ]
    , textView $
      [ text $ getString REPEAT_RIDE
      , color Color.blue900
      , singleLine true
      , maxLines 1
      , padding $ Padding 3 3 3 3
      ] <> FontStyle.body1 TypoGraphy
    ]
  ]
  where
    extractTicketNumber :: String
    extractTicketNumber = do
      let (FRFSTicketBookingStatusAPIRes ticketBookingStatusResp) = ticketData.metroTicketStatusApiResp
          ticketAPIData = map (\(FRFSTicketAPI tickets) -> tickets.ticketNumber) ticketBookingStatusResp.tickets
      fromMaybe (defaultTicketNumber ticketBookingStatusResp.bookingId) $ DA.head ticketAPIData
    
    defaultTicketNumber :: String -> String 
    defaultTicketNumber = DS.take 8

singleStopView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketCardData -> Boolean -> PrestoDOM (Effect Unit) w
singleStopView push ticketData isSourceView =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  ]
  [ imageView
    [ width $ V 8
    , height $ V 8
    , imageWithFallback $ fetchImage FF_COMMON_ASSET $ if isSourceView then "ny_ic_green_circle" else "ny_ic_red_circle"
    , margin $ MarginTop $ if isSourceView then 0 else 2
    ]
  , textView $
    [ text $ if isSourceView then ticketData.sourceName else ticketData.destinationName
    , margin $ MarginHorizontal 16 15
    , color Color.black650
    , ellipsize true
    , maxLines 1
    , gravity CENTER_VERTICAL
    ] <> FontStyle.body1 TypoGraphy
  ]

viewMoreButton :: forall w. (Action -> Effect Unit) -> ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
viewMoreButton push state = 
  linearLayout  
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , padding $ Padding 8 0 8 8
  , layoutGravity "center_horizontal"
  , onClick push $ const ViewMoreClicked
  , rippleColor Color.rippleShade
  , visibility $ boolToVisibility $ (not $ state.props.showAllTickets) && (DA.length $ getAllBusTickets state) > 3
  ]
  [ textView $
    [ text $ getString VIEW_MORE
    , color Color.blue900
    ] <> FontStyle.body1 TypoGraphy
  ]

getAllBusTickets :: ST.BusTicketBookingState -> Array ST.MetroTicketCardData
getAllBusTickets state =
  maybe [] (\ticketDetailsState -> ticketDetailsState.data.activeTickets <> ticketDetailsState.data.pastTickets) state.data.ticketDetailsState


mapView' :: forall w. (Action -> Effect Unit) -> ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
mapView' push state =
    Keyed.relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    ]
    [ Tuple "MapContainer" $ PrestoAnim.animationSet
        [ Anim.fadeIn true
        ]
        $ linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , id $ EHC.getNewIDWithTag "BusTicketBookingScreenMap"
            , padding $ PaddingBottom 400
            , onAnimationEnd
                ( \action -> do
                    let wmbFlowConfig = CRU.fetchWmbFlowConfig FunctionCall
                        _ = runFn2 setInCache "MAP_READY" "true"
                    void $ JB.showMap (EHC.getNewIDWithTag "BusTicketBookingScreenMap") true "satellite" wmbFlowConfig.defaultZoomLevelOnMap state.props.srcLat state.props.srcLong push MapReady
                    push action
                )
                (const NoAction)
            ]
            []
    , Tuple "BottomGradient" $ relativeLayout 
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , alignParentBottom "true,-1"
        , gravity RIGHT
        , orientation VERTICAL
        ][ linearLayout
          [ height $ V 60
          , width MATCH_PARENT
          , alignParentBottom "true,-1"
          , gradient (Linear 0.0 [Color.white900, Color.transparent])
          ][]
       ]
    , Tuple "TopGradient" $ relativeLayout 
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][ linearLayout
          [ height $ V 30
          , width MATCH_PARENT
          , gradient (Linear 180.0 [Color.white900, Color.transparent])
          ][]
      ]
    ]


recenterButtonView :: forall w. (Action -> Effect Unit) -> ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
recenterButtonView push state =
  PrestoAnim.animationSet [ Anim.translateYAnimFromTop $ translateYAnimHomeConfig BOTTOM_TOP ]
    $ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background Color.transparent
        , gravity RIGHT
        , alignParentBottom "true,-1"
        , padding $ Padding 0 0 16 14
        , accessibility DISABLE
        ]
        [ imageView
            [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_recenter_btn"
            , accessibility DISABLE
            , onClick
                ( \action -> do
                    _ <- push action
                    _ <- JB.getCurrentPosition push UpdateCurrentLocation
                    void $ JB.animateCamera state.props.srcLat state.props.srcLong (MU.getZoomLevel state.data.closestBusDistance) "ZOOM"
                    -- _ <- pure $ logEvent state.data.logField "ny_user_recenter_btn_click"
                    pure unit
                )
               (const $ RecenterCurrentLocation)
            , height $ V 40
            , width $ V 40
            ]
        ]