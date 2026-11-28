module Screens.SelectBusRoute.View where

import Animation as Anim 
import Animation.Config (translateYAnimConfig, translateYAnimMapConfig, removeYAnimFromTopConfig)
import JBridge as JB 
import Prelude (not, Unit, bind, const, pure, unit, discard, void, ($), (&&), (/=), (<<<),(<>), (==), map, show, (||), show, (-), (*), (/), (>), when, (+))
import PrestoDOM
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout)
import PrestoDOM.Properties (cornerRadii, sheetState)
import PrestoDOM.Types.DomAttributes (Corners(..), BottomSheetState(..))
import Screens.SelectBusRoute.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Screens.Types (LocationActionId(..))
import Styles.Colors as Color
import Common.Types.App
import Effect (Effect)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Font.Style as FontStyle
import Font.Size as FontSize
import Engineering.Helpers.Commons (screenWidth, convertUTCtoISC, getNewIDWithTag)
import Helpers.Utils (convertUTCToISTAnd12HourFormat, fetchImage, FetchImageFrom(..), decodeError, fetchAndUpdateCurrentLocation, getAssetsBaseUrl, getCurrentLocationMarker, getLocationName, getNewTrackingId, getSearchType, parseFloat, storeCallBackCustomer)
import Services.API (FRFSRouteAPI(..), FrfsSearchResp(..), FrfsQuotesRes(..), FrfsQuote(..), FRFSVehicleServiceTierAPI(..), BusTrackingRouteReq(..), BusTrackingRouteResp(..)) --(BookingStatus(..), TicketPlaceResp(..), PlaceType(..))
import Animation (fadeInWithDelay, translateInXBackwardAnim, translateInXBackwardFadeAnimWithDelay, translateInXForwardAnim, translateInXForwardFadeAnimWithDelay)
import Halogen.VDom.DOM.Prop (Prop)
import Data.Array as DA
import Data.Maybe (fromMaybe, Maybe(..), maybe, isNothing, isJust, fromJust)
import Debug
import Data.List ((:))
import Effect.Uncurried  (runEffectFn1)
import PaymentPage (consumeBP)
import Engineering.Helpers.Commons as EHC
import Data.Int as DI 
import Data.String as DS
import Screens.SelectBusRoute.ScreenData as SD
import Screens.SelectBusRoute.ComponentConfig (headerConfig, seeRouteButtonConfig)
import Services.Backend as Remote
import Language.Strings (getString)
import Language.Types (STR(..))
import Helpers.API as HelpersAPI
import Presto.Core.Types.Language.Flow (Flow, doAff)
import Types.App (GlobalState(..), defaultGlobalState)
import Data.Either (Either(..))
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons (flowRunner)
import Effect.Class (liftEffect)
import Mobility.Prelude (boolToVisibility)
import Data.Time.Duration (Milliseconds(..))
import Helpers.Pooling (delay)
import Helpers.Utils as HU
import Data.Function.Uncurried (runFn1)
import Helpers.FrfsUtils (getFirstRoute)
import Engineering.Helpers.LogEvent (logEvent)
import Data.Foldable (traverse_)

screen :: String -> String -> SD.SelectBusRouteScreenState -> Screen Action SD.SelectBusRouteScreenState ScreenOutput
screen fromStationCode toStationCode initialState =
  { initialState
  , view : view
  , name : "SelectBusRoute"
  , globalEvents : [(\push -> do
                      _ <- pure $ spy "debug route" (fromStationCode <> " " <> toStationCode)
                      when (isNothing initialState.data.quotes) $
                        void $ launchAff $ flowRunner defaultGlobalState $ getSearchId push UpdateQuotes fromStationCode toStationCode
                      pure $ pure unit)
                    ]
  , eval :
    \action state -> do
        let _ = spy "SelectBusRoute action " action
        let _ = spy "SelectBusRoute state " state
        eval action state
  }

view :: forall w . (Action -> Effect Unit) -> SD.SelectBusRouteScreenState -> PrestoDOM (Effect Unit) w
view push state =
  PrestoAnim.animationSet [Anim.fadeIn true]  $ frameLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
  , onBackPressed push $ const BackPressed
  ][frameLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , gravity BOTTOM
    , alignParentBottom "true,-1"
    ]
     [linearLayout 
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ]
      [ headerView state push
      , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.greySmoke
        ][]
      , pickupAndDestView state push
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , margin $ Margin 16 32 16 0
        , orientation HORIZONTAL
        ][textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ getString ROUTE_BUS_NO
          , color Color.black800
          , margin $ MarginBottom 8
          ] <> FontStyle.body3 TypoGraphy
          , linearLayout [weight 1.0] []
          , sortByPillView push state
        ]
      , frameLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , gravity BOTTOM
        , alignParentBottom "true,-1"
        ][
           scrollView
            [ height $ WRAP_CONTENT
            , width MATCH_PARENT
            , margin $ Margin 16 0 16 100
            ][  linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ][case state.data.quotes of
                    Just quotes -> routeListView quotes state push
                    Nothing -> routeListShimmerView
                ]
            ]
        ]
      ]
     , sortByPopUpView push state
     ]
  ]

pickupAndDestView :: forall w. SD.SelectBusRouteScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
pickupAndDestView state push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ Margin 16 24 16 0
  ][  textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ getString PICKUP_AND_DESTINATION_STOP
      , color Color.black800
      ] <> FontStyle.body3 TypoGraphy
    , locationSelectionView push state
  ]

routeListView :: forall w. (Array FrfsQuote) -> SD.SelectBusRouteScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
routeListView quotes state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ](
      DA.mapWithIndex (\index quote ->
        routeRadioComponent state push quote (DA.length quotes)
      ) quotes
    )

routeListShimmerView :: forall w. PrestoDOM (Effect Unit) w
routeListShimmerView =
  shimmerFrameLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 8
    , background Color.white900
    ](
      DA.mapWithIndex (\index quote ->
        linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , stroke ("1," <> Color.grey900)
        , padding $ Padding 20 25 20 25
        , cornerRadius 8.0
        , gravity CENTER_VERTICAL
        , background Color.borderGreyColor
        , margin $ MarginTop $ index * (50 + 12)
        ][]
      ) [1,2,3,4]
    )

routeRadioComponent :: forall w. SD.SelectBusRouteScreenState -> (Action -> Effect Unit) -> FrfsQuote -> Int -> PrestoDOM (Effect Unit) w
routeRadioComponent state push (FrfsQuote quote) totalRoutes =
  let route = getFirstRoute (FrfsQuote quote)
      isSelected =
        case state.data.selectedQuote of
          Just (FrfsQuote selectedQuote) -> selectedQuote.quoteId == quote.quoteId
          Nothing -> false
  in
  case route of
    Nothing -> dummyView state
    Just (FRFSRouteAPI route') -> 
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , stroke ("1," <> Color.grey900)
      , padding $ Padding 20 16 20 16
      , cornerRadius 8.0
      , gravity CENTER_VERTICAL
      , margin $ MarginBottom 12
      , rippleColor Color.rippleShade
      , if state.data.isSortByPillClicked then onClick push $ const $ NoAction else onClick push $ const $ SelectQuote (FrfsQuote quote)
      ][linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity LEFT
        ][textView $
          [ text route'.shortName
          , color Color.black800
          , padding $ PaddingLeft 8
          ] <> FontStyle.body25 LanguageStyle
          , vehicleACServiceTierPillView state route'.vehicleServiceTier
          , cheapestRoutePillView state totalRoutes route'.shortName
          , fastestRoutePillView state totalRoutes route'.shortName
        ]
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity RIGHT
        ][textView $
           [ textFromHtml $ getETAforRoute state route'.shortName
           , color Color.black800
           , padding $ PaddingRight 4
           , visibility $ boolToVisibility $ DA.length state.data.eta == totalRoutes
           ] <> FontStyle.body4 LanguageStyle 
          , imageView
            [ height $ V 21
            , width $ V 21
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right"
            ]
          ]
       ]

locationSelectionView :: (Action -> Effect Unit) -> SD.SelectBusRouteScreenState ->  forall w. PrestoDOM (Effect Unit) w
locationSelectionView push state =
  relativeLayout 
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ]
  [
    linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , cornerRadius 8.0
  , stroke ("1," <> Color.borderColorLight)
  , orientation VERTICAL
  , margin $  MarginTop 5
  , gravity CENTER
  ]
  [srcTextView push state
  , linearLayout[
      height $ V 1
    , width MATCH_PARENT
    , margin $ MarginHorizontal 20 40
    , background Color.borderColorLight
    ][ ]
  , destTextView push state
]
  , imageView $ 
      [ height $ V 32
      , width $ V 32
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_src_dest_edit"
      , cornerRadius 10.0 
      , margin $ Margin 0 46 16 0
      , alignParentRight "true,-1"
      , background Color.white900
      , if state.data.isSortByPillClicked then onClick push (const NoAction) else onClick push $ const $ EditStops
      ]
]

srcTextView :: forall w. (Action -> Effect Unit) -> SD.SelectBusRouteScreenState -> PrestoDOM (Effect Unit) w
srcTextView push state = textViewForLocation (getString FROM) Src push state

destTextView :: forall w. (Action -> Effect Unit) -> SD.SelectBusRouteScreenState -> PrestoDOM (Effect Unit) w
destTextView push state = textViewForLocation (getString TO) Dest push state

textViewForLocation :: forall w. String -> LocationActionId -> (Action -> Effect Unit) -> SD.SelectBusRouteScreenState -> PrestoDOM (Effect Unit) w
textViewForLocation label actionId push state =
  let 
    fieldConfig = case actionId of
      Src -> do
        let fieldValue = if (DS.null state.data.srcLoc) then (getString STARTING_FROM) <> "?" else state.data.srcLoc
            alpha = if (DS.null state.data.srcLoc) then 0.5 else 1.0
        {fieldText : fieldValue, alphaValue : alpha}
      Dest -> do
        let fieldValue = if (DS.null state.data.destLoc) then (getString WHERE_TO) else state.data.destLoc
            alpha = if (DS.null state.data.destLoc) then 0.5 else 1.0
        {fieldText : fieldValue, alphaValue : alpha}
  in
    linearLayout 
    [ height $ V 54
    , width MATCH_PARENT
    , background Color.white900
    , gravity CENTER_VERTICAL
    , rippleColor Color.rippleShade
    , cornerRadius 8.0
    ]
    [ imageView $ 
            [ height $ V 18
            , width $ V 18
            , imageWithFallback $ fetchImage COMMON_ASSET (if actionId == Src then "ny_ic_pickup_green_indicator" else "ny_ic_drop_red_indicator")
            , cornerRadius 4.0 
            , margin $ MarginLeft 8
            ]
    , textView $ 
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , text $ fieldConfig.fieldText
        , color Color.black800
        , gravity CENTER_VERTICAL
        , singleLine true
        , ellipsize true
        , margin $ MarginHorizontal 10 10
        , alpha fieldConfig.alphaValue
        ] <> (FontStyle.getFontStyle FontStyle.SubHeading1 LanguageStyle)
        
    ]

headerView :: forall w. SD.SelectBusRouteScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , padding (PaddingTop EHC.safeMarginTop)
    , background Color.white900
    ][  GenericHeader.view (if state.data.isSortByPillClicked then \_ -> push NoAction else push <<< GenericHeaderAC) (headerConfig state) ]
      
getSearchId :: (Action -> Effect Unit) -> ((Array FrfsQuote) -> Action) -> String -> String -> Flow GlobalState Unit
getSearchId push action srcCode destCode = do
  resp <- Remote.frfsSearch "BUS" (Remote.makeSearchMetroReq srcCode destCode 1 Nothing)
  case resp of
    Right (FrfsSearchResp response) -> do
      _ <- pure $ spy "debug route searchId" response.searchId
      void $ delay $ Milliseconds 1000.0
      getQuotes push action response.searchId
    Left err -> do
      pure unit
      
getQuotes :: (Action -> Effect Unit) -> ((Array FrfsQuote) -> Action) -> String -> Flow GlobalState Unit
getQuotes push action searchId = do
  resp <- Remote.frfsQuotes searchId
  case resp of
    Right (FrfsQuotesRes response) -> do
      if DA.length response == 0 
        then do
          void $ delay $ Milliseconds 2000.0
          getQuotes push action searchId
        else do
          let routes =
                DA.catMaybes $  
                  map (\quote ->
                    getFirstRoute quote
                  ) response
              routeCodeList = map (\(FRFSRouteAPI route) -> route.shortName) routes
          _ <- getVehicleTrackingInfo push routeCodeList
          doAff do liftEffect $ push $ action response
    Left err -> do
      pure unit

dummyView :: forall w. SD.SelectBusRouteScreenState -> PrestoDOM ( Effect Unit) w
dummyView state = 
  linearLayout
  [height $ V 0
  , width $ V 0
  ][]

getSeeRouteButton :: String -> Int 
getSeeRouteButton id = 
  HU.getDefaultPixelSize $ (runFn1 JB.getLayoutBounds $ getNewIDWithTag id).height - 82


sortByPillView :: forall w.(Action -> Effect Unit) -> SD.SelectBusRouteScreenState -> PrestoDOM (Effect Unit) w
sortByPillView push state = 
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , cornerRadius 20.0
  , background Color.white900
  , stroke ("1," <> Color.grey900)
  , padding $ Padding 10 4 10 4
  , margin $ MarginBottom 4
  , onClick push $ const $ SortByPressed
  ][textView $
    [ text state.data.sortbyPillText
    , color Color.black800
    , padding $ Padding 0 0 6 2
    ] <> FontStyle.body3 TypoGraphy
    , imageView
      [ height $ V 14
      , width $ V 10
      , padding $ PaddingTop 4
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_down"
      ]
  ]

vehicleACServiceTierPillView :: forall w. SD.SelectBusRouteScreenState -> Maybe FRFSVehicleServiceTierAPI -> PrestoDOM (Effect Unit) w
vehicleACServiceTierPillView state mbServiceTier = 
  case mbServiceTier of
    Nothing -> dummyView state
    Just (FRFSVehicleServiceTierAPI serviceTier) ->
      linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , cornerRadius 20.0
      , background Color.blue600
      , gravity CENTER
      , margin $ Margin 8 4 0 0
      , padding $ Padding 2 0 3 0
      , visibility $ boolToVisibility $ serviceTier._type == "AC"
      ][imageView
        [ height $ V 15
        , width $ V 15
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_ac"
        , padding $ PaddingRight 2
        ]
        , textView $
        [ text "AC"
        , color Color.blue800
        , padding $ PaddingBottom 2
        ] <> FontStyle.body24 LanguageStyle
      ]

cheapestRoutePillView :: forall w. SD.SelectBusRouteScreenState -> Int -> String -> PrestoDOM (Effect Unit) w
cheapestRoutePillView state totalRoutes routeCode = 
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , cornerRadius 20.0
    , background Color.green300
    , gravity CENTER
    , margin $ Margin 8 4 0 0
    , padding $ Padding 3 0 3 2
    , visibility $ boolToVisibility $ totalRoutes > 1 && state.data.cheapestRoute == Just routeCode
    ][ textView $
      [ text $ getString CHEAPEST
      , color Color.green900
      ] <> FontStyle.body24 LanguageStyle
    ]   

fastestRoutePillView :: forall w. SD.SelectBusRouteScreenState -> Int -> String -> PrestoDOM (Effect Unit) w
fastestRoutePillView state totalRoutes routeCode = 
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , cornerRadius 20.0
    , background Color.yellow800
    , gravity CENTER
    , margin $ Margin 8 4 0 0
    , padding $ Padding 3 0 3 2
    , visibility $ boolToVisibility $ totalRoutes > 1 && state.data.fastestRoute == Just routeCode
    ][ textView $
      [ text $ getString FASTEST
      , color Color.yellow900
      ] <> FontStyle.body24 LanguageStyle
    ]

sortByPopUpView :: forall w. (Action -> Effect Unit) -> SD.SelectBusRouteScreenState -> PrestoDOM (Effect Unit) w
sortByPopUpView push state =   
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , alignParentBottom "true,-1"
  , orientation VERTICAL
  , gravity BOTTOM
  , background Color.black9000
  , visibility $ boolToVisibility state.data.isSortByPillClicked
  ]
  [ popUpView push state ]  

popUpView :: forall w. (Action -> Effect Unit) -> SD.SelectBusRouteScreenState -> PrestoDOM (Effect Unit) w
popUpView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , gravity BOTTOM
  , cornerRadius 24.0
  , background Color.white900
  ]
  [ linearLayout
    [
      width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , padding $ Padding 20 24 20 16
    ]
    [textView
      ([ width MATCH_PARENT
      , height WRAP_CONTENT
      , background Color.white900
      , gravity LEFT
      , text $ getString SORT_BY
      , textSize $ FontSize.a_16
      , color Color.black800
      , margin $ MarginBottom 12
      ] <> FontStyle.body4 TypoGraphy
      )
     , linearLayout
      [ width MATCH_PARENT
       , height WRAP_CONTENT
       , orientation VERTICAL
      ] 
      (DA.mapWithIndex (\index item -> popUpOptionsView push state item) [AC_BUS, EARLY_DEPARTURE, TICKET_PRICE])
     ]
     , popUpButtonView push state
  ]

popUpOptionsView :: forall w. (Action -> Effect Unit) -> SD.SelectBusRouteScreenState -> STR -> PrestoDOM (Effect Unit) w
popUpOptionsView push state option = 
  linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , stroke ("1," <> Color.grey900)
     , padding $ Padding 20 16 20 16
     , cornerRadius 12.0
     , gravity CENTER_VERTICAL
     , margin $ MarginBottom 12
     , background Color.white900
     , rippleColor Color.rippleShade
     , onClick push $ const $ SortByOptionPressed option
     ]
     [
      textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getString option
        , color Color.black800
        , gravity LEFT
        , padding $ PaddingBottom 3
        ] <> FontStyle.body25 LanguageStyle 
     ]

popUpButtonView :: forall w. (Action -> Effect Unit) -> SD.SelectBusRouteScreenState -> PrestoDOM (Effect Unit) w
popUpButtonView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity BOTTOM
  , alignParentBottom "true,-1"
  , background Color.transparent
  , orientation VERTICAL
  ]
  [ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ][
      linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.white900
      , padding $ PaddingVertical 5 24
      , stroke ("1," <> Color.borderColorLight)
      ][PrimaryButton.view (\_ -> push ClosePopupButtonAction) (seeRouteButtonConfig state)]
    ]
  ]


getVehicleTrackingInfo :: (Action -> Effect Unit) -> Array String  -> Flow GlobalState Unit
getVehicleTrackingInfo push routeCodeList =
  traverse_ (\routeCode -> do
    resp <- Remote.trackRouteVehicles $ BusTrackingRouteReq routeCode
    case resp of
      Right (BusTrackingRouteResp response) -> do
        void $ pure $ spy "debug route tracking info" response
        doAff $ liftEffect $ push $ UpdateEtaForRoutes routeCode response.vehicleTrackingInfo
      Left _ -> pure unit
  ) routeCodeList

getETAforRoute :: SD.SelectBusRouteScreenState -> String -> String
getETAforRoute state routeCode =
  let etaTimeInSeconds = DA.find (\eta -> eta.routeCode == routeCode) state.data.eta
      isJustEta = case etaTimeInSeconds of
        Just eta -> fromMaybe 0 eta.etas
        Nothing -> 0
  in 
    if isJustEta /= 0 then do 
      let secondsTohhmm = HU.secondsToHms isJustEta 
      if (secondsTohhmm == "--") then "In less than 1 min" else  "In " <> secondsTohhmm  
    else getString NO_INCOMING_BUS
      
