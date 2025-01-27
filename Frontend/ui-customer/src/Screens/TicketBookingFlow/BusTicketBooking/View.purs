module Screens.TicketBookingFlow.BusTicketBooking.View 
  ( busTicketBookingScreen
  , view
  )
  where

import Prelude

import Animation as Anim
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
import Data.Maybe (maybe, fromMaybe, Maybe(..), isNothing, isJust)
import Data.String as DS
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
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
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..), Shadow(..), background, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, relativeLayout, scrollView, shimmerFrameLayout, onBackPressed, alignParentBottom, singleLine, accessibilityHint,accessibility,accessibilityHint, Accessiblity(..), id, afterRender, layoutGravity, rippleColor, maxLines, ellipsize, onAnimationEnd, shadow)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.Properties (alpha, cornerRadii, lineHeight, minWidth)
import PrestoDOM.Types.DomAttributes (Corners(..))
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

busTicketBookingScreen :: ST.BusTicketBookingState -> Screen Action ST.BusTicketBookingState ScreenOutput
busTicketBookingScreen initialState =
  { initialState
  , view
  , name: "BusTicketBookingScreen"
  , globalEvents: [getTicketBookingListEvent]
  , eval:
      \action state -> do
        let _ = spy "BusTicketBookingScreen action " action
        let _ = spy "BusTicketBookingScreen state " state
        eval action state
  }
  where
    getTicketBookingListEvent push =
      if (isNothing initialState.data.ticketDetailsState) then do
        void $ launchAff_ $ void $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do 
          resp <- lift $ lift $ Remote.getMetroBookingStatusList (show initialState.data.ticketServiceType) (Just "5") (Just "0")
          let bookingList = 
                case resp of
                  Right (GetMetroBookingListResp res) -> res
                  Left _ -> []
          lift $ lift $ doAff do liftEffect $ push $ BusTicketBookingListRespAC bookingList
        pure $ pure unit
      else pure $ pure unit

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
    , dummyIllustrationView push state
    , scrollView 
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , visibility $ boolToVisibility $ (not $ DA.null $ getAllBusTickets state) || (isNothing state.data.ticketDetailsState)
      ]
      [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding $ PaddingBottom 28
        ]
        [ shimmerView state
        , recentTicketsView push state
        -- , recentSearchesView push state -- To be done in v2
        , viewMoreButton push state
        ]
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
  relativeLayout
  [ height $ V $ 220 + EHC.safeMarginTop
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.black900
  , gravity CENTER
  , cornerRadii $ Corners 24.0 false false true true
  , padding $ PaddingTop EHC.safeMarginTop
  ]
  [ linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity RIGHT
    , orientation HORIZONTAL
    ]
    [ imageView
      [ width $ V $ 220
      , height MATCH_PARENT
      , gravity RIGHT
      , alignParentBottom "true,-1"
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_ticket_clip_background"
      ]
    ]
  , linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , padding $ Padding 12 12 12 12
      , gravity CENTER_VERTICAL
      ]
      [ imageView
        [ width $ V 32
        , height $ V 32
        , padding $ Padding 4 4 4 4
        , onClick push $ const GoBack
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left_white"
        , rippleColor Color.rippleShade
        ]
      , linearLayout
        [ weight 1.0
        ] []
      , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation HORIZONTAL
        , onClick push $ const TicketIconClicked
        , gravity CENTER_VERTICAL
        ]
        [ imageView
          [ width $ V 32
          , height $ V 32
          , gravity RIGHT
          , padding $ Padding 4 4 4 4
          , imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_ticket_icon_yellow"
          , rippleColor Color.rippleShade
          ]
        , textView $
          [ text $ getString TICKETS
          , color Color.yellow900
          , gravity RIGHT
          , singleLine true
          , maxLines 1
          , rippleColor Color.rippleShade
          ]
        ]
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , margin $ Margin 16 16 0 0
      ]
      [ imageView
        [ width $ V 42
        , height $ V 42
        , imageWithFallback $ fetchImage GLOBAL_COMMON_ASSET "ny_ic_bus_icon_light_blue"
        , margin $ MarginRight 16
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ textView $
          [ text $ getString BOOK_BUS_TICKET
          , color Color.white900
          , singleLine true
          , maxLines 1
          ] <> FontStyle.body25 TypoGraphy
        , textView $
          [ text $ getString BOOK_A_ONE_WAY_INSTANT_BUS_TICKET
          , color Color.black500
          , singleLine true
          , maxLines 1
          , margin $ MarginTop 2
          ] <> FontStyle.tags TypoGraphy
        ]
      ]
    , searchRouteButton push state
    ]
  ]

dummyIllustrationView :: forall w. (Action -> Effect Unit) -> ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
dummyIllustrationView push state =
  let busConfig = RCU.getBusFlowConfigs (getValueToLocalStore CUSTOMER_LOCATION)
  in
  linearLayout
  [ height WRAP_CONTENT
  , weight 1.0
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER_HORIZONTAL
  , margin $ Margin 24 48 24 0
  , visibility $ boolToVisibility $ (DA.null $ getAllBusTickets state) && (isJust state.data.ticketDetailsState)
  ]
  [ imageView $
    [ width $ V $ 280
    , height $ V 280
    , gravity CENTER_HORIZONTAL
    , layoutGravity "center_horizontal"
    , imageWithFallback $ fetchImage FF_ASSET "ny_ic_bus_ticket_illustration"
    ]
  , textView $
    [ text $ getString $ EXPERIENCE_OUR_PILOT_LAUNCH_FOR_BUS_TICKETING_IN_PRIME_ROUTES (show busConfig.liveRoutes)
    , color Color.black800
    , gravity CENTER_HORIZONTAL
    , visibility $ boolToVisibility $ busConfig.liveRoutes > 0
    , width MATCH_PARENT
    , height WRAP_CONTENT
    ] <> FontStyle.subHeading3 TypoGraphy
  , textView $
    [ text $ getString $ NOTE_YOUR_TICKET_IS_ONLY_VALID_FOR busConfig.ticketValidity
    , color Color.black800
    , margin $ MarginTop 12
    , gravity CENTER_HORIZONTAL
    , width MATCH_PARENT
    , height WRAP_CONTENT
    , visibility $ boolToVisibility $ not $ DS.null busConfig.ticketValidity
    ] <> FontStyle.body3 TypoGraphy
  ]
  where
    appName = fromMaybe "Namma Yatri" $ DFU.runFn3 DU.getAnyFromWindow "appName" Nothing Just

searchRouteButton :: forall w. (Action -> Effect Unit) -> ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
searchRouteButton push state =
  let buttonPadding = if EHC.os == "IOS" then Padding 16 16 16 12 else Padding 16 16 16 16
  in 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ Margin 16 24 16 20
    , background Color.white900
    , padding buttonPadding
    , onClick push $ const SearchButtonClick
    , cornerRadius 12.0
    , rippleColor Color.rippleShade
    , gravity CENTER_VERTICAL
    , accessibilityHint "Enter Bus No/Destination : Button"
    ]
    [ imageView
      [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_search_grey"
      , height $ V 16
      , width $ V 16
      , gravity CENTER_VERTICAL
      , margin $ MarginRight 12
      ]
    , textView $ 
      [ text "Enter Bus No/Destination"
      , color Color.black900
      , singleLine false
      , gravity CENTER_VERTICAL
      ] <> FontStyle.subHeading3 TypoGraphy
    ]

recentTicketsView :: forall w. (Action -> Effect Unit) -> ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
recentTicketsView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ Margin 16 24 16 16
  , orientation VERTICAL
  , visibility $ boolToVisibility $ isJust state.data.ticketDetailsState
  ]
  [ textView $
    [ text $ getString RECENT_TICKETS
    , color Color.black800
    , singleLine true
    , maxLines 1
    ] <> FontStyle.body6 TypoGraphy
  , linearLayout
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
  -- , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5 -- Shadow is not having cornerRadius
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