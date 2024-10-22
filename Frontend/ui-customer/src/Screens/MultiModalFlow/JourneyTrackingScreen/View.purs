{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.MultiModalFlow.JourneyTrackingScreen.View where

import Debug
import Prelude
import PrestoDOM
import Screens.MultiModalFlow.JourneyTrackingScreen.Controller
import Screens.MultiModalFlow.JourneyTrackingScreen.ScreenData

import Accessor (_lat, _lon)
import Animation (screenAnimation)
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
import Data.Lens ((^.))
import Data.Maybe as Mb
import Effect (Effect)
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToInvisibility, boolToVisibility)
import Presto.Core.Types.Language.Flow (Flow)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout)
import PrestoDOM.List (ListItem, preComputeListItem)
import PrestoDOM.Properties (sheetState, cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.MultiModalFlow.Components.AlertWidget as AlertWidget
import Screens.MultiModalFlow.Components.MetroCard as MetroCard
import Screens.MultiModalFlow.Components.VehicleCard as VehicleCard
import Screens.MultiModalFlow.JourneyTrackingScreen.ComponentConfig (getAlertWidgetConfig)
import Screens.RideBookingFlow.HomeScreen.Config as HSConfig
import Screens.Types as ST
import Services.API as API
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (GlobalState(..), defaultGlobalState)
import Components.SeparatorView.View as SeparatorView
import Types.EndPoint (shareRide)
-- module Screens.MultiModalFlow.JourneyTrackingScreen.View
--   ( bottomSheetContentView
--   , bottomSheetView
--   , journeyLegDescriptionView
--   , journeyLegTitleView
--   , journeyLegView
--   , knobView
--   , screen
--   , verticalLineView
--   , view
--   )
--   where

-- import Animation (screenAnimation)
-- import Common.Types.App as CTA
-- import Data.Maybe as Mb
-- import Data.Array as DA
-- import Effect (Effect)
-- import Engineering.Helpers.Commons as EHC
-- import Helpers.Utils as HU
-- import Types.App (GlobalState(..), defaultGlobalState)
-- import JBridge as JB
-- import Prelude
-- import PrestoDOM.Animation as PrestoAnim
-- import PrestoDOM
-- import Screens.MultiModalFlow.JourneyTrackingScreen.Controller
-- import Screens.MultiModalFlow.JourneyTrackingScreen.ComponentConfig as CC
-- import Screens.Types as ST
-- import Styles.Colors as Color
-- import Components.GenericHeader as GenericHeader
-- import PrestoDOM.List (ListItem, preComputeListItem)
-- import CarouselHolder as CarouselHolder
-- import Components.BannerCarousel as BannerCarousel
-- import Effect.Aff (launchAff)
-- import Presto.Core.Types.Language.Flow (Flow)
-- import Font.Style as FontStyle
-- import Mobility.Prelude (boolToInvisibility, boolToVisibility)
-- import Components.BoxContainer as BoxContainer
-- import Components.InfoBox as InfoBox
-- import Components.DropDownWithHeader as DropDownWithHeader
-- import Debug
-- import Components.PrimaryButton as PrimaryButton
-- import Language.Strings (getString)
-- import Language.Types (STR(..))
-- import Screens.MultiModalFlow.Components.MetroCard as MetroCard
-- import Screens.MultiModalFlow.Components.VehicleCard as VehicleCard
-- import Common.Resources.Constants as CRC
-- import Services.API as API
-- import Animation as Anim
-- import Animation.Config (translateFullYAnimWithDurationConfig, translateYAnimConfig, Direction(..), AnimConfig, animConfig)
-- import Screens.MultiModalFlow.JourneyTrackingScreen.ScreenData
-- import Data.Lens ((^.))
-- import Accessor (_lat, _lon)
-- import Control.Monad.Except (runExceptT)
-- import Control.Transformers.Back.Trans (runBackT)
-- import Services.Backend as Remote
-- import Screens.RideBookingFlow.HomeScreen.Config as HSConfig
-- import Constants.Configs (getPolylineAnimationConfig)
-- import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout)
-- import PrestoDOM.Properties (sheetState, cornerRadii)
-- import PrestoDOM.Types.DomAttributes (Corners(..))
-- import Components.SeparatorView.View as SeparatorView

screen :: ST.JourneyTrackingScreenState -> Screen Action ST.JourneyTrackingScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "JourneyTrackingScreen"
  , globalEvents:
      [ ( \push -> do
            void $ launchAff $ EHC.flowRunner defaultGlobalState
              $ do
                  defaultMockInviteFlow 0 initialState
                  let
                    _ = spy "defaultMockInviteFlow" "defaultMockInviteFlow"
                  pure unit
            -- void $ launchAff $ flowRunner globalState $ updateMockData push initialState tracking.id
            pure $ pure unit
        )
      ]
  , eval:
      ( \action state -> do
          let _ = spy "JourneyTrackingScreen state -----" state
              _ = spy "JourneyTrackingScreen--------action" action
          eval action state
      )
  }

view :: forall w. (Action -> Effect Unit) -> ST.JourneyTrackingScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimation $ 
    relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ PrestoAnim.animationSet
      [ Anim.fadeIn true
      ]
      $ linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , id $ EHC.getNewIDWithTag "JourneyTrackingScreenMap"
          , onAnimationEnd
              ( \action -> do
                  void $ JB.showMap (EHC.getNewIDWithTag "JourneyTrackingScreenMap") true "satellite" CRC.pickupZoomLevel 0.0 0.0 push MapReady
                  push action
              )
              (const NoAction)
          ]
          []
    , relativeLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      ]
      [ linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        -- , background Color.ba
        , visibility GONE
        ]
        [ -- topLeftIconView push state
          -- dynamicProgressBarView push state
        -- , safetyButtonView push state
        ]
      , bottomSheetView push state
      ]
    -- journeyLegView push state
    ]

journeyLegView :: forall w. (Action -> Effect Unit) -> ST.JourneyTrackingScreenState -> PrestoDOM (Effect Unit) w
journeyLegView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 16 8 16 0
    , background Color.white900
    ]
    [ journeyLegTitleView push state
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ]
        [ verticalLineView push state true
        , journeyLegDescriptionView push state
        ]
    ]

journeyLegTitleView :: forall w. (Action -> Effect Unit) -> ST.JourneyTrackingScreenState -> PrestoDOM (Effect Unit) w
journeyLegTitleView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , cornerRadius 25.0
        , padding $ Padding 5 5 5 5
        , background Color.red
        ]
        [ imageView
            [ height $ V 10
            , width $ V 10
            , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_metro_icon"
            ]
        ]
    , textView
        $ [ text "Trinity Metro Station"
          , margin $ MarginLeft 8
          ]
        <> FontStyle.body1 CTA.TypoGraphy
    ]

journeyLegDescriptionView :: forall w. (Action -> Effect Unit) -> ST.JourneyTrackingScreenState -> PrestoDOM (Effect Unit) w
journeyLegDescriptionView push state =
  linearLayout
    [ height WRAP_CONTENT
    , weight 1.0
    , width WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ textView
        $ [ text "10 min"
          , margin $ MarginLeft 8
          ]
        <> FontStyle.body3 CTA.TypoGraphy
    -- , MetroCard.view (push <<< MetroCardAction) MetroCard.dummyRouteInfo
    , VehicleCard.view (push <<< VehicleCardAction) VehicleCard.dummyRouteInfo
    ]

verticalLineView :: forall w. (Action -> Effect Unit) -> ST.JourneyTrackingScreenState -> Boolean -> PrestoDOM (Effect Unit) w
verticalLineView push state isDotted =
  linearLayout
    [ height MATCH_PARENT
    , width $ V 2
    , margin $ Margin 9 8 9 0
    , background if isDotted then Color.white900 else Color.red
    ]
    [ SeparatorView.view separatorConfig
      -- relativeLayout
      --   [ height MATCH_PARENT
      --   , width MATCH_PARENT
      --   ]
      --   [ linearLayout
      --       [ height MATCH_PARENT, 
      --       width MATCH_PARENT, id $ EHC.getNewIDWithTag "VerticalDashView"
      --       ]
      --       []
      --   , linearLayout
      --       [ height MATCH_PARENT,
      --        width MATCH_PARENT, orientation VERTICAL
      --       ]
      --       ( map
      --           ( \_ ->
      --               imageView
      --                 [ height $ V 40
      --                 , width MATCH_PARENT
      --                 , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_vertical_line"
      --                 ]
      --           )
      --           [ 1, 2, 3, 4 ]
      --       )
      --   ]
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

--  , Tuple "BottomGradient" $ relativeLayout 
--     [ height MATCH_PARENT
--     , width MATCH_PARENT
--     , alignParentBottom "true,-1"
--     , gravity RIGHT
--     , orientation VERTICAL
--     , visibility $ boolToVisibility $ enableActions
--     ][ linearLayout
--       [ height $ V 60
--       , width MATCH_PARENT
--       , alignParentBottom "true,-1"
--       , gradient (Linear 0.0 [Color.white900, Color.transparent])
--       ][]
--    ]
--   , Tuple "TopGradient" $ relativeLayout 
--     [ height WRAP_CONTENT
--     , width MATCH_PARENT
--     , orientation VERTICAL
--     , visibility $ boolToVisibility $ enableActions
--     ][ linearLayout
--         [ height $ V 35
--         , width MATCH_PARENT
--         , gradient (Linear 180.0 [Color.white900, Color.transparent])
--         ][]
--       , if isFollowEnabled state 
--           then followView push $ getFollowers state
--           else linearLayout[visibility GONE][]
--     ]
--   -- , Tuple "WhereTo" $ linearLayout
--   --   [ height WRAP_CONTENT
--   --   , width MATCH_PARENT
--   --   , gravity RIGHT
--   --   , alignParentBottom "true,-1"
--   --   , orientation VERTICAL
--   --   , visibility $ boolToVisibility $ isHomeScreenView state
--   --   ][ imageView
--   --      [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_recenter_btn"
--   --      , accessibility DISABLE
--   --      , clickable true
--   --      , margin $ MarginRight 16
--   --      , onClick
--   --          ( \action -> do
--   --              _ <- push action
--   --              _ <- getCurrentPosition push UpdateCurrentLocation
--   --              _ <- pure $ logEvent state.data.logField "ny_user_recenter_btn_click"
--   --              pure unit
--   --          )
--   --          (const $ RecenterCurrentLocation)
--   --      , height $ V 32
--   --      , width $ V 32
--   --      ] 
-- ,relativeLayout
--   [ height MATCH_PARENT
--   , width MATCH_PARENT
--   , margin $ Margin 16 16 16 0
--   , padding buttonPadding
--   , alignParentBottom "true,-1"
--   , gradient (Linear 180.0 [Color.gunMetal, Color.eerieBlack])
--   , cornerRadius 12.0
--   , rippleColor "#000000"
--   , onClick ( \action -> do
--           void $ pure $ startLottieProcess lottieAnimationConfig{ rawJson = "primary_button_loader.json", lottieId = getNewIDWithTag "HomeScreenNewPrimaryButtonWithLottie"}
--           push action
--       ) $ const OpenSearchLocation
--   , gravity CENTER_VERTICAL
--   , visibility $ boolToVisibility $ enableActions
--   , accessibility ENABLE
--   , accessibilityHint "Where are you going? : Button"
--   ][ 
--     linearLayout
--       [ height WRAP_CONTENT
--       , visibility $ boolToInvisibility $ not state.props.homeScreenPrimaryButtonLottie
--       , gravity CENTER_VERTICAL
--       , width MATCH_PARENT
--       ][
--         imageView
--         [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_search_yellow"
--         , height $ V 16
--         , width $ V 16
--         , gravity CENTER_VERTICAL
--         , margin $ MarginRight 12
--         ]
--       , textView $ 
--         [ text $ getString WHERE_ARE_YOU_GOING
--         , color Color.yellow900
--         , singleLine false
--         , padding $ if (getLanguageLocale languageKey) == "EN_US" && os /= "IOS" then PaddingBottom 4 else PaddingBottom 0
--         ] <> FontStyle.subHeading3 TypoGraphy
--       ]
--   , linearLayout
--     ([ height WRAP_CONTENT
--     , width MATCH_PARENT
--     , gravity CENTER
--     ])
--     [ lottieAnimationView
--         [ id (getNewIDWithTag "HomeScreenNewPrimaryButtonWithLottie")
--         , visibility $ boolToInvisibility state.props.homeScreenPrimaryButtonLottie
--         , height $ V 27
--         , width $ V 100
--         ]
--     ]
--   ]
-- ]
-- ]

bottomSheetView :: forall w. (Action -> Effect Unit) -> ST.JourneyTrackingScreenState -> PrestoDOM (Effect Unit) w
bottomSheetView push state = --let
  -- isRideData = isJust state.data.driverInfoCardState
  -- in 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , alignParentBottom "true,-1"
    ]
    [ coordinatorLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , cornerRadii $ Corners 24.0 true true false false
        ]
        [ bottomSheetLayout
            ( [ height WRAP_CONTENT
              , width MATCH_PARENT
              , peakHeight $ 400 --getPeekHeight state
              , enableShift false
              , cornerRadii $ Corners 24.0 true true false false
              ]
                <> Mb.maybe [] (\sheet -> [ sheetState sheet ]) Mb.Nothing
            ) --state.props.sheetState)
            [ linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              ]
              [ linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , layoutGravity "right"
                , orientation HORIZONTAL
                ]
                [ toggleViewButtons push state
                , shareRideButton push state  
                ]
              , smartAlertsView push state
              , linearLayout
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , orientation VERTICAL
                  , gravity CENTER
                  , background if state.props.showRouteDetailsTab then Color.white900 else Color.grey700
                  , padding $ PaddingBottom 16
                  , cornerRadii $ Corners 24.0 true true false false
                  ]
                  [ bottomSheetContentView push state
                  ]
              ]
            ]
        ]
    ]

bottomSheetContentView :: forall w. (Action -> Effect Unit) -> ST.JourneyTrackingScreenState -> PrestoDOM (Effect Unit) w
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
          , padding $ PaddingVertical 6 16
          , cornerRadii $ Corners 12.0 true true true true
          ]
            <> if state.props.showRouteDetailsTab then [] else [ margin $ MarginHorizontal 16 16 ]
        )
        [ journeyLegView push state
        , journeyLegView push state
        ]
    ]

-- localDelay 180000.0
-- trackingId <- liftFlow $ runEffectFn1 getValueFromIdMap "FollowsRide"
-- when (id == trackingId.id) $ do
--   pushAction BackPressed

smartAlertsView :: forall w. (Action -> Effect Unit) -> ST.JourneyTrackingScreenState -> PrestoDOM (Effect Unit) w
smartAlertsView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.transparent
  , margin $ MarginBottom 8
  ]
  [ -- Configure this as per response
    -- (if disableSuggestions state then 
    --     PrestoAnim.animationSet[] 
    --   else (if state.props.showChatNotification then 
    --     PrestoAnim.animationSet [translateYAnimFromTop $ messageInAnimConfig true] 
    --   else if state.props.isNotificationExpanded then 
    --     PrestoAnim.animationSet [translateYAnimFromTop $ messageOutAnimConfig true] 
    --   else PrestoAnim.animationSet[scaleYAnimWithDelay 5000])) $ 
     linearLayout
     [ height $ WRAP_CONTENT
     , width MATCH_PARENT
     , padding $ PaddingHorizontal 8 8
     , gravity BOTTOM
    --  , accessibility ENABLE
    --  , onAnimationEnd push $ const $ NotificationAnimationEnd
     , orientation VERTICAL
     ]
     [ AlertWidget.view (push <<< AlertWidgetAction) (getAlertWidgetConfig state)
     ]
  ]
  -- where disableSuggestions state = not $ state.data.config.feature.enableChat || state.data.config.feature.enableSuggestions/

toggleViewButtons :: forall w. (Action -> Effect Unit) -> ST.JourneyTrackingScreenState -> PrestoDOM (Effect Unit) w
toggleViewButtons push state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , layoutGravity "center_vertical"
  -- , gravity CENTER
  , orientation HORIZONTAL
  , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5
  , background Color.white900
  , stroke $ "1,"<> Color.grey900
  , cornerRadius if EHC.os == "IOS" then 20.0 else 32.0
  -- , margin $ Margin 8 8 8 8
  ]
  [ linearLayout
    [ width $ V 32
    , height $ V 36
    , gravity CENTER
    , background Color.white900
    -- , stroke $ "1,"<> Color.grey900
    , cornerRadius if EHC.os == "IOS" then 20.0 else 32.0
    , clickable true
    , accessibilityHint "Share Ride : Button : Select to share ride details"
    , accessibility ENABLE
    , onClick push $ const ToggleViewButtonClicked
    , margin $ Margin 2 2 2 2
    -- , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5
    , rippleColor Color.rippleShade
    ][ imageView
      [ imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_bulleted_list"
      , height $ V 16
      , width $ V 16
      , accessibility DISABLE
      --  , alpha if state.data.providerType == ONUS then 1.0 else 0.5
      ]
    ]
  , linearLayout
    [ width $ V 32
    , height $ V 36
    , gravity CENTER
    , background Color.white900
    -- , stroke $ "1,"<> Color.grey900
    , cornerRadius if EHC.os == "IOS" then 20.0 else 32.0
    , clickable true
    , accessibilityHint "Share Ride : Button : Select to share ride details"
    , accessibility ENABLE
    , onClick push $ const ToggleViewButtonClicked
    , margin $ Margin 2 2 2 2
    , rippleColor Color.rippleShade
    ][ imageView
      [ imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_bulleted_list"
      , height $ V 16
      , width $ V 16
      , accessibility DISABLE
      --  , alpha if state.data.providerType == ONUS then 1.0 else 0.5
      ]
    ]
  ]

shareRideButton :: forall w. (Action -> Effect Unit) -> ST.JourneyTrackingScreenState -> PrestoDOM ( Effect Unit) w
shareRideButton push state = 
  linearLayout
  [ width $ V 56
  , height $ V 56
  , layoutGravity "bottom"
  , orientation VERTICAL
  , clickable true
  , accessibility DISABLE
  , clipChildren false
  -- , margin $ Margin 8 8 8 8
  ][ linearLayout
     [ width $ V 40
     , height $ V 40
     , gravity CENTER
     , background Color.white900
     , stroke $ "1,"<> Color.grey900
     , cornerRadius if EHC.os == "IOS" then 20.0 else 32.0
     , clickable true
     , accessibilityHint "Share Ride : Button : Select to share ride details"
     , accessibility ENABLE
     , onClick push $ const ShareRide
     , margin $ Margin 8 9 8 9
     , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5
     , rippleColor Color.rippleShade
     ][ imageView
       [ imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_share_icon"
       , height $ V 16
       , width $ V 16
       , accessibility DISABLE
      --  , alpha if state.data.providerType == ONUS then 1.0 else 0.5
       ]
     ]
  ]

-- topLeftIconView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
-- topLeftIconView state push =
--   let image = if (any (_ == state.props.currentStage) [ SettingPrice, ConfirmingLocation, PricingTutorial, EditPickUpLocation, DistanceOutsideLimits ]) then fetchImage FF_COMMON_ASSET "ny_ic_chevron_left" else if state.data.config.dashboard.enable && (checkVersion "LazyCheck") then fetchImage FF_ASSET "ic_menu_notify" else fetchImage FF_ASSET "ny_ic_hamburger"
--       onClickAction = if (any (_ == state.props.currentStage) [ SettingPrice, ConfirmingLocation, PricingTutorial, EditPickUpLocation, DistanceOutsideLimits ]) then const BackPressed else const OpenSettings
--       isBackPress = (any (_ == state.props.currentStage) [ SettingPrice, ConfirmingLocation, PricingTutorial, EditPickUpLocation, DistanceOutsideLimits ]) 
--       followerBar = (showFollowerBar (fromMaybe [] state.data.followers) state) && (any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithDriver])
--       isEditDestination = any (_ == state.props.currentStage) [EditingDestinationLoc, ConfirmEditDestinationLoc, ConfirmingEditDestinationLoc, RevisedEstimate]
--       isVisible = state.data.config.showHamMenu && not isEditDestination && not ((not state.props.rideRequestFlow) || any (_ == state.props.currentStage) [ FindingEstimate, ConfirmingRide, HomeScreen])
--       manuallySharedFollowers = fromMaybe [] state.data.manuallySharedFollowers
--   in 
--   linearLayout
--     [ width MATCH_PARENT
--     , height WRAP_CONTENT
--     , orientation VERTICAL
--     , visibility $ boolToVisibility isVisible
--     , margin $ MarginTop if followerBar then 0 else safeMarginTop
--     ]
--     $ []
--     <> ( if isFollowEnabled state && followerBar then [ followRideBar push (getFollowers state) (MATCH_PARENT) true false] else []
--       )
--     <> ( [ linearLayout
--             [ width MATCH_PARENT
--             , height WRAP_CONTENT
--             , orientation HORIZONTAL
--             , visibility $ boolToVisibility  state.data.config.showHamMenu
--             , margin $ Margin 16 20 0 0
--             , accessibility if state.data.settingSideBar.opened /= SettingSideBar.CLOSED || state.props.currentStage == ChatWithDriver || state.props.isCancelRide || state.props.isLocationTracking || state.props.callSupportPopUp || state.props.cancelSearchCallDriver || state.props.showCallPopUp || state.props.showRateCard || state.data.waitTimeInfo then DISABLE_DESCENDANT else DISABLE
--             ]
--             [ linearLayout
--                 [ height $ V 48
--                 , width $ V 48
--                 , stroke ("1," <> Color.grey900)
--                 , background Color.white900
--                 , gravity CENTER
--                 , cornerRadius 24.0
--                 , visibility $ boolToVisibility $ not (any (_ == state.props.currentStage) [ FindingEstimate, ConfirmingRide, FindingQuotes, TryAgain, RideCompleted, RideRating, ReAllocated, SearchLocationModel ])
--                 , clickable true
--                 , onClick push $ if isBackPress then const BackPressed else const OpenSettings
--                 , accessibilityHint if isBackPress then "Back : Button" else "Menu : Button"
--                 , accessibility ENABLE
--                 , rippleColor Color.rippleShade
--                 ]
--                 [ imageView
--                     [ imageWithFallback image
--                     , height $ V 25
--                     , accessibility DISABLE
--                     , clickable true
--                     , onClick push $ onClickAction
--                     , width $ V 25
--                     ]
--                 ]
--             , linearLayout
--                 [ height WRAP_CONTENT
--                 , weight 1.0
--                 ]
--                 []
--             , sosView push state
--             , if (not state.data.config.dashboard.enable) || (isPreviousVersion (getValueToLocalStore VERSION_NAME) (if os == "IOS" then "1.2.5" else "1.2.1")) then emptyTextView state else liveStatsDashboardView push state
--             ]
--         ]
--       )
--     <> (case state.data.upcomingRideDetails of 
--           Nothing -> []
--           Just rideDetails -> [upcomingRideBanner push rideDetails])
--   where
--   upcomingRideBanner push rideDetails = 
--     linearLayout
--           [ height $ WRAP_CONTENT
--           , width MATCH_PARENT
--           , background Color.blueGreen
--           , margin $ Margin 16 16 16 0 
--           , padding $ Padding 2 4 2 4
--           , cornerRadius 8.0
--           , gravity CENTER
--           ][  textView $ 
--               [ textFromHtml $ getString $ YOU_HAVE_AN_UPCOMING_BOOKING (rideDetails.rideScheduledAt)
--                 , color Color.white900
--               ] <> FontStyle.tags TypoGraphy ]

-- dynamicProgressBarView :: forall w. (Action -> Effect Unit) -> ST.JourneyTrackingScreenState -> PrestoDOM (Effect Unit) w
-- dynamicProgressBarView push state =
--   linearLayout
--   [ height WRAP_CONTENT
--   , width WRAP_CONTENT 
--   , layoutGravity "center_horizontal"
--   , margin $ MarginTop EHC.safeMarginTop
--   , padding $ Padding 12 12 12 12
--   , cornerRadius 24.0
--   , background Color.white900
--   , stroke $ "1,"<> Color.grey900
--   , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5
--   ]
--   [ linearLayout
--     [ height WRAP_CONTENT
--     , width WRAP_CONTENT
--     , background Color.black650
--     ]
--     [ imageView
--       [ height $ V 16
--       , width $ V 16
--       , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_walk_icon"
--       , margin $ Margin 4 4 4 4
--       ]
--     ]
--   , linearLayout
--     [ height WRAP_CONTENT
--     , width WRAP_CONTENT
--     , background Color.black650
--     ]
--     [ imageView
--       [ height $ V 16
--       , width $ V 16
--       , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_walk_icon"
--       , margin $ Margin 4 4 4 4
--       ]
--     ]
--   , linearLayout
--     [ height WRAP_CONTENT
--     , width WRAP_CONTENT
--     , background Color.black650
--     ]
--     [ imageView
--       [ height $ V 16
--       , width $ V 16
--       , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_walk_icon"
--       , margin $ Margin 4 4 4 4
--       ]
--     ]
--   ]

separatorConfig :: SeparatorView.Config
separatorConfig = 
  { orientation : VERTICAL
  , count : 50
  , height : V 10
  , width : V 2
  , layoutWidth : V 2
  , layoutHeight : V 100
  , color : Color.black500
  }

rideActionsPopupView :: forall w. (Action -> Effect Unit) -> ST.JourneyTrackingScreenState -> PrestoDOM ( Effect Unit) w
rideActionsPopupView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , alignParentBottom "true,-1"
  , orientation VERTICAL
  ][ VehicleCard.view (push <<< VehicleCardAction) VehicleCard.dummyRouteInfo
  -- , 
  ]

actionListVIew :: forall w. (Action -> Effect Unit) -> ST.JourneyTrackingScreenState -> PrestoDOM ( Effect Unit) w
actionListVIew push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][]