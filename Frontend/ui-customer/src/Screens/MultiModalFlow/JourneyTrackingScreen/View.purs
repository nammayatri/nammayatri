{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.MultiModalFlow.JourneyTrackingScreen.View where

import Animation (screenAnimation)
import Common.Types.App as CTA
import Data.Maybe as Mb
import Data.Array as DA
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Helpers.Utils as HU
import Types.App (GlobalState(..), defaultGlobalState)
import JBridge as JB
import Prelude
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM
import Screens.MultiModalFlow.JourneyTrackingScreen.Controller
import Screens.MultiModalFlow.JourneyTrackingScreen.ComponentConfig as CC
import Screens.Types as ST
import Styles.Colors as Color
import Components.GenericHeader as GenericHeader
import PrestoDOM.List (ListItem, preComputeListItem)
import CarouselHolder as CarouselHolder
import Components.BannerCarousel as BannerCarousel
import Effect.Aff (launchAff)
import Presto.Core.Types.Language.Flow (Flow)
import Font.Style as FontStyle
import Mobility.Prelude (boolToInvisibility, boolToVisibility)
import Components.BoxContainer as BoxContainer
import Components.InfoBox as InfoBox
import Components.DropDownWithHeader as DropDownWithHeader
import Debug
import Components.PrimaryButton as PrimaryButton
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.MultiModalFlow.Components.MetroCard as MetroCard
import Screens.MultiModalFlow.Components.VehicleCard as VehicleCard
import Common.Resources.Constants as CRC
import Services.API as API
import Animation as Anim
import Animation.Config (translateFullYAnimWithDurationConfig, translateYAnimConfig, Direction(..), AnimConfig, animConfig)
import Screens.MultiModalFlow.JourneyTrackingScreen.ScreenData
import Data.Lens ((^.))
import Accessor (_lat, _lon)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Services.Backend as Remote
import Screens.RideBookingFlow.HomeScreen.Config as HSConfig
import Constants.Configs (getPolylineAnimationConfig)
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout)
import PrestoDOM.Properties (sheetState, cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))

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
          let
            _ = spy "JourneyTrackingScreen state -----" state
          let
            _ = spy "JourneyTrackingScreen--------action" action
          eval action state
      )
  }

view :: forall w. (Action -> Effect Unit) -> ST.JourneyTrackingScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimation
    $ relativeLayout
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
            [ --messageWidgetView push state
            -- ,
            bottomSheetView push state
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
    ]
    [ journeyLegTitleView push state
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ]
        [ verticalLineView push state
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
    , MetroCard.view (push <<< MetroCardAction) MetroCard.dummyRouteInfo
    -- , VehicleCard.view (push <<< VehicleCardAction) VehicleCard.dummyRouteInfo
    ]

verticalLineView :: forall w. (Action -> Effect Unit) -> ST.JourneyTrackingScreenState -> PrestoDOM (Effect Unit) w
verticalLineView push state =
  linearLayout
    [ height MATCH_PARENT
    , width $ V 2
    , margin $ Margin 9 8 9 0
    , background Color.red
    ]
    []

-- mapView :: forall w. (Action -> Effect Unit) -> ST.JourneyTrackingScreenState -> PrestoDOM (Effect Unit) w
-- mapView push state = 
--   -- let mapDimensions = getMapDimensions state
--       -- bottomPadding = if state.props.currentStage == ConfirmingLocation then getDefaultPixelSize extraBottomPadding else 0
--       -- banners = getBannerConfigs state BannerCarousel
--   --     enableActions = isHomeScreenView state && state.props.isSrcServiceable
--   --     buttonPadding = if os == "IOS" then Padding 16 16 16 12 else Padding 16 16 16 16
--   -- in
--   -- PrestoAnim.animationSet[scaleYAnimWithDelay 5000] $
--   relativeLayout
--     [ height MATCH_PARENT
--     , width MATCH_PARENT
--     -- , cornerRadius if state.props.currentStage == HomeScreen then 16.0 else 0.0
--     -- , padding $ PaddingBottom $ bottomPadding
--     ]$[ linearLayout
--           ([ height MATCH_PARENT
--           , width MATCH_PARENT
--           , accessibility DISABLE_DESCENDANT
--           , id (EHC.getNewIDWithTag "MapViewJourneyTrackingScreen")
--           , visibility if (state.props.isSrcServiceable && not state.props.userBlocked) then VISIBLE else GONE
--           -- , cornerRadius if state.props.currentStage == HomeScreen && os == "IOS" then 16.0 else 0.0
--           , background Color.white900
--           , afterRender
--             ( \action -> do
--                 _ <- push action
--                 -- if state.props.sourceLat == 0.0 && state.props.sourceLong == 0.0 then do
--                 --   void $ getCurrentPosition push CurrentLocation
--                 -- else pure unit
--                 _ <- showMap (EHC.getNewIDWithTag "MapViewJourneyTrackingScreen") true "satellite" zoomLevel state.props.sourceLat state.props.sourceLong push MAPREADY
--                 if os == "IOS" then
--                   case state.props.currentStage of  
--                     HomeScreen -> void $ setMapPadding 0 0 0 0
--                     ConfirmingLocation -> void $ runEffectFn1 locateOnMap locateOnMapConfig { goToCurrentLocation = false, lat = state.props.sourceLat, lon = state.props.sourceLong, geoJson = state.data.polygonCoordinates, points = state.data.nearByPickUpPoints, zoomLevel = zoomLevel, labelId = getNewIDWithTag "LocateOnMapPin" }
--                     _ -> pure unit
--                 else pure unit
--                 if state.props.openChatScreen && state.props.currentStage == RideAccepted then push OpenChatScreen
--                 else pure unit
--                 case state.props.currentStage of
--                   HomeScreen -> if ((getSearchType unit) == "direct_search") then push DirectSearch else pure unit
--                   _ -> pure unit
--                 pure unit
--             )
--             (const MapReadyAction)
--           ])[]
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
-- messageWidgetView :: forall w. (Action -> Effect Unit) -> FollowRideScreenState -> PrestoDOM (Effect Unit) w
-- messageWidgetView push state = 
--   linearLayout
--   [ height MATCH_PARENT
--   , width MATCH_PARENT
--   , orientation VERTICAL
--   ][ (if disableSuggestions state then 
--         PrestoAnim.animationSet[] 
--       else (if state.props.showChatNotification then 
--         PrestoAnim.animationSet [translateYAnimFromTop $ messageInAnimConfig true] 
--       else if state.props.isNotificationExpanded then 
--         PrestoAnim.animationSet [translateYAnimFromTop $ messageOutAnimConfig true] 
--       else PrestoAnim.animationSet[scaleYAnimWithDelay 5000])) $ 
--      linearLayout
--      [ height $ MATCH_PARENT
--      , width MATCH_PARENT
--      , padding $ PaddingHorizontal 8 8
--      , alignParentBottom "true,-1"
--      , gravity BOTTOM
--      , accessibility DISABLE
--      , onAnimationEnd push $ const $ NotificationAnimationEnd
--      , orientation VERTICAL
--      ][ messageNotificationView push (getMessageNotificationViewConfig state)
--       , linearLayout
--         [ height $ V $ ((getPeekHeight state) - if state.props.removeNotification then 0 else 180)
--         , width $ MATCH_PARENT
--         , accessibility DISABLE
--         ][]
--      ]
--   ]
--   where disableSuggestions state = not $ state.data.config.feature.enableChat || state.data.config.feature.enableSuggestions
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
        ]
        [ bottomSheetLayout
            ([ height WRAP_CONTENT
            , width MATCH_PARENT
            , peakHeight $ 400 --getPeekHeight state
            , enableShift false
            ] <> Mb.maybe [] (\sheet -> [sheetState sheet]) Mb.Nothing)--state.props.sheetState)
            [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                [ linearLayout
                    [ height $ V 500
                    , width MATCH_PARENT
                    , background Color.white900
                    , orientation VERTICAL
                    , gravity CENTER
                    , cornerRadii $ Corners 24.0 true true false false
                    ]
                    [ journeyLegView push state
                    , journeyLegView push state
                    ]
                    -- [ linearLayout
                    --     [ width MATCH_PARENT
                    --     , height WRAP_CONTENT
                    --     , orientation VERTICAL
                    --     , gravity CENTER
                    --     , id $ EHC.getNewIDWithTag "JourneyTrackingHeaderView"
                    --     ]
                    --     [ --knobView
                    --     -- , headerView push state
                    --     -- , emergencyActionsView push state
                    --     ]
                    -- , addressView push state
                    -- , if isJust state.data.driverInfoCardState 
                    --     then driverInfoView push state
                    --     else PrestoAnim.animationSet [ fadeOut isRideData ]
                    --           $ linearLayout
                    --               [ width MATCH_PARENT
                    --               , height WRAP_CONTENT
                    --               , visibility $ boolToVisibility $ not $ isJust state.data.driverInfoCardState
                    --               ]
                    --               [ driverInfoShimmer
                    --               ]
                    -- ]
                ]
            ]
        ]
    ]
-- localDelay 180000.0
-- trackingId <- liftFlow $ runEffectFn1 getValueFromIdMap "FollowsRide"
-- when (id == trackingId.id) $ do
--   pushAction BackPressed
