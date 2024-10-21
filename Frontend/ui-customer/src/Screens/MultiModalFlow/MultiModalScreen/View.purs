{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.MultiModalFlow.MultiModalScreen.View
  ( multiModalScreen
  , view
  )
  where

import Prelude

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Common.Resources.Constants (zoomLevel)
import Components.ChooseVehicle.View as ChooseVehicle
import Components.GenericHeader.View as GenericHeader
import Animation.Config (removeYAnimFromTopConfig)
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination.View as SourceToDestinationView
import Components.PopUpModal as PopUpModal
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (isJust, isNothing, maybe, fromMaybe, Maybe(..))
import Data.String as DS
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)
import DecodeUtil (getAnyFromWindow)
import Data.Function.Uncurried (runFn3)
import Common.Types.App as CT
import Debug (spy)
import Effect (Effect)
import Components.RateCard.View as RateCard
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Animation (fadeIn, scaleYAnimWithDelay)
import Effect.Uncurried (runEffectFn2)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.CommonView (emptyTextView)
import Helpers.Utils (FetchImageFrom(..), fetchImage, decodeError, storeCallBackCustomer)
import JBridge as JB
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Presto.Core.Types.Language.Flow (Flow, doAff, delay)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..) ,background, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, relativeLayout, scrollView, shimmerFrameLayout, onBackPressed, alignParentBottom, singleLine, accessibilityHint,accessibility,accessibilityHint, Accessiblity(..), id, afterRender, layoutGravity, rippleColor, maxLines, ellipsize, onAnimationEnd, scrollBarY, fillViewport, enableShift, peakHeight, halfExpandedRatio, onStateChanged, frameLayout, clickable)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout)
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.Properties (cornerRadii, sheetState)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Resources.Constants
import Resources.Localizable.EN (getEN)
import Screens.MultiModalFlow.MultiModalScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.MultiModalFlow.MultiModalScreen.ComponentConfig -- (chooseVehicleConfig, deliveryPickupDetialsModalConfig, genericHeaderConfig, primaryButtonConfig, rateCardConfig, decodeAddress')
import Screens.HomeScreen.ScreenData (dummyRideBooking)
import Screens.Types as ST
import Services.API
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (GlobalState, defaultGlobalState)

multiModalScreen :: ST.MultiModalScreenState -> Screen Action ST.MultiModalScreenState ScreenOutput
multiModalScreen initialState =
  { initialState
  , view
  , name: "MultiModalScreen"
  , globalEvents: []
  , eval:
      \action state -> do
        let _ = spy "MultiModalScreen action " action
        let _ = spy "MultiModalScreen state " state
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> ST.MultiModalScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , onBackPressed push $ const BackPressed
    , onClick push $ const NoAction
    , accessibility DISABLE
    ] 
    [ frameLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , accessibility DISABLE
      , clickable true
      ]
      [ genericMapView push state 
        -- multiModalRideDetailsView push state
      ]
    ]
  
genericMapView :: forall w. (Action -> Effect Unit) -> ST.MultiModalScreenState -> PrestoDOM (Effect Unit) w
genericMapView push state =
  PrestoAnim.animationSet[scaleYAnimWithDelay 5000] $
  Keyed.relativeLayout
    [ height getMapHeight
    , width MATCH_PARENT
    ] $
    [ Tuple ("MapView" <> idTag) $ 
        linearLayout
          ([ height getMapHeight
          , width MATCH_PARENT
          , accessibility DISABLE_DESCENDANT
          , id $ EHC.getNewIDWithTag idTag
          -- , visibility $ boolToVisibility $ state.data.currentStage /= ST.Instructions
          -- , cornerRadius if state.props.currentStage == HomeScreen && os == "IOS" then 16.0 else 0.0
          , background Color.white900
          , afterRender
            ( \action ->
                void $ JB.showMap (EHC.getNewIDWithTag idTag) true "satellite" zoomLevel state.data.currentLat state.data.currentLong push MapViewLoaded
            ) $ const NoAction
          ])[]
    ]
  where
    idTag :: String
    idTag = "MapView"
      -- case state.data.currentStage of
      --   HomeScreen -> "HomeScreenMapView"
      --   _ -> "MapView"

    getMapHeight :: Length
    getMapHeight = V $ ((EHC.screenHeight unit) / 15) * 10
      -- case state.data.currentStage of
      --   HomeScreen -> V $ JB.getHeightFromPercent 20
      --   _ -> MATCH_PARENT      

multiModalRideDetailsView :: forall w. (Action -> Effect Unit) -> ST.MultiModalScreenState -> PrestoDOM (Effect Unit) w
multiModalRideDetailsView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding (Padding 0 0 0 0)
  , margin $ MarginTop topMargin
  , background Color.transparent
  -- , accessibility if (state.data.settingSideBar.opened /= SettingSideBar.CLOSED) || state.props.currentStage == ChatWithDriver || state.props.cancelSearchCallDriver || state.props.showCallPopUp || state.props.isCancelRide || state.props.isLocationTracking || state.props.callSupportPopUp || (state.props.showShareAppPopUp && state.data.config.feature.enableShareApp) || state.data.waitTimeInfo then DISABLE_DESCENDANT else DISABLE
  , alignParentBottom "true,-1"
  ]
  [ coordinatorLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    ]
    [ bottomSheetLayout
      ([ height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.transparent 
      , accessibility DISABLE
      , enableShift false
      , peakHeight 400-- $ getInfoCardPeekHeight state
      , halfExpandedRatio 611.0 -- $ halfExpanded
      , orientation VERTICAL
      ] <> 
        if EHC.os == "ANDROID" then 
          [ onStateChanged push $ ScrollStateChanged
          , sheetState state.props.currentSheetState] 
        else 
          case state.props.sheetState of
            Nothing -> []
            Just state -> [sheetState state])
      [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          ]
          [ emptyTextView
          ]
      ]
    ]
  ]
  where
    topMargin = 60 + if EHC.os == "IOS" then EHC.safeMarginTop else 20