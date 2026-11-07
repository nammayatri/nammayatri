{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideBookingFlow.PickupInstructionsScreen.View where

import Animation as Anim
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, map, pure, unit, ($), (&&), (<<<), (<>), (==), (>), not, void, discard, (-), show, (*), (<=), (>=), (/))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, background, color, cornerRadius, fontStyle, relativeLayout, gravity, height, alpha, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, visibility, weight, width, singleLine, id, frameLayout, scrollBarY, fillViewport, onAnimationEnd, rippleColor, alignParentBottom, progressBar)
import Engineering.Helpers.Commons (safeMarginBottom, safeMarginTop)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.RideBookingFlow.PickupInstructionsScreen.Controller (Action(..), eval, ScreenOutput)
import Data.Maybe (fromMaybe, isJust, Maybe(..))
import Mobility.Prelude (boolToVisibility)
import Engineering.Helpers.Commons as EHC
import Components.RateCard as RateCard
import RemoteConfig as RC
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App as CT
import Helpers.Utils as HU
import Data.Array as DA
import Data.Ord as DO
import JBridge as JB
import ConfigProvider as CP
import Services.API as API
import Constants as CS
import Data.Int as DI
import Data.Array as DA
import Debug

screen :: ST.PickupInstructionsScreenState -> Screen Action ST.PickupInstructionsScreenState ScreenOutput
screen initialState =
    { initialState
    , view: view
    , name: "PickupInstructionsScreen"
    , globalEvents: [ (\push -> pure $ pure unit) ]
    , eval:
        ( \state action -> do
            let _ = spy "PickupInstructionsScreen state" state
            let _ = spy "PickupInstructionsScreen action" action
            eval state action
        ) 
    }


view :: forall w. (Action -> Effect Unit) -> ST.PickupInstructionsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
    relativeLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , background Color.white900
     , onBackPressed push $ const BackClick
     , margin $ Margin 0 safeMarginTop 0 0
     ] $
     [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        ]
        [ headerLayout push state
        , scrollView
          [ width MATCH_PARENT
          , weight 1.0
          , scrollBarY false
          , margin $ MarginBottom if EHC.os == "IOS" then 85 else 0
          ][ linearLayout
              [ width MATCH_PARENT
              , height MATCH_PARENT
              , orientation VERTICAL
              ][    linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , orientation VERTICAL
                    ](map (\item -> instructionItem push item) state.data.pickupInstructions)
              ]
          ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , alignParentBottom "true,-1"
            , stroke $ "1," <> Color.grey900
            , background Color.white900
            ][ PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)]
        ]
    ]

instructionItem :: forall w. (Action -> Effect Unit) -> RC.PickupInstructions -> PrestoDOM (Effect Unit) w
instructionItem push item =
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER_HORIZONTAL
    , orientation VERTICAL
    , margin $ Margin 16 20 16 0
    , background Color.blue600
    , stroke $ "1," <> Color.grey900
    , cornerRadius 8.0
    ][  
        textView $ 
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , text item.title
        , color Color.black800
        , padding $ Padding 12 12 12 12
        ] <> FontStyle.body1 CT.TypoGraphy
      , imageView
        [ width MATCH_PARENT
        , height $ V 240
        , imageUrl item.image
        ]
    ]


headerLayout :: forall w. (Action -> Effect Unit) -> ST.PickupInstructionsScreenState -> PrestoDOM (Effect Unit) w
headerLayout push state =
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , layoutGravity "center_vertical"
    , padding $ PaddingVertical 10 10
    , stroke $ "1," <> Color.grey900
    ]
    [ imageView
        [ width $ V 30
        , height $ V 30
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
        , gravity CENTER_VERTICAL
        , onClick push $ const BackClick
        , padding $ Padding 2 2 2 2
        , margin $ MarginLeft 5
        ]
    , textView
        $ [ width WRAP_CONTENT
            , height MATCH_PARENT
            , text $ getString WALKING_DIRECTIONS_TO_PICKUP
            , margin $ MarginLeft 20
            , color Color.black
            , weight 1.0
            , gravity CENTER_VERTICAL
            , alpha 0.8
            ]
        <> FontStyle.h3 CT.TypoGraphy
    ]



primaryButtonConfig :: ST.PickupInstructionsScreenState -> PrimaryButton.Config
primaryButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = getString DONE
      , color = Color.primaryButtonColor
      }
      , background = Color.black900
      , height = V 50
      , cornerRadius = 8.0
      , margin = Margin 16 16 16 16
      , id = "pickup_instructions_primary_button"
      }
  in primaryButtonConfig'