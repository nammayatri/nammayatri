{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RentalScheduleRide.View where

import Common.Types.App

import Animation (translateYAnimFromTop)
import Animation.Config (translateFullYAnimWithDurationConfig, translateYAnimHomeConfig, Direction(..))
import Common.Types.App (LazyCheck(..))
import Components.LocationListItem as LocationListItem
import Components.LocationTagBar as LocationTagBar
import Components.PrimaryButton as PrimaryButton
import Components.RentalScheduleRide.Controller (Action(..), SearchLocationModelState)
import Data.Array (mapWithIndex, length)
import Data.Function (flip)
import Data.Maybe (Maybe(..), fromMaybe)
import Debug (spy)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag, isPreviousVersion, os, safeMarginBottom, safeMarginTop, screenHeight, screenWidth, setText)
import Engineering.Helpers.LogEvent (logEvent)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getLocationName, getPreviousVersion, getSearchType)
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import JBridge (getBtnLoader, showKeyboard, getCurrentPosition, firebaseLogEvent)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude ((<>))
import Prelude (Unit, bind, const, map, pure, unit, ($), (&&), (+), (-), (/), (/=), (<<<), (<>), (==), (||), not, discard)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Accessiblity(..), Padding(..), PrestoDOM, Visibility(..), Accessiblity(..), accessibilityHint ,adjustViewWithKeyboard, afterRender, alignParentBottom, alpha, autoCorrectionType, background, clickable, color, cornerRadius, cursorColor, disableClickFeedback, editText, ellipsize, fontStyle, frameLayout, gravity, height, hint, hintColor, id, imageUrl, imageView, imageWithFallback, inputTypeI, lineHeight, linearLayout, margin, onBackPressed, onChange, onClick, onFocus, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, accessibility)
import PrestoDOM.Animation as PrestoAnim
import Resources.Constants (getDelayForAutoComplete)
import Screens.Types (RentalStage(..), SearchLocationModelType(..), LocationListItemState)
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Data.String as DS

view :: forall w. (Action -> Effect Unit) -> SearchLocationModelState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , onBackPressed push (const $ GoBack)
    ][ linearLayout
        [ height $ V 48
        , width $ V 48
        , onClick push (const GoBack)
        , disableClickFeedback true
        , margin (Margin 16 10 16 0)
        , gravity CENTER
        ]
        [ imageView
            [ height $ V 25
            , width $ V 25
            , accessibilityHint "Back : Button"
            , accessibility ENABLE
            , imageWithFallback $ "jp_toolbarbackarrow_1_2c2f3a_2023_06_23_18_36_07" 
            ]
        ]
      , primaryButtonView push state
    ]

primaryButtonConfig :: SearchLocationModelState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
        { text =  "Confirm"
        , color = state.homeScreenConfig.primaryTextColor
        , height = V 40
        }
      , height = V state.homeScreenConfig.searchLocationConfig.primaryButtonHeight
      , gravity = CENTER
      , cornerRadius = state.homeScreenConfig.primaryButtonCornerRadius
      , background = state.homeScreenConfig.primaryBackground
      , margin = (MarginHorizontal 16 16)
      , isClickable = if (state.rentalStage == RentalSearchLocation) then true else if (state.isSource == Just true && state.source /= "" && state.isSrcServiceable) || (state.isSource == Just false && state.destination /= "" && state.isDestServiceable) then true else false
      , id = "RentalConfirmBooking"
      }
  in primaryButtonConfig'

primaryButtonView :: forall w. (Action -> Effect Unit) -> SearchLocationModelState -> PrestoDOM (Effect Unit) w
primaryButtonView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , weight 0.0
    , gravity BOTTOM
    , margin (MarginBottom 14)
    ][ PrimaryButton.view (push <<< PrimaryButtonActionController)(primaryButtonConfig state)
    ]