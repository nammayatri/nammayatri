{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerUtils.RideSelectionScreen.ComponentConfig where

import Common.Types.App

import Common.Types.App (LazyCheck(..))
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..), Accessiblity(..))
import Storage as Storage
import Styles.Colors as Color
import Data.Maybe 
import Screens.RideSelectionScreen.ScreenData
import MerchantConfig.DefaultConfig as DC
import Mobility.Prelude

apiErrorModalConfig :: RideSelectionScreenState -> ErrorModal.Config 
apiErrorModalConfig state = let 
  config = ErrorModal.config 
  errorModalConfig' = config 
    { imageConfig {
        imageUrl = fetchImage FF_ASSET "ny_ic_error_404"
      , height = V 110
      , width = V 124
      , margin = MarginBottom 32
      }
    , errorConfig {
        text = getString ERROR_404
      , margin = MarginBottom 7
      , color = Color.black800
      }
    , errorDescriptionConfig {
        text = getString PROBLEM_AT_OUR_END
      , color = Color.black700
      , margin = MarginHorizontal 16 16
      }
    , buttonConfig {
        text = getString NOTIFY_ME
      , margin = Margin 16 0 16 16
      , background = DC.config.primaryBackground
      , color =  DC.config.primaryTextColor
      }
    }
  in errorModalConfig' 

errorModalConfig :: RideSelectionScreenState -> ErrorModal.Config 
errorModalConfig state = let 
  config = ErrorModal.config 
  errorModalConfig' = config 
    { imageConfig {
        imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_no_past_rides"
      , height = V 110
      , width = V 124
      , margin = MarginBottom 32
      }
    , errorConfig {
        text = getString EMPTY_RIDES
      , margin = MarginBottom 7
      , color = Color.black800
      }
    , errorDescriptionConfig {
        text = getString errorMessage
      , color = Color.black700
      }
    , buttonConfig {
       visibility = GONE
      }
    }
  in errorModalConfig' 
  where
    errorMessage = 
      case state.selectedCategory.maxAllowedRideAge of
      Just maxAllowedRideAge -> YOU_HAVENT_TAKEN_A_TRIP_YET_IN_PAST_HOURS (show $ maxAllowedRideAge/3600)
      Nothing -> YOU_HAVENT_TAKEN_A_TRIP_YET

genericHeaderConfig :: RideSelectionScreenState -> GenericHeader.Config 
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , margin = Margin 8 8 8 8
      , layoutMargin = Margin 4 4 4 4
      , enableRipple = true
      , accessibility = ENABLE
      , accessibilityHint = "Back Button"
      } 
    , textConfig {
        text = getString REPORT_AN_ISSUE
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

cancelButtonConfig :: RideSelectionScreenState -> PrimaryButton.Config
cancelButtonConfig _ = let
  config = PrimaryButton.config
  primaryButtonConfig' = config
    { textConfig
      { 
        textFromHtml = Just $ "<u>"  <> (getString I_DONT_KNOW_WHICH_RIDE) <> "</u>"
      , color = Color.black700
      , id = "cancelButtonTextConfig"
      }
    , background = Color.grey700
    , height     = V 60
    , isClickable  = true
    , cornerRadius= 0.0
    , margin = Margin 0 0 0 0
    , id = "cancelButtonConfig"
    , visibility = boolToVisibility false
    }
  in primaryButtonConfig'