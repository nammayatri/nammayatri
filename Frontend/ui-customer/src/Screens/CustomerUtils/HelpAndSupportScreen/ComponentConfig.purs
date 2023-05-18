{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerUtils.HelpAndSupportScreen.ComponentConfig where

import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.SourceToDestination as SourceToDestination
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App
import Engineering.Helpers.Commons (os)
import Prelude
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)

sourceToDestinationConfig :: ST.HelpAndSupportScreenState -> SourceToDestination.Config
sourceToDestinationConfig state = let 
  config = SourceToDestination.config
  sourceToDestinationConfig' = config
    {
      margin = (Margin 0 13 16 0)
    , width = MATCH_PARENT
    , lineMargin = (Margin 4 6 0 0)
    , sourceMargin = (Margin 0 0 0 14)
    , sourceImageConfig {
        imageUrl = "ny_ic_green_circle," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_green_circle.png"
      , margin = (MarginTop 5)
      }
    , sourceTextConfig {
        text = state.data.source
      , padding = (Padding 0 0 0 0)
      , margin = (Margin 7 0 15 0)
      , color = Color.darkDescriptionText
      , textStyle = FontStyle.Body1
      , ellipsize = true
      , maxLines = 1
      }
    , destinationImageConfig {
        imageUrl = "ny_ic_red_circle," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_red_circle.png"
      , margin = (MarginTop 4)
      }
    , destinationTextConfig {
        text = state.data.destination
      , padding = (Padding 0 0 0 0)
      , margin = (Margin 7 0 15 0)
      , color = Color.darkDescriptionText
      , textStyle = FontStyle.Body1
      , maxLines = 1
      , ellipsize = true
      }
    }
  in sourceToDestinationConfig'

apiErrorModalConfig :: ST.HelpAndSupportScreenState -> ErrorModal.Config 
apiErrorModalConfig state = let 
  config = ErrorModal.config 
  errorModalConfig' = config 
    { imageConfig {
        imageUrl = "ny_ic_error_404," <> (getAssetStoreLink FunctionCall) <> "ny_ic_error_404.png"
      , height = V 110
      , width = V 124
      , margin = (MarginBottom 32)
      }
    , errorConfig {
        text = (getString ERROR_404)
      , margin = (MarginBottom 7)  
      , color = Color.black800
      }
    , errorDescriptionConfig {
        text = (getString PROBLEM_AT_OUR_END)
      , color = Color.black700
      , margin = (Margin 16 0 16 0)
      }
    , buttonConfig {
        text = (getString NOTIFY_ME)
      , margin = (Margin 16 0 16 16)
      , background = state.data.config.primaryBackground
      , color = state.data.config.primaryTextColor
      }
    }
  in errorModalConfig' 

callConfirmationPopup :: ST.HelpAndSupportScreenState -> PopUpModal.Config 
callConfirmationPopup state = let 
    config = PopUpModal.config
    popUpConfig' = config {
      padding = PaddingBottom if os =="IOS" then 24 else 0
      , primaryText { 
          text = (getString CONTACT_SUPPORT) 
      , margin = (Margin 0 20 0 20)
        },
      secondaryText { 
        visibility = GONE
        },
      option1 {
        text = (getString GO_BACK_)
      , strokeColor = state.data.config.primaryBackground
      , background = state.data.config.primaryTextColor
      , color = state.data.config.primaryBackground
      },
      option2 {
        text = (getString CALL)
      , strokeColor = state.data.config.primaryBackground
      , background = state.data.config.primaryBackground
      , color = state.data.config.primaryTextColor
      }
    }
  in popUpConfig'

genericHeaderConfig :: ST.HelpAndSupportScreenState -> GenericHeader.Config 
genericHeaderConfig state = let 
  config = if state.data.config.nyBrandingVisibility then GenericHeader.merchantConfig else GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_chevron_left," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_left.png"
      } 
    , textConfig {
        text = (getString HELP_AND_SUPPORT)
      , color = Color.darkDescriptionText
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'
  
