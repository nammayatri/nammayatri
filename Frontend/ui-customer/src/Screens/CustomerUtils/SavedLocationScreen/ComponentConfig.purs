{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerUtils.SavedLocationScreen.ComponentConfig where

import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PopUpModal as PopUpModal
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB 
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((<>))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App
import Data.Maybe 

requestDeletePopUp :: ST.SavedLocationScreenState -> PopUpModal.Config 
requestDeletePopUp state = let 
    config = PopUpModal.config
    popUpConfig' = config {
      primaryText { 
        text = (getString REMOVE_FAVOURITE) <> " '"<>(fromMaybe "" state.data.deleteTag )<> "' ? "
      , margin = (Margin 16 24 16 0)
      },
      secondaryText { 
        text = (getString ARE_YOU_SURE_YOU_WANT_TO_REMOVE_FAVOURITE_)
      , color = Color.black700
      , margin = (Margin 16 12 16 40)
        },
      option1 {
        text = (getString CANCEL_STR)
      , fontSize = FontSize.a_16 
      , color = Color.black700
      , strokeColor = Color.black700
      , fontStyle = FontStyle.semiBold LanguageStyle
      },
      option2 {text = (getString YES_REMOVE)
      , background = Color.red
      , color = Color.white900
      , strokeColor = Color.red
      , fontSize = FontSize.a_16 
      , margin = (MarginLeft 12)
      , fontStyle = FontStyle.semiBold LanguageStyle }
     
    }
  in popUpConfig'

primaryButtonConfig :: ST.SavedLocationScreenState -> PrimaryButton.Config 
primaryButtonConfig state = let 
    config' = PrimaryButton.config 
    primaryButtonConfig' = config'
      { textConfig
        {
          text = (getString ADD_NEW_FAVOURITE)
        , fontStyle = FontStyle.semiBold LanguageStyle
        }
      , margin = (Margin 16 15 16 24)
      , height = V 52
      , id = "AddNewAddressSavedLocationScreen"
      , enableLoader = (JB.getBtnLoader "AddNewAddressSavedLocationScreen")
      }
  in primaryButtonConfig'

genericHeaderConfig :: ST.SavedLocationScreenState -> GenericHeader.Config 
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , width = WRAP_CONTENT
    , background = Color.white900
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
      , margin = (Margin 12 12 12 12)
      } 
    , textConfig {
        text = (getString FAVOURITES)
      , textSize = FontSize.a_18
      , color = Color.black800
      , fontStyle = FontStyle.semiBold LanguageStyle
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , padding = (Padding 0 5 0 5)
    }
  in genericHeaderConfig'

errorModalConfig :: ST.SavedLocationScreenState ->  ErrorModal.Config 
errorModalConfig state = let 
  config = ErrorModal.config 
  errorModalConfig' = config 
    { imageConfig {
        imageUrl = "ny_ic_no_saved_address,https://assets.juspay.in/nammayatri/images/user/ny_ic_no_saved_address.png"
      , height = V 110
      , width = V 124
      , margin = (MarginBottom 31)
      }
    , errorConfig {
        text = (getString NO_FAVOURITES_SAVED_YET)
      , margin = (MarginBottom 7)  
      , color = Color.black900
      , textSize = FontSize.a_18
      , fontStyle = FontStyle.bold LanguageStyle
      }
    , errorDescriptionConfig {
        text = (getString SAVED_ADDRESS_HELPS_YOU_KEEP_YOUR_FAVOURITE_PLACES_HANDY)
      , color = Color.black700
      , textSize = FontSize.a_14
      , margin = (Margin 33 0 33 0)
      , padding = (Padding 16 0 16 16)
      , fontStyle =  FontStyle.regular LanguageStyle
      }
    , buttonConfig {
        text = (getString ADD_NEW_FAVOURITE)
      , margin = (Margin 16 0 16 24)
      , background = Color.black900
      , height = V 52
      , color = Color.yellow900
      , fontStyle = FontStyle.semiBold LanguageStyle
      , textSize = FontSize.a_16
      }
    }
  in errorModalConfig' 