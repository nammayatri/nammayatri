{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerUtils.FaqScreen.ComponentConfig where

import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..), PrestoDOM, linearLayout, height, width, textView, text, textFromHtml, color, margin)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App
import Prelude
import Data.Maybe (Maybe(..))
import Components.PrimaryButton as PrimaryButton
import Screens.FaqScreen.Transformer 
import Screens.FaqScreen.ScreenData
import Helpers.Utils (fetchImage, FetchImageFrom(..), isParentView, showTitle)
import Components.DropDownCard as DropDownCard
import Components.PrimaryButton as PrimaryButton
import Screens.FaqScreen.Controller (Action(..), eval)
import Effect (Effect)

genericHeaderConfig :: ST.FaqScreenState -> GenericHeader.Config 
genericHeaderConfig state = let 
  config = if state.data.config.nyBrandingVisibility then GenericHeader.merchantConfig else GenericHeader.config
  btnVisibility =  config.prefixImageConfig.visibility
  titleVisibility = if showTitle FunctionCall then config.visibility else GONE
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , visibility =  btnVisibility
      , margin = Margin 8 8 8 8 
      , layoutMargin = Margin 4 4 4 4
      , enableRipple = true
      } 
    , textConfig {
        text = getString FAQ
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , visibility = titleVisibility
    }
  in genericHeaderConfig'

apiErrorModalConfig :: ST.FaqScreenState -> ErrorModal.Config
apiErrorModalConfig state = let
  config = ErrorModal.config
  errorModalConfig' = config
    { imageConfig {
        imageUrl = fetchImage FF_ASSET "ny_ic_error_404"
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

dropDownCardConfig ::  ST.FaqScreenState -> FaqCardDropDownInfo -> (forall w . PrestoDOM (Effect Unit ) w) -> DropDownCard.Config 
dropDownCardConfig state cardInfo dropDownCardDataa = 
  { isOpen : cardInfo.isExpanded,
    title : cardInfo.title,
    layout : dropDownCardDataa,
    openArrowImage : "ny_ic_chevron_up",
    closeArrowImage : "ny_ic_chevron_down", 
    id : cardInfo.id,
    titleBackground : if cardInfo.isExpanded then Color.catskillWhite else Color.white900,
    cardMargin : Margin 12 4 12 4,
    cardPadding : Padding 0 0 0 0,
    headingPadding : Padding 16 16 16 16,
    imageHeight : V 16,
    imageWidth : V 16,
    headingCornerRadius : 16.0
  } 
  
primaryButtonConfig :: ST.FaqScreenState -> Maybe String -> PrimaryButton.Config
primaryButtonConfig state messageAction = let
  config = PrimaryButton.config
  primaryButtonConfig' = config
    { textConfig
      { text = case messageAction of 
                  Just action -> action
                  Nothing -> "Raise a Ticket"
      , color = Color.yellow900
      , accessibilityHint = "Raise a Ticket : Button"
      }
    , id = "RaiseTicket"
    , background = Color.black900
    , margin = Margin 0 16 0 0
    , enableRipple = true
    , rippleColor = Color.rippleShade
    }
  in primaryButtonConfig'