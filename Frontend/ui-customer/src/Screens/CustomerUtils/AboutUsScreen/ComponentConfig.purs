module Screens.CustomerUtils.AboutUsScreen.ComponentConfig where

import Components.GenericHeader as GenericHeader
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM ( Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App

genericHeaderConfig :: ST.AboutUsScreenState -> GenericHeader.Config
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
      , margin = (Margin 12 12 12 12)
      } 
    , padding = (Padding 0 5 0 5)
    , textConfig {
        text = (getString ABOUT)
      , textSize = FontSize.a_18
      , color = Color.darkDescriptionText
      , fontStyle = FontStyle.semiBold LanguageStyle
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'
