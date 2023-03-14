module Screens.HelpAndSupportScreen.ComponentConfig where

import Common.Types.App
import Components.SourceToDestination as SourceToDestination
import Font.Size as FontSize
import Font.Style as FontStyle
import PrestoDOM
import Screens.Types as ST

sourceToDestinationConfig :: ST.HelpAndSupportScreenState -> SourceToDestination.Config
sourceToDestinationConfig state =
 SourceToDestination.config
    {
      margin = (Margin 0 13 0 0)
    , sourceMargin = (Margin 0 0 0 14)
    , sourceImageConfig {
        imageUrl = "ny_ic_green_circle,https://assets.juspay.in/nammayatri/images/common/ny_ic_green_circle.png"
      , margin = (MarginTop 3)
      }
    , sourceTextConfig {
        text = state.data.source
      , textSize = FontSize.a_15
      , padding = (Padding 2 0 2 2)
      , margin = (Margin 7 0 15 0)
      , ellipsize = true
      , maxLines = 1
      , fontStyle = FontStyle.medium LanguageStyle
      }
    , destinationImageConfig {
        imageUrl = "ny_ic_red_circle,https://assets.juspay.in/nammayatri/images/common/ny_ic_red_circle.png"
      , margin = (MarginTop 3)
      }
    , destinationTextConfig {
        text = state.data.destination
      , textSize = FontSize.a_15
      , padding = (Padding 2 0 2 2)
      , margin = (Margin 7 0 15 0)
      , maxLines = 1
      , ellipsize = true
      , fontStyle = FontStyle.medium LanguageStyle
      }
    }