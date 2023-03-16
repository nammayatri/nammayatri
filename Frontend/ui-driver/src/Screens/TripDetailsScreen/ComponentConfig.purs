{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TripDetailsScreen.ComponentConfig where

import Common.Types.App
import Components.PrimaryButton as PrimaryButton
import Components.GenericHeader as GenericHeader
import Components.SourceToDestination as SourceToDestination
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings
import Language.Types (STR(..))
import PrestoDOM
import Screens.Types as ST
import Styles.Colors as Color

---------------- genericHeaderConfig ----------------
genericHeaderConfig :: ST.TripDetailsScreenState -> GenericHeader.Config 
genericHeaderConfig state= let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = (V 30)
      , width = (V 30)
      , margin = (Margin 16 16 25 16)
      , imageUrl = "ny_ic_back,https://assets.juspay.in/nammayatri/images/driver/ny_ic_back.png"
      , padding = (Padding 5 5 5 5 )
      , visibility = if state.props.issueReported then GONE else VISIBLE
      }
    , textConfig {
        text = if state.props.issueReported then "" else (getString TRIP_DETAILS)
      , textSize = FontSize.a_20
      , color = Color.black
      , fontStyle = FontStyle.semiBold LanguageStyle
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

---------------- sourceAndDestinationConfig ----------------
sourceToDestinationConfig :: ST.TripDetailsScreenState -> SourceToDestination.Config
sourceToDestinationConfig state = let 
  config = SourceToDestination.config
  sourceToDestinationConfig' = config
    {
      margin = (Margin 0 13 0 0)
    , sourceMargin = (Margin 0 10 0 25)
    , lineMargin = (Margin 4 12 0 0)
    , sourceImageConfig {
        imageUrl = "ny_ic_green_circle,https://assets.juspay.in/nammayatri/images/common/ny_ic_green_circle.png"
      , margin = (MarginTop 3)
      }
    , sourceTextConfig {
        text = state.data.source
      , textSize = FontSize.a_15
      , padding = (Padding 2 0 2 2)
      , margin = (Margin 12 0 15 0)
      , ellipsize = false
      , maxLines = 2
      }
    , destinationImageConfig {
        imageUrl = "ny_ic_red_circle,https://assets.juspay.in/nammayatri/images/common/ny_ic_red_circle.png"
      , margin = (MarginTop 3)
      }
    , destinationBackground = Color.blue600
    , destinationTextConfig {
        text = state.data.destination
      , textSize = 15
      , padding = (Padding 2 0 2 2)
      , margin = (Margin 12 0 15 0)
      , ellipsize = false
      , maxLines = 2
      }
    }
  in sourceToDestinationConfig'

---------------- primaryButtonConfig ----------------
primaryButtonConfig :: ST.TripDetailsScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = if state.props.issueReported then (getString GO_HOME) else (getString SUBMIT), textSize = FontSize.a_18 }
      , width = MATCH_PARENT
      , margin = (Margin 0 0 0 0 )
      , cornerRadius = 0.0
      , background = Color.black900
      , height = (V 64)
      }
  in primaryButtonConfig'