{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RentalBookingFlow.RideScheduledScreen.ComponentConfig where

import Components.GenericHeader.Controller as GenericHeader
import Components.PrimaryButton.Controller as PrimaryButton
import Components.SeparatorView.View as SeparatorView
import Components.SourceToDestination.Controller as SourceToDestination
import Data.Maybe (maybe)
import Font.Style (Style(..))
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..))
import Screens.Types (RideScheduledScreenState)
import Styles.Colors as Color

primaryButtonConfig :: RideScheduledScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
          { text = state.primaryButtonText
          , color = Color.yellow900
          , height = V 40
          }
      , gravity = CENTER
      , margin = (MarginHorizontal 16 16)
      , isClickable = true
      , id = "GoHomeButton"
      }
  in
    primaryButtonConfig'

separatorConfig :: SeparatorView.Config
separatorConfig =
  { orientation: VERTICAL
  , count: 3
  , height: V 4
  , width: V 2
  , layoutWidth: V 15
  , layoutHeight: V 15
  , color : Color.grey900
  }

sourceToDestinationConfig :: RideScheduledScreenState -> SourceToDestination.Config
sourceToDestinationConfig state =
  let
    config = SourceToDestination.config
    sourceToDestinationConfig' = config
      { sourceTextConfig
          { text = state.source
          , textStyle = ParagraphText
          , ellipsize = true
          , maxLines = 2
          , margin = MarginLeft 12
          , color = Color.black800
          }
      , sourceImageConfig
          { imageUrl = "ny_ic_pickup"
          , margin = MarginTop 7
          , height = V 16
          , width = V 16
          }
      , destinationImageConfig
          { imageUrl = maybe ("ny_ic_plus_circle") (\_ -> "ny_ic_drop") state.destination
          , margin = MarginTop 5
          , height = V 16
          , width = V 16
          }
      , destinationTextConfig
          { text = maybe (getString ADD_FIRST_STOP) (\dest -> dest) state.destination
          , color = maybe (Color.blue800) (\_ -> Color.black800) state.destination
          , isEditable = true
          , textStyle = ParagraphText
          , ellipsize = true
          , maxLines = 1
          , margin = MarginLeft 12
          , isClickable = true
          }
      , distanceConfig { distanceVisibility = GONE }
      , separatorMargin = 24
      }
  in
    sourceToDestinationConfig'

genericHeaderConfig :: RideScheduledScreenState -> GenericHeader.Config
genericHeaderConfig _ = let
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , width = WRAP_CONTENT
    , background = Color.white900
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , margin = (Margin 12 12 12 12)
      }
    , textConfig {
        text = "Rental Ride"
      , color = Color.black800
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , padding = (Padding 0 5 0 0)
    }
  in genericHeaderConfig'