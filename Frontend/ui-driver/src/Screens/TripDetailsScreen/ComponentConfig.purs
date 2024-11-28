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
import Language.Strings
import PrestoDOM
import Prelude
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination as SourceToDestination
import Data.Maybe (Maybe(..), isJust)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Types (STR(..))
import Prelude ((<>), ($))
import Screens.Types as ST
import Styles.Colors as Color
import Engineering.Helpers.Commons as EHC
import Debug
import Data.Array as DA 

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
      , imageUrl = fetchImage FF_ASSET "ny_ic_back"
      , padding = (Padding 5 5 5 5 )
      , visibility = if state.props.issueReported then GONE else VISIBLE
      }
    , textConfig {
        text = if state.props.issueReported then "" else (getString RIDE_DETAILS)
      , color = Color.black
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
  formatTimeAndLocation maybeTime location = case maybeTime of
    Just time -> ((EHC.convertUTCtoISC time "D MMM") <> " • " <> (EHC.convertUTCtoISC time "h:mm A")) <> "\n" <> location
    Nothing -> location
  source =  formatTimeAndLocation state.data.tripStartTime state.data.source
  destination =  formatTimeAndLocation state.data.tripEndTime state.data.destination
  tripType  =  state.data.tripType
  sourceToDestinationConfig' = config
    {
      margin = (Margin 0 13 0 0)
    , id = Just $ "TripDetailsScreenSTDC_" <> state.data.tripId
    , sourceImageConfig {
        imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_green_circle"
      , margin = (MarginTop 3)
      }
    , sourceTextConfig {
        text = source
      , padding = (Padding 2 0 2 2)
      , margin = (Margin 12 0 15 0)
      }
    , destinationImageConfig {
        imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_red_circle"
      , margin = (MarginTop 3)
      }
    , destinationBackground = Color.blue600
    , destinationTextConfig {
        text = destination
      , padding = (Padding 2 0 2 2)
      , margin = (Margin 12 0 15 0)
      , ellipsize = false
      }
    , overrideSeparatorCount = 6
    , showDestination = tripType /= ST.Rental
    , stops = state.data.stops
    , stopsImageConfig {
      imageUrl = fetchImage COMMON_ASSET "ny_ic_stop_grey"
    }
    , showSourceDestWithStops = true
    }
  in sourceToDestinationConfig'

---------------- primaryButtonConfig ----------------
primaryButtonConfig :: ST.TripDetailsScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = if state.props.issueReported then (getString GO_HOME) else (getString SUBMIT) }
      , width = MATCH_PARENT
      , margin = (Margin 0 0 0 0 )
      , cornerRadius = 0.0
      , background = Color.black900
      , height = (V 64)
      , id = "TripDetailsScreenPrimaryButton"
      }
  in primaryButtonConfig'