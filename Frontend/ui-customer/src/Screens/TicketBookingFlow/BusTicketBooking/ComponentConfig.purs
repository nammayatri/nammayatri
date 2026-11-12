{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TicketBookingFlow.BusTicketBooking.ComponentConfig where

import Data.String.CodeUnits (slice)
import Data.Array as DA
import Data.String as DS
import Common.Types.App as CTA
import Components.ChooseVehicle.Controller as ChooseVehicle
import Components.GenericHeader.Controller as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton.Controller as PrimaryButton
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.RateCard.Controller as RateCard
import Components.SeparatorView.View as SeparatorView
import Components.SelectListModal as CancelRidePopUpConfig
import Components.SourceToDestination.Controller as SourceToDestination
import ConfigProvider
import Data.Maybe (Maybe(..), isJust, maybe, fromMaybe)
import Font.Style (Style(..))
import Helpers.Utils as HU
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Services.API as API
import Styles.Colors as Color

genericHeaderConfig :: ST.BusTicketBookingState -> GenericHeader.Config
genericHeaderConfig state = let
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , width = WRAP_CONTENT
    , background = Color.black900
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_chevron_left"
      , margin = (Margin 12 12 12 12)
      }
    , suffixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_ticket_icon_yellow"
      }
    , padding = (Padding 0 5 0 0)
    }
  in genericHeaderConfig'

sourceToDestinationConfig :: ST.BusTicketBookingState -> SourceToDestination.Config
sourceToDestinationConfig state =
  let
    config = SourceToDestination.config
    sourceToDestinationConfig' =
      config
        { sourceImageConfig
          { imageUrl = HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_green_circle"
          , margin = MarginTop 3
          , width = V 8
          , height = V 8
          }
        , sourceTextConfig
          { text = "Howrah Station"
          , margin = MarginHorizontal 16 15
          , color = Color.black650
          , ellipsize = true
          , maxLines = 1
          , textStyle = Body1
          }
        , destinationImageConfig
          { imageUrl = HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_red_circle"
          , margin = MarginTop 3
          , width = V 8
          , height = V 8
          }
        , destinationTextConfig
          { text = "Sealdah"
          , margin = MarginHorizontal 16 15
          , color = Color.black650
          , ellipsize = true
          , maxLines = 1
          , textStyle = Body1
          }
        }
  in
    sourceToDestinationConfig'