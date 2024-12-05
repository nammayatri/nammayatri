{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TicketBookingFlow.BusTrackingScreen.ComponentConfig where

import Components.PrimaryButton as PrimaryButton
import Data.Array (find)
import Data.Maybe (Maybe(..), fromMaybe)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Prelude (map, not, ($), (<>), (==), (&&), (/=))
import PrestoDOM 
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Styles.Colors as Color

primaryButtonConfig :: ST.BusTrackingScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config


    primaryButtonConfig' =
      config
        { textConfig { text = getString BOOK_TICKET }
        , width = MATCH_PARENT
        , cornerRadius = 12.0
        , background = Color.black900
        , margin = (Margin 16 16 16 16)
        , id = "BusTrackingScreenPrimaryButton"
        }
  in
    primaryButtonConfig'