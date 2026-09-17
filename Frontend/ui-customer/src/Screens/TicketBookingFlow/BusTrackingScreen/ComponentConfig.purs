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
import Data.Array (find, elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Prelude (map, not, ($), (<>), (==), (&&), (/=))
import PrestoDOM 
import PrestoDOM.Types.DomAttributes (Corners(..))
import Components.PopUpModal as PopUpModal
import Screens.Types as ST
import Styles.Colors as Color
import Common.RemoteConfig.Utils as RU
import Common.Types.App as CT

primaryButtonConfig :: ST.BusTrackingScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config
    wmbFlowConfig = RU.fetchWmbFlowConfig CT.FunctionCall
    isTicketBookingEnabled = wmbFlowConfig.enableTicketBooking
    isTicketBookingDisabledInRoute = elem state.data.busRouteCode wmbFlowConfig.disableTicketBookingInRoutes
    buttonText = case isTicketBookingEnabled, isTicketBookingDisabledInRoute of
                  true, false -> getString BOOK_TICKET
                  _, _ ->  getString GO_HOME_
    primaryButtonConfig' =
      config
        { textConfig { text = buttonText }
        , width = MATCH_PARENT
        , cornerRadius = 12.0
        , background = Color.black900
        , margin = (Margin 16 16 16 16)
        , id = "BusTrackingScreenPrimaryButton"
        }
  in
    primaryButtonConfig'

noBusPopUpModelConfig :: PopUpModal.Config
noBusPopUpModelConfig =
  let
    config = PopUpModal.config

    popUpModalConfig' =
      config
        { primaryText { text = "No incoming bus found!" }
        , secondaryText { text = "Do you still want to book a ticket?" }
        , optionButtonOrientation = "VERTICAL"
        , backgroundClickable = false
        , option1
            { text = "Yes, Continue"
            , enableRipple = true
            , background = Color.black900
            , color = Color.yellow900
            , margin = (Margin 16 0 16 0)
            , width = MATCH_PARENT
            }
        , option2
            { text = "Cancel"
            , enableRipple = true
            , background = Color.white900
            , color = Color.black600
            , margin = (Margin 16 0 16 0)
            , width = MATCH_PARENT
            , strokeColor = Color.white900
            }
        }
  in
    popUpModalConfig'