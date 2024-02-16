{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Screens.DriverSavedLocationScreen.ComponentConfig where

import Data.Maybe

import Common.Styles.Colors as Color
import Components.GoToLocationModal as GoToLocationModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Data.String (length)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((<>), (==), (<), (||), ($), (&&), not)
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types as ST

primaryButtonConfig :: ST.DriverSavedLocationScreenState -> PrimaryButton.Config
primaryButtonConfig state = PrimaryButton.config { 
  textConfig
    { text =
      case state.props.viewType of
        ST.GoToList -> if DA.null state.data.savedLocationsArray then (getString ADD_LOCATION) else getString ADD_ANOTHER_LOCATION
        ST.SearchLocation ->  getString SELECT_ON_MAP
        ST.LOCATE_ON_MAP -> getString CONFIRM_LOCATION_STR
        ST.ConfirmLocation -> getString SAVE_LOCATION_STR
    }
  , margin = MarginTop 15
  , height = V 52
  , visibility = if (DA.length state.data.savedLocationsArray < 5 || state.props.viewType == ST.ConfirmLocation) then VISIBLE else GONE 
  , alpha = if disabled then 0.7 else 1.0
  , isClickable = not disabled
  , enableRipple = not disabled
  }
  where disabled = state.props.viewType == ST.ConfirmLocation && (length state.data.saveLocationObject.tag < 1 || isJust state.props.errorText)

locationListItemConfig :: ST.GoToLocation -> GoToLocationModal.GoToModalConfig
locationListItemConfig state =
  GoToLocationModal.config
    { id = state.id
    , lat = state.lat
    , lon = state.lon
    , address = state.address
    , tag = state.tag
    , isSelectable = false
    , isEditEnabled = true
    , isSelected = false
    , removeAcText = Just $ getString REMOVE
    , editAcText = Nothing -- TODO :: need to do later
    }

confirmDeletePopupConfig :: ST.DriverSavedLocationScreenState -> PopUpModal.Config
confirmDeletePopupConfig state = PopUpModal.config
  { primaryText { text = getString REMOVE_PREF_LOC <> "'" <> state.props.selectedLocation.tag <> "'?"}
  , secondaryText { text = getString CONF_REMOVE_PREF_LOC}
  , option1 { text = getString CANCEL}
  , option2 { 
      text = getString YES_REMOVE
    , background = Color.red
    , color = Color.white900
    , strokeColor = Color.red
    }
  }
