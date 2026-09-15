{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SelectContactsFlow.SelectContactsScreen.ComponentConfig where

import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Data.Array (length, null, difference, filter)
import Data.Show (show)
import Data.Maybe (Maybe(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (not, (<>), (==), ($), (&&), (>), (<), (||))
import PrestoDOM (Length(..), Margin(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, height, margin, padding, text, textSize, width, imageUrl, visibility, stroke)
import Styles.Colors as Color
import Components.PrimaryButton.Controller as PrimaryButtonConfig
import Screens.SelectContactsFlow.SelectContactsScreen.ScreenData (SelectContactsScreenState)
import Screens.Types (NewContacts)

genericHeaderConfig :: SelectContactsScreenState -> GenericHeader.Config
genericHeaderConfig state =
  let
    config = GenericHeader.config
    titleText = case null state.data.contacts, state.props.showContacts of 
                    _, true -> show (length state.data.selectedContacts) <> "/" <> (show state.data.contactSelectionLimit) <> " " <> (getString CONTACTS_SELECTED)
                    true, false -> getString TRUSTED_CONTACT
                    false, false -> getString TRUSTED_CONTACT
    genericHeaderConfig' =
      config
        { height = WRAP_CONTENT
        , prefixImageConfig
          { height = V 25
          , width = V 25
          , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
          , margin = Margin 8 8 8 8 
          , layoutMargin = Margin 4 4 4 4
          , enableRipple = true
          }
        , padding = (Padding 0 5 0 5)
        , textConfig
          { text = titleText
          , accessibilityHint = (show (length state.data.contacts) <> " Of " <> (show state.data.contactSelectionLimit) <> " " <> (getString CONTACTS_SELECTED))
          , color = Color.darkCharcoal
          }
        , suffixImageConfig
          { visibility = GONE
          }
        }
  in
    genericHeaderConfig'

contactListPrimaryButtonConfig :: SelectContactsScreenState -> PrimaryButtonConfig.Config
contactListPrimaryButtonConfig state =
  let
    config' = PrimaryButtonConfig.config
    uniqueContacts = length $ contactDifference state.data.selectedContacts state.data.alreadySelectedContacts
    enableBtn = if (uniqueContacts == 0) && (length state.data.selectedContacts) < (length state.data.alreadySelectedContacts) then true else uniqueContacts > 0

    primaryButtonConfig' =
      config'
        { textConfig
          { text = if enableBtn then (getString CONFIRM_CONTACTS) else (getString SELECT_CONTACTS)
          , accessibilityHint = (if enableBtn then (getString CONFIRM_CONTACTS) else (getString SELECT_CONTACTS)) <> " : Button"
          }
        , isClickable = if enableBtn then true else false
        , id = "ContactListPrimaryButton"
        , enableRipple = if enableBtn then true else false
        , margin = (MarginBottom 0)
        , alpha = if enableBtn then 1.0 else 0.6
        }
  in
    primaryButtonConfig'

contactDifference :: Array NewContacts -> Array NewContacts -> Array NewContacts
contactDifference array1 array2 = 
  filter (\a1 -> length (filter (\a2 -> a1.name == a2.name && a1.number == a2.number) array2) == 0) array1
