{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.DropDownWithHeader.Controller where

import Data.Array
import Data.Maybe
import Data.String
import Prelude
import Engineering.Helpers.Commons
import Font.Style (Style(..))
import Halogen.VDom.DOM.Prop (PropValue)
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..), Prop, toPropValue)
import PrestoDOM.List (ListItem)
import Screens.Types (NewContacts, DropDownOptions)
import Common.RemoteConfig (RCCarousel(..))
import Data.String as DS
import Common.RemoteConfig.Types
import Common.Styles.Colors as Color
import Services.API as API
import Screens.EmergencyContactsScreen.ScreenData (alwaysShareRideOption, neverShareRideOption, shareWithTimeContraintsRideOption)

data Action
  = OnExpand NewContacts String
  | NoAction
  | OnSelect DropDownOptions String

type Config
  = { margin :: Margin
    , selectedValue :: DropDownOptions
    , dropDownOptions :: Array DropDownOptions
    , listVisibility :: Visibility
    , headerText :: String
    , boxPadding :: Padding

    , boxBackground :: String
    , selectedContact :: NewContacts
    , dropDownAction :: String
    }

config :: Config
config =
  { margin: MarginVertical 12 8
  , selectedValue: alwaysShareRideOption
  , dropDownOptions: [ alwaysShareRideOption, shareWithTimeContraintsRideOption ]
  , listVisibility: GONE
  , headerText: ""
  , boxPadding: Padding 16 16 16 16
  , boxBackground: Color.blue600
  , dropDownAction: ""
  , selectedContact:
      { name: ""
      , number: ""
      , isSelected: false
      , enableForFollowing: false
      , enableForShareRide: false
      , shareTripWithEmergencyContactOption: neverShareRideOption
      , onRide: false
      , priority: 0
      , contactPersonId: Nothing
      , isFollowing: Nothing
      , notifiedViaFCM : Nothing
      }
  }
