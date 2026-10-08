{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.InfoBox.Controller where

import Data.Array
import Data.Maybe
import Data.String
import Prelude
import Engineering.Helpers.Commons
import Font.Style (Style(..))
import Halogen.VDom.DOM.Prop (PropValue)
import PrestoDOM (Length(..), Margin(..), Visibility(..), Padding(..), Prop, toPropValue)
import PrestoDOM.List (ListItem)
import Screens.Types (Gender)
import Styles.Colors as Colors
import Common.RemoteConfig (RCCarousel(..))
import Data.String as DS
import Common.RemoteConfig.Types
import Styles.Types (Color)

instance showAction :: Show Action where
  show (OnClick) = "OnClick"
  show (NoAction) = "NoAction"
  
data Action
  = OnClick
  | NoAction

type Config
  = { margin :: Margin
    , text :: String
    , icon :: String
    , visibility :: Visibility
    , subTitle :: String
    , backgroundColor :: Color
    , titleColor :: Color
    , disabled :: Boolean
    }

config :: Config
config =
  { margin: Margin 16 16 16 16
  , text: ""
  , icon: ""
  , visibility: VISIBLE
  , subTitle: ""
  , backgroundColor: Colors.blue650
  , titleColor: Colors.black900
  , disabled: false
  }
