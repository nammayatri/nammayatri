{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.BoxContainer.Controller where

import Data.Array
import Data.Maybe
import Data.String as DS
import Prelude
import Engineering.Helpers.Commons
import Font.Style (Style(..))
import Halogen.VDom.DOM.Prop (PropValue)
import PrestoDOM (Length(..), Margin(..), Padding(..), Prop, Visibility(..), toPropValue)
import PrestoDOM.List (ListItem)
import Screens.Types (Gender)
import Styles.Colors as Color
import Common.RemoteConfig (RCCarousel(..))
import Data.String as DS
import Common.RemoteConfig.Types
import Components.InfoBox as InfoBox

instance showAction :: Show Action where
  show (OnClick _) = "OnClick"
  show (NoAction) = "NoAction"

data Action
  = OnClick String
  | NoAction

type Config
  = { title :: String
    , subTitle :: String
    , noteText :: String
    , noteImage :: String
    , toggleButton :: Boolean
    , buttonAction :: String
    , margin :: Margin
    }

config :: Config
config =
  { title: ""
  , subTitle: ""
  , noteText: ""
  , noteImage: ""
  , toggleButton: false
  , buttonAction: ""
  , margin: Margin 16 16 16 0
  }

infoBoxConfig :: Config -> InfoBox.Config
infoBoxConfig config =
  InfoBox.config
    { margin = MarginTop 16
    , text = config.noteText
    , icon = config.noteImage
    , visibility = if DS.null config.noteText then GONE else VISIBLE
    }
