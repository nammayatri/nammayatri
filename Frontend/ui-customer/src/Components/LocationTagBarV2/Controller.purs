{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.LocationTagBarV2.Controller where

import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM (Length(..), Padding(..))
import Styles.Types (FontStyle(..), FontSize(..))
import Halogen.VDom.DOM.Prop (Prop)
import Common.Types.App (LazyCheck(..))
import Font.Size as FontSize
import Font.Style (Style(..))

data Action = NoAction 
            | TagClicked String

type LocationTagBarConfig = {
  tagList :: Array TagConfig
}

type TagConfig = {
  imageConfig :: ImageConfig ,
  textConfig :: TextConfig,
  stroke :: String,
  cornerRadius :: Corners,
  background :: String,
  height :: Length,
  width :: Length,
  padding :: Padding,
  id :: String}

type ImageConfig = {
  width :: Length,
  height :: Length,
  imageWithFallback :: String
}

type TextConfig = {
  text :: String ,
  fontStyle :: Style,
  fontSize :: FontSize,
  color :: String
}

config :: LocationTagBarConfig 
config = {
  tagList : []
}
