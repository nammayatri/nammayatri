{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.OptionsMenu.View where

import Common.Types.App

import Animation as Anim
import Components.OptionsMenu.Controller (Action(..), Config, MenuItemData)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Show (show)
import Data.String (length)
import Debug (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, map, pure, unit, ($), (/=), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alpha, background, clickable, color, cornerRadius, fontStyle, frameLayout, gravity, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, scrollView, stroke, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Styles.Colors as Color

view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
  linearLayout[
    width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.transparent
  ][
    linearLayout[
      background config.backgroundColor
    , visibility if config.menuExpanded then VISIBLE else GONE
      , height MATCH_PARENT
      , width MATCH_PARENT
      , gravity config.gravity
      , onClick push $ const BackgroundClick
    ][
      linearLayout[
        background config.menuBackgroundColor
      , orientation VERTICAL
      , height WRAP_CONTENT
      , width $ V config.width
      , margin $ MarginRight config.marginRight
      , cornerRadius config.cornerRadius
      ](map (\item -> 
                linearLayout[
                  height WRAP_CONTENT
                  , width MATCH_PARENT
                  , gravity CENTER_VERTICAL
                  , orientation VERTICAL
                  , onClick push $ const $ ItemClick item.action
                  , visibility if item.isVisible then VISIBLE else GONE
                ][
                  linearLayout[
                    gravity CENTER_VERTICAL
                    , padding $ Padding config.itemPadding config.itemPadding config.itemPadding config.itemPadding
                    , height WRAP_CONTENT
                  ][
                    imageView [
                      imageWithFallback item.image
                      , margin $ MarginRight 8
                      , visibility if length item.image /= 0 then VISIBLE else GONE
                      , height $ V 16
                      , width $ V 16
                    ]
                    , textView $ [
                      text item.textdata
                      , margin $ MarginBottom 1
                      , color Color.black800
                    ] <> FontStyle.paragraphText TypoGraphy
                  ]
                  , linearLayout [
                      width MATCH_PARENT
                      , height $ V 2
                      , background Color.grey700
                      , margin $ MarginHorizontal 16 16
                    ][]
                ]) config.menuItems)
    ]
  ]