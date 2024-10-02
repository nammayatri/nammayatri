{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.Selector.View where  

import Components.Selector.Controller
import Data.Array
import Prelude

import Common.Types.App (LazyCheck(..))
import Components.Selector.Controller (Action(..))
import Data.String (take)
import Effect (Effect)
import Font.Style as FontStyle
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..),Accessiblity(..), accessibility ,accessibilityHint ,background, color, fontStyle, gravity, height, linearLayout, margin, orientation, padding, text, textSize, textView, weight, width, onClick, alpha, cornerRadius, id, visibility, stroke)
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> BaseConfig -> PrestoDOM (Effect Unit) w
view push config=
  linearLayout
    [ height config.baseHeight
    , width config.baseWidth
    , cornerRadius config.baseCornerRadius
    , margin $ Margin 6 6 6 6
    , background config.baseBackground
    , padding $ config.basePadding
    , gravity config.baseGravity
    ](map (\itemConfig -> 
         textView $ [
          height itemConfig.itemHeight
        , weight 1.0
        , background itemConfig.itemBackground
        , color itemConfig.itemFontColor
        , text $ itemConfig.itemText
        , cornerRadius itemConfig.itemCornerRadius
        , gravity itemConfig.itemGravity
        , padding $ itemConfig.itemPadding
        , onClick push $ const $ OnClick itemConfig.itemTripType
        , accessibility ENABLE
        , margin $ itemConfig.itemMargin
        , accessibilityHint $ itemConfig.itemAccessibilty
        ] <> FontStyle.getFontStyle itemConfig.itemFontStyle LanguageStyle
      ) 
    config.items
    )
