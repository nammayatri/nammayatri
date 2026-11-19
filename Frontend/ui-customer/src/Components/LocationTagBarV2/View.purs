{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.LocationTagBarV2.View where

import Components.LocationTagBarV2.Controller(Action(..), LocationTagBarConfig, TagConfig )
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM (PrestoDOM, Length(..), Padding(..), JustifyContent(..), FlexDirection(..), FlexWrap(..), AlignItems(..), Margin(..), Gravity(..), Visibility(..), Orientation (..), alignItems, linearLayout, height, width, background, stroke, cornerRadius, padding, imageView, imageWithFallback, textView, text, textSize, color, flexBoxLayout, flexDirection, justifyContent, flexWrap, margin, flexWrap, onClick, weight, gravity, rippleColor, orientation, visibility, singleLine, maxLines)
import PrestoDOM.Properties (cornerRadii)
import Engineering.Helpers.Commons (screenWidth)
import Prelude(Unit, map, unit, ($), (<>), (-), (==), const)
import Data.Array (mapWithIndex, length) as DA
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Common.Types.App (LazyCheck(..))
import Mobility.Prelude (boolToVisibility)
import Language.Strings (getString)
import Language.Types (STR(..))

view :: forall w. (Action -> Effect Unit) -> LocationTagBarConfig -> PrestoDOM (Effect Unit) w 
view push state = let 
  lastIndex = (DA.length state.tagList - 1)
  in
    linearLayout
      [ height WRAP_CONTENT
      , width $ V (screenWidth unit - 32)
      ]( DA.mapWithIndex 
          (\index item -> tagView item (index == lastIndex) push) 
          state.tagList
        )


tagView :: forall w. TagConfig -> Boolean -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
tagView item isLast push = 
  let 
    textConfig = item.textConfig
    imageConfig = item.imageConfig
    rightMargin = if isLast then 0 else 8
  in linearLayout
      ([ height item.height 
      , weight 1.0
      , background item.background
      , stroke item.stroke 
      , gravity CENTER
      , cornerRadii item.cornerRadius
      , onClick push $ const $ TagClicked item.id
      , padding item.padding
      , margin $ MarginRight rightMargin
      ] <> (if item.enableRipple then [rippleColor item.rippleColor] else []))[
        linearLayout[
          height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , orientation item.orientation
        ][
          linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , background item.bannerConfig.background
            , cornerRadii item.bannerConfig.cornerRadii
            , visibility $ item.showBanner
            , margin $ MarginHorizontal 8 8
            , gravity CENTER
            ][  textView $
                [ text item.bannerConfig.text
                , textSize item.bannerConfig.textSize 
                , color item.bannerConfig.color
                , padding $ Padding 4 0 4 2
                , singleLine true
                , maxLines 1
                ] <> (FontStyle.getFontStyle item.bannerConfig.fontStyle LanguageStyle)
              ]
        , imageView 
            [ height imageConfig.height
            , width imageConfig.width
            , imageWithFallback imageConfig.imageWithFallback
            , margin $ imageConfig.margin
            ]
        , textView $
            [ text textConfig.text
            , textSize textConfig.fontSize
            , color textConfig.color 
            , padding $ PaddingBottom 2
            ] <> (FontStyle.getFontStyle textConfig.fontStyle LanguageStyle)
      ]]