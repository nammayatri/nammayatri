module Components.DateTimeSelector.Controller where

import Prelude
import Prelude
import Styles.Colors as Color
import Font.Style as FontStyle
import Common.Types.App (LazyCheck(..))
import Prelude
import PrestoDOM 

data Action = OnClick String | NoAction 

type DateSelectorConfig = {
  baseWidth :: Length,
  baseHeight :: Length,
  baseOrientation :: Orientation,
  baseMargin :: Margin,
  titleConfig :: String,
  textColor :: String,
  textMargin :: Margin,
  pickerHeight :: Length,
  pickerWidth :: Length,
  pickerCornerRadius :: Number,
  pickerBackground :: String,
  pickerPadding :: Padding,
  selectDateText :: String,
  dateHeight :: Length,
  dateWidth :: Length,
  dateColor :: String,
  iconHeight :: Length,
  iconWidth :: Length,
  iconMargin :: Margin,
  iconGravity :: Gravity
}
