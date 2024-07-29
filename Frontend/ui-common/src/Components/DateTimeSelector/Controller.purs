module Components.DateTimeSelector.Controller where

import Prelude
import Prelude
import Styles.Colors as Color
import Font.Style as FontStyle
import Common.Types.App (LazyCheck(..))
import Prelude
import PrestoDOM 
import Components.MenuButton as MenuButton
import Components.MenuButton.Controller as MenuButtonActionController

data Action = OnClick String | NoAction | MenuButtonActionController MenuButtonActionController.Action

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
  iconGravity :: Gravity,
  id :: String,
  radioButtonViewVisibilty :: Boolean,
  returnTextViewVisibilty :: Boolean,
  isEnabled :: Boolean
}
