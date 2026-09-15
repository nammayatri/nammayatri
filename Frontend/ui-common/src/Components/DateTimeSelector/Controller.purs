module Components.DateTimeSelector.Controller where

import Prelude
import Styles.Colors as Color
import Font.Style as FontStyle
import Common.Types.App (LazyCheck(..),DateTime(..))
import PrestoDOM 
import Components.MenuButton as MenuButton
import Components.MenuButton.Controller as MenuButtonActionController

instance showAction :: Show Action where
  show (OnClick _ _) = "OnClick"
  show (NoAction) = "NoAction"
  show (MenuButtonActionController var1) = "MenuButtonActionController_" <> show var1

data Action = OnClick String DateTime
            | NoAction
            | MenuButtonActionController MenuButtonActionController.Action

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
  isEnabled :: Boolean,
  radioButtonTextConfig :: RadioTextConfig,
  selectTimeText :: String,
  dateIconImage :: String,
  timeIconImage :: String
}

type RadioTextConfig = {
  primaryText :: String,
  primaryTextAccessibilityHint :: String,
  secondaryText :: String,
  secondaryTextAccessibilityHint :: String
}