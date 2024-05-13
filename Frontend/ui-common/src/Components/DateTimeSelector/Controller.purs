module Components.DateTimeSelector.Controller where

import Prelude
import Prelude
import Styles.Colors as Color
import Font.Style as FontStyle
import Common.Types.App (LazyCheck(..))
import Prelude
import PrestoDOM 
-- DateSelectorConfig

data Action = OnClick String | NoAction 

-- Define a data type for the configuration
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

--  baseWidth: MATCH_PARENT,
--   baseHeight: WRAP_CONTENT,
--   baseOrientation: VERTICAL,
--   baseMargin: MarginBottom 20,
--   titleConfig: "Return", -- strings needed here 
--   textColor: Color.black900,
--   textMargin: MarginBottom 9,
--   pickerHeight: WRAP_CONTENT,
--   pickerWidth: MATCH_PARENT,
--   pickerCornerRadius: 8.0,
--   pickerBackground: Color.white900,
--   pickerPadding: Padding 20 15 20 15,
--   selectDateText: case state.data.tripTypeDataConfig.tripReturnData of 
--                   Just obj -> obj.tripDateReadableString
--                   Nothing -> "Enter Return Location"
--                    -- date Entry here 
--   , dateHeight: WRAP_CONTENT,
--   dateWidth: WRAP_CONTENT,
--   dateColor: Color.black800,
--   iconHeight: V 22,
--   iconWidth: V 22,
--   iconMargin: MarginLeft 8,
--   iconGravity: BOTTOM


