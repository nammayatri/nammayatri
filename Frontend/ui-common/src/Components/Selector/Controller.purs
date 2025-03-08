    

module Components.Selector.Controller where 

import Prelude
import Styles.Colors as Color
import Font.Style as FontStyle
import Common.Types.App (LazyCheck(..))
import Prelude
import PrestoDOM 
import Common.Types.App (TicketType(..))

instance showAction :: Show Action where
  show (OnClick _) = "OnClick"
  show (NoAction) = "NoAction"

data Action = OnClick TicketType | NoAction

type BaseConfig =  {
    baseHeight :: Length,
    baseGravity :: Gravity,
    basePadding :: Padding,
    baseCornerRadius :: Number,
    baseWidth :: Length,
    baseBackground :: String,
    items :: Array ItemConfig,
    id :: String
}

type ItemConfig = {
    itemHeight :: Length,
    itemGravity :: Gravity,
    itemFontColor :: String,
    itemBackground :: String,
    itemText :: String,
    itemCornerRadius :: Number,
    id :: String,
    itemPadding :: Padding,
    itemTripType :: TicketType,
    itemAccessibilty :: String,
    itemMargin :: Margin,
    itemFontStyle :: FontStyle.Style
}
