    

module Components.Selector.Controller where 

import Prelude
import Styles.Colors as Color
import Font.Style as FontStyle
import Common.Types.App (LazyCheck(..))
import Prelude
import PrestoDOM 
import Screens.Types (TicketType)

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
    itemTripType :: TicketType
}

-- height WRAP_CONTENT
--         , weight 1.0
--         , background if state.props.screenType == ST.DRIVER_DETAILS then Color.black900 else Color.white900
--         , text (getString DRIVER_DETAILS)
--         , cornerRadius 24.0
--         , padding $ PaddingVertical 6 6
--         , onClick push $ const $ ChangeScreen ST.DRIVER_DETAILS
--         , fontStyle $ FontStyle.medium LanguageStyle
--         , gravity CENTER
--         , color if state.props.screenType == ST.DRIVER_DETAILS then Color.white900 else Color.black900

-- itemsArray :: Array ItemConfig 
-- itemsArray = [
--     { itemHeight: ( V 54 )
--     , itemGravity: CENTER_VERTICAL
--     , itemFontColor: Color.white900
--     , itemBackground: Color.black900
--     , itemText: "One_Way"
--     , itemCornerRadius: 24.0
--     , id: "One Way"
--     , itemPadding : ( Padding 4 4 4 4 )
--     , isSelected : true
--     }
--     ,
--     { itemHeight: ( V 54 )
--     , itemGravity: CENTER_VERTICAL
--     , itemFontColor: Color.black900
--     , itemBackground: Color.white900
--     , itemText: "Return Trip"
--     , itemCornerRadius: 24.0
--     , id: "Return Trip"
--     , itemPadding : ( Padding 4 4 4 4 )
--     , isSelected : false
--     }
--   ]


-- config :: BaseConfig 
-- config = {
--       baseHeight : WRAP_CONTENT
--     , baseWidth : MATCH_PARENT
--     , baseCornerRadius :  24.0
--     , baseMargin : (MarginHorizontal 16 16)
--     , baseBackground  : Color.green700
--     , basePadding : ( Padding 4 4 4 4 )
--     , baseGravity : CENTER
--     , items : itemsArray
--     , id : "base"
-- }


