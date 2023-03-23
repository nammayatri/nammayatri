module Components.ChooseVehicle.Controller where

import Prelude((<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import Common.Types.App
import Data.Maybe(Maybe(..))

data Action = NoAction | OnClick | OnImageClick

type Config = {
    backgroundColour :: String,
    height :: Length,
    stroke :: String,
    margin :: Margin,
    primaryimage :: ImageConfig,
    secondaryImage :: ImageConfig,
    mainText :: TextConfig,
    subText1 :: TextConfig,
    subText2 :: TextConfig,
    cornerText :: TextConfig,
    imageStroke :: Maybe String,
    imageLayoutHeight :: Length,
    imageLayoutWidth :: Length,
    onselect :: Boolean,
    clickable :: Boolean
}

type ImageConfig = {
    height :: Length,
    width :: Length,
    margin :: Margin,
    clickable ::  Boolean,
    imageUrl :: String,
    visibility :: Visibility,
    padding :: Padding
}

type TextConfig = {
    text :: String,
    textSize :: Int,
    visibility :: Visibility,
    colour :: String 
}
config :: Config
config = {
    backgroundColour : Color.blue600,
    height : V 72,
    stroke : "1,"<>Color.blue800,
    margin :  MarginHorizontal 16 16,
    clickable : true,
    primaryimage :{
    height : V 48,
    width :  V 60,
    margin : (Margin 10 0 8 0),
    clickable : false,
    imageUrl : "ny_ic_Sedan_Yellow.png,https://assets.juspay.in/nammayatri/images/user/ny_ic_sedan_yellow.png",
    visibility : VISIBLE,
    padding : (Padding 0 0 0 0)
},
    secondaryImage : {
    height : V 16,
    width : V 16,
    margin : (Margin 0 0 0 0),
    clickable : true,
    imageUrl : "ny_ic_info,https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png",
    visibility : VISIBLE,
    padding : (Padding 8 7 10 0)
}
,
    mainText :{
    text : "Non AC Taxi",
    textSize : FontSize.a_18,
    visibility : VISIBLE,
    colour : Color.black800
},
    subText1 :{
    text : "Economical",
    textSize : FontSize.a_12,
    visibility : VISIBLE,
    colour : Color.black700 
},
    subText2 :{
    text : "4 people",
    textSize : FontSize.a_12,
    visibility : VISIBLE,
    colour : Color.black700
},  
    cornerText :{
    text : "146",
    textSize : FontSize.a_18,
    visibility : VISIBLE,
    colour : Color.black800
},
    imageStroke : Nothing,
    imageLayoutHeight : V 44,
    imageLayoutWidth : V 34,
    onselect : false
}