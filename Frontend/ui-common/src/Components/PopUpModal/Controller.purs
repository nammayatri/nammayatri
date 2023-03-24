{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.PopUpModal.Controller where

import Styles.Colors as Color
import PrestoDOM (Padding(..), Margin(..), Gravity(..), Visibility(..), Length(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Common.Types.App
import PrestoDOM.Types.DomAttributes (Corners(..))
import Components.PrimaryEditText.Controller as PrimaryEditTextController

data Action = OnButton1Click 
            | OnButton2Click 
            | NoAction 
            | ETextController PrimaryEditTextController.Action 
            | CountDown Int String String String
            | OnImageClick

type Config = {
    primaryText :: TextConfig,
    secondaryText :: TextConfig,
    option1 :: ButtonConfig,
    option2 :: ButtonConfig,
    backgroundClickable :: Boolean,
    cornerRadius :: Corners,
    margin :: Margin,
    gravity :: Gravity,
    buttonLayoutMargin :: Margin,
    eTextConfig :: PrimaryEditTextController.Config,
    editTextVisibility :: Visibility,
    dismissPopupConfig :: DismissPopupConfig,
    coverImageConfig :: CoverImageConfig,
    contactViewConfig :: ContactViewConfig,
    contactViewPadding :: Padding,
    contactViewMargin :: Margin
}

type ContactViewConfig = {
  visibility :: Visibility,
  fullName :: String,
  nameInitials :: String,
  padding :: Padding
}

type TextConfig = {
  text :: String,
  color :: String,
  fontSize :: Int,
  gravity :: Gravity,
  padding :: Padding,
  margin :: Margin,
  visibility :: Visibility,
  fontStyle :: String
}
type ButtonConfig = {
  background :: String,
  strokeColor :: String,
  text :: String,
  color :: String,
  fontSize :: Int,
  visibility :: Boolean,
  margin :: Margin,
  isClickable :: Boolean,
  width :: Length,
  padding :: Padding,
  timerValue :: Int,
  enableTimer :: Boolean,
  timerID :: String,
  fontStyle :: String
}

type DismissPopupConfig = 
  { imageUrl :: String
  , height :: Length
  , width :: Length 
  , margin :: Margin 
  , alpha :: Number
  , padding :: Padding
  , visibility :: Visibility
  }

type CoverImageConfig =
  {
    visibility :: Visibility
  , imageUrl :: String
  , height :: Length
  , width :: Length 
  , margin :: Margin 
  , padding :: Padding
  }

config :: Config 
config = {
  backgroundClickable : true
  , cornerRadius : (Corners 24.0 true true false false)
  , margin : (Margin 0 0 0 0)
  , gravity : BOTTOM
  , buttonLayoutMargin : (Margin 0 0 0 25)
  , editTextVisibility : GONE
  , primaryText : {
      text : "Text1",
      color : Color.black800,
      fontSize : FontSize.a_18,
      gravity : CENTER,
      padding : (Padding 16 0 16 0),
      margin : (Margin 0 20 0 0),
      visibility : VISIBLE,
      fontStyle : FontStyle.bold LanguageStyle
    }
  , secondaryText : {
      text : "Text2",
      color : Color.textSecondary,
      fontSize : FontSize.a_15,
      gravity : CENTER,
      padding : (Padding 16 0 16 0),
      margin : (Margin 0 20 0 20),
      visibility : VISIBLE,
      fontStyle : FontStyle.medium LanguageStyle
    }
  , option1 : {
      background : Color.white900
    , text : "Button1"
    , strokeColor : Color.black900 
    , color : Color.black900
    , fontSize : FontSize.a_14
    , visibility : true
    , margin : (Margin 0 0 0 0)
    , isClickable : true
    , width : (V 156)
    , padding : (Padding 0 0 0 0)
    , timerValue : 5
    , enableTimer : false
    , timerID : ""
    , fontStyle : FontStyle.bold LanguageStyle
    }
  , option2 : {
      background : Color.black900
    , text : "Button2"
    , strokeColor : Color.black900
    , color : Color.yellow900
    , fontSize : FontSize.a_14
    , visibility : true
    , margin : (Margin 12 0 0 16)
    , isClickable : true
    , width : (V 156)
    , padding : (Padding 0 0 0 0)
    , timerValue : 5
    , enableTimer : false
    , timerID : ""
    , fontStyle : FontStyle.bold LanguageStyle
    }
    , dismissPopupConfig : 
    { imageUrl : "ny_ic_close,https://assets.juspay.in/nammayatri/images/common/ny_ic_close.png"
    , height : WRAP_CONTENT
    , width : WRAP_CONTENT
    , margin : (MarginTop 20)
    , padding : (Padding 0 0 0 0)
    , alpha : 0.7
    , visibility : GONE
    }
    , eTextConfig : PrimaryEditTextController.config
    , coverImageConfig :
    {
      imageUrl : "ny_ic_ride_completed,https://assets.juspay.in/nammayatri/images/common/ny_ic_ride_completed.png"
    , visibility : GONE
    , height : WRAP_CONTENT
    , width : WRAP_CONTENT
    , margin : (Margin 0 0 0 0)
    , padding : (Padding 0 0 0 0)
    },
    contactViewConfig : 
    {
       nameInitials: "",
       fullName: "",
       visibility : GONE,
       padding : PaddingLeft 8
    }
    , contactViewPadding : (Padding 23 16 23 16)
    , contactViewMargin : (Margin 16 12 16 32)
}