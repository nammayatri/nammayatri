{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.PopUpModal.Controller where

import Common.Styles.Colors as Color
import PrestoDOM (Padding(..), Margin(..), Gravity(..), Visibility(..), Length(..))
import Font.Size as FontSize
import Font.Style (Style(..))
import Common.Types.App as Common
import PrestoDOM.Types.DomAttributes (Corners(..)) as PTD
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Prelude ((<>))
import Data.Maybe as Mb

data Action = OnButton1Click
            | OnButton2Click
            | NoAction
            | ETextController PrimaryEditTextController.Action
            | CountDown Int String String String
            | OnImageClick
            | Tipbtnclick Int Int
            | DismissPopup
            | OptionWithHtmlClick
            | OnSecondaryTextClick

type Config = {
    primaryText :: TextConfig,
    customerTipArray :: Array String,
    customerTipArrayWithValues :: Array Int,
    secondaryText :: TextConfig,
    option1 :: ButtonConfig,
    option2 :: ButtonConfig,
    tipButton :: ButtonConfig,
    backgroundClickable :: Boolean,
    customerTipAvailable :: Boolean,
    cornerRadius :: PTD.Corners,
    margin :: Margin,
    gravity :: Gravity,
    activeIndex :: Int,
    optionButtonOrientation :: String,
    buttonLayoutMargin :: Margin,
    tipLayoutMargin :: Margin,
    eTextConfig :: PrimaryEditTextController.Config,
    editTextVisibility :: Visibility,
    dismissPopupConfig :: DismissPopupConfig,
    coverImageConfig :: ImageConfig,
    contactViewConfig :: ContactViewConfig,
    contactViewPadding :: Padding,
    contactViewMargin :: Margin,
    dismissPopup :: Boolean,
    padding :: Padding,
    dismissIconVisibility :: Visibility,
    dismissIconMargin :: Margin,
    fareEstimate :: String,
    tipSelected :: String,
    fareEstimateText :: String,
    tipSelectedText :: String,
    backgroundColor  :: String,
    optionWithHtml :: OptionWithHtmlConfig,
    topTitle :: Mb.Maybe String,
    listViewArray :: Array String
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
  gravity :: Gravity,
  padding :: Padding,
  margin :: Margin,
  visibility :: Visibility,
  textStyle :: Style,
  accessibilityHint :: String,
  suffixImage :: ImageConfig
}
type ButtonConfig = {
  background :: String,
  strokeColor :: String,
  text :: String,
  color :: String,
  visibility :: Boolean,
  margin :: Margin,
  isClickable :: Boolean,
  width :: Length,
  padding :: Padding,
  timerValue :: Int,
  enableTimer :: Boolean,
  timerID :: String,
  textStyle :: Style,
  height :: Length,
  image :: ImageConfig,
  showShimmer :: Boolean
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

type ImageConfig =
  {
    visibility :: Visibility
  , imageUrl :: String
  , height :: Length
  , width :: Length
  , margin :: Margin
  , padding :: Padding
  }

type OptionWithHtmlConfig = {
  background :: String,
  strokeColor :: String,
  textOpt1 :: TextConfig,
  textOpt2 :: TextConfig,
  visibility :: Boolean,
  margin :: Margin,
  isClickable :: Boolean,
  width :: Length,
  height :: Length,
  padding :: Padding,
  textStyle :: Style,
  image :: ImageConfig,
  cornerRadius :: Number
}

config :: Config
config = {
  optionButtonOrientation: "HORIZONTAL"
  , activeIndex : 1
  , customerTipAvailable : false
  , backgroundClickable : true
  , customerTipArray : []
  , customerTipArrayWithValues : []
  , cornerRadius : (PTD.Corners 24.0 true true false false)
  , margin : (Margin 0 0 0 0)
  , gravity : BOTTOM
  , backgroundColor : Color.black9000
  , buttonLayoutMargin : (Margin 0 0 0 25)
  , editTextVisibility : GONE
  , tipLayoutMargin : (Margin 0 0 0 0)
  , padding : (Padding 0 0 0 0)
  , topTitle : Mb.Nothing
  , primaryText : {
      text : "Text1",
      color : Color.black800,
      gravity : CENTER,
      padding : (Padding 16 0 16 0),
      margin : (Margin 0 20 0 0),
      visibility : VISIBLE,
      textStyle : Heading2,
      accessibilityHint : "", 
      suffixImage : {
        visibility : GONE
        , imageUrl : ""
        , height : (V 0)
        , width : (V 0)
        , margin : (Margin 0 0 0 0)
        , padding : (Padding 0 0 0 0)
      }
    }
  , secondaryText : {
      text : "Text2",
      color : Color.textSecondary,
      gravity : CENTER,
      padding : (Padding 16 0 16 0),
      margin : (Margin 0 20 0 20),
      visibility : VISIBLE,
      textStyle : ParagraphText,
      accessibilityHint : "", 
      suffixImage : {
        visibility : GONE
        , imageUrl : ""
        , height : (V 0)
        , width : (V 0)
        , margin : (Margin 0 0 0 0)
        , padding : (Padding 0 0 0 0)
      }
    }
  , tipButton: {
     background : Color.white900
    , text : "Button1"
    , strokeColor : Color.black900
    , color : Color.black900
    , visibility : true
    , margin : (Margin 0 0 0 0)
    , isClickable : true
    , width : (V 100)
    , padding : (Padding 15 7 15 7)
    , timerValue : 5
    , enableTimer : false
    , timerID : ""
    , textStyle : Body3
    , height : (V 48)
    , image : {
        visibility : GONE
        , imageUrl : ""
        , height : (V 0)
        , width : (V 0)
        , margin : (Margin 0 0 0 0)
        , padding : (Padding 0 0 0 0)
    }
    , showShimmer : false
  } 
  , option1 : {
      background : Color.white900
    , text : "Button1"
    , strokeColor : Color.black900
    , color : Color.black900
    , visibility : true
    , margin : (Margin 0 0 0 0)
    , isClickable : true
    , width : (V 156)
    , padding : (Padding 0 0 0 0)
    , timerValue : 5
    , enableTimer : false
    , timerID : ""
    , height : (V 48)
    , textStyle : SubHeading1
    , image : {
        visibility : GONE
        , imageUrl : ""
        , height : (V 0)
        , width : (V 0)
        , margin : (Margin 0 0 0 0)
        , padding : (Padding 0 0 0 0)
    }
    , showShimmer : false
    }
  , option2 : {
      background : Color.black900
    , text : "Button2"
    , strokeColor : Color.black900
    , color : Color.yellow900
    , visibility : true
    , margin : (Margin 12 0 0 16)
    , isClickable : true
    , width : (V 156)
    , padding : (Padding 0 0 0 0)
    , timerValue : 5
    , enableTimer : false
    , timerID : ""
    , height : (V 48)
    , textStyle : SubHeading1
    , image : {
        visibility : GONE
        , imageUrl : ""
        , height : (V 0)
        , width : (V 0)
        , margin : (Margin 0 0 0 0)
        , padding : (Padding 0 0 0 0)
    }
    , showShimmer : false
    }
  , optionWithHtml : {
      background : Color.black900,
      strokeColor : Color.black900,
      textOpt1 : {
        text : "",
        color : Color.black800,
        gravity : CENTER,
        padding : (Padding 0 0 0 0),
        margin : (Margin 0 0 0 0),
        visibility : GONE,
        textStyle : Heading2,
        accessibilityHint : "", 
        suffixImage : {
          visibility : GONE
          , imageUrl : ""
          , height : (V 0)
          , width : (V 0)
          , margin : (Margin 0 0 0 0)
          , padding : (Padding 0 0 0 0)
        }
      },
      textOpt2 : {
        text : "",
        color : Color.black800,
        gravity : CENTER,
        padding : (Padding 0 0 0 0),
        margin : (Margin 0 0 0 0),
        visibility : GONE,
        textStyle : Heading2,
        accessibilityHint : "", 
        suffixImage : {
          visibility : GONE
          , imageUrl : ""
          , height : (V 0)
          , width : (V 0)
          , margin : (Margin 0 0 0 0)
          , padding : (Padding 0 0 0 0)
        }
      },
      visibility : false,
      margin : (Margin 0 0 0 0),
      isClickable : true,
      width : MATCH_PARENT,
      height : (V 48),
      padding : (Padding 0 0 0 0),
      textStyle : Heading2,
      cornerRadius : 8.0,
      image : {
        visibility : GONE
        , imageUrl : ""
        , height : (V 0)
        , width : (V 0)
        , margin : (Margin 0 0 0 0)
        , padding : (Padding 0 0 0 0)
      }
    }
    , dismissPopupConfig : 
    { imageUrl : "ny_ic_close," <> (getCommonAssetStoreLink Common.FunctionCall) <> "ny_ic_close.png"
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
      imageUrl : "ny_ic_ride_completed," <> (getCommonAssetStoreLink Common.FunctionCall) <> "ny_ic_ride_completed.png"
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
    , dismissPopup:false
    , dismissIconVisibility : GONE
    , dismissIconMargin : Margin 0 0 0 0
    , fareEstimate : ""
    , tipSelected : ""
    , fareEstimateText : ""
    , tipSelectedText : ""
    , listViewArray : []
}


