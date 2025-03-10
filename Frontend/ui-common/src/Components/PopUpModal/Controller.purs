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
import PrestoDOM (Padding(..), Margin(..), Gravity(..), Visibility(..), Length(..), PrestoDOM)
import Font.Size as FontSize
import Font.Style (Style(..))
import Common.Types.App as Common
import PrestoDOM.Types.DomAttributes (Corners(..)) as PTD
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Prelude ((<>), Unit)
import Data.Maybe as Mb
import Font.Style as FontStyle
import Engineering.Helpers.Commons as EHC
import Components.TipsView as TipsView
import JBridge
import Effect (Effect)

data Action = OnButton1Click
            | OnButton2Click
            | NoAction
            | ETextController PrimaryEditTextController.Action
            | CountDown Int String String
            | OnImageClick
            | DismissPopup
            | OptionWithHtmlClick
            | OnSecondaryTextClick
            | YoutubeVideoStatus String
            | TipsViewActionController TipsView.Action
            | OnCoverImageClick
            | PersonMobile PrimaryEditTextController.Action
            | PersonName PrimaryEditTextController.Action
            | PersonAddress PrimaryEditTextController.Action
            | PersonInstruction PrimaryEditTextController.Action
            | CheckBoxClick

type Config = {
    primaryText :: TextConfig,
    headerInfo :: TextConfig,
    customerTipArray :: Array String,
    customerTipArrayWithValues :: Array Int,
    secondaryText :: TextConfig,
    popUpHeaderConfig :: PopUpHeaderConfig,
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
    searchExpired :: Boolean,
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
    topTitle :: TopTitle,
    listViewArray :: Array String,
    coverMediaConfig :: CoverMediaConfig,
    timerId :: String,
    onlyTopTitle :: Visibility,
    topTextVisibility :: Boolean,
    isTipEnabled :: Boolean,
    isVisible :: Boolean,
    isTipPopup :: Boolean,
    coverLottieConfig :: LottieConfig,
    showRetry :: Boolean,
    coverLottie :: CoverLottie,
    layout :: forall w. Mb.Maybe (LayoutConfig -> PrestoDOM (Effect Unit) w),
    completeProfileLayout :: forall w. Mb.Maybe (PrestoDOM (Effect Unit) w),
    upiDetailConfig :: UPIDetailConfig,
    deliveryDetailsConfig :: DeliveryDetailsConfig,
    externalHeader :: forall w. Mb.Maybe (PrestoDOM (Effect Unit) w),
    voiceToTextConfig :: VoiceToTextConfig
}


type VoiceToTextConfig = {
  id :: String,
  enabled :: Boolean
}

type DeliveryDetailsConfig = {
  visibility :: Visibility,
  margin  :: Margin,
  personNameDetails :: PrimaryEditTextController.Config,
  mobileNumberDetails :: PrimaryEditTextController.Config,
  addressDetails :: PrimaryEditTextController.Config,
  instructionDetails :: PrimaryEditTextController.Config,
  isSource :: Boolean,
  locationTitle :: String,
  locationDetails :: String,
  checkBoxDetails :: {text :: String, isSelected :: Boolean, visibility :: Boolean }
}

type UPIDetailConfig = {
  visibility :: Visibility,
  upiID :: String,
  accountName :: String,
  imageConfig :: ImageConfig
}

type LayoutConfig = {
  visibility :: Visibility
}

type CoverMediaConfig = {
  visibility :: Visibility,
  height :: Length ,
  width :: Length ,
  margin :: Margin ,
  padding :: Padding ,
  mediaUrl :: String ,
  mediaType :: String ,
  id :: String,
  background :: String,
  stroke :: String,
  cornerRadius :: Number,
  coverMediaText :: TextConfig,
  audioAutoPlay :: Boolean
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
  suffixImage :: ImageConfig,
  prefixImage :: ImageConfig,
  isClickable :: Boolean
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
  showShimmer :: Boolean,
  gravity :: Gravity,
  enableRipple :: Boolean,
  rippleColor :: String
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

type LottieConfig = 
  {
    lottieUrl :: String
  , visibility :: Visibility
  , repeat :: Boolean
  , height :: Length
  , width :: Length
  , margin :: Margin
  , padding :: Padding
  , id :: String
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

type TopTitle = {
    text :: String
  , visibility :: Visibility
  , width :: Length
  , height :: Length 
  , margin :: Margin
  , color :: String
  , gravity :: Gravity
  , textStyle :: Style
}

type CoverLottie = {
  id :: String
, background :: String
, cornerRadius :: Number
, padding :: Padding
, visibility :: Visibility
, height :: Length
, width :: Length
, margin :: Margin
, config :: LottieAnimationConfig
}
type PopUpHeaderConfig = { 
    height :: Length
  , width :: Length
  , margin :: Margin
  , padding :: Padding
  , visibility :: Visibility
  , backgroundColor :: String
  , cornerRadius :: Number
  , orientation :: String
  , headingText :: TextConfig
  , subHeadingText :: TextConfig
  , imageConfig :: ImageConfig
  , gravity :: Gravity
  }

config :: Config
config = {
  optionButtonOrientation: "HORIZONTAL"
  , showRetry : true
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
  , searchExpired : false
  , padding : (Padding 0 0 0 0)
  , topTitle : {
      text : ""
    , visibility : GONE
    , width : WRAP_CONTENT
    , height : WRAP_CONTENT 
    , margin : MarginVertical 10 10
    , color : Color.black800
    , gravity : LEFT
    , textStyle : Heading2
    }
  , primaryText : {
      text : "Text1",
      color : Color.black800,
      gravity : CENTER,
      isClickable : false,
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
      },
      prefixImage : {
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
      isClickable : false ,
      textStyle : ParagraphText,
      accessibilityHint : "", 
      suffixImage : {
        visibility : GONE
        , imageUrl : ""
        , height : (V 0)
        , width : (V 0)
        , margin : (Margin 0 0 0 0)
        , padding : (Padding 0 0 0 0)
      },
      prefixImage : {
        visibility : GONE
        , imageUrl : ""
        , height : (V 0)
        , width : (V 0)
        , margin : (Margin 0 0 0 0)
        , padding : (Padding 0 0 0 0)
      }
    }
  , headerInfo : {
      text : "Step",
      color : Color.textSecondary,
      gravity : RIGHT,
      padding : (Padding 16 0 16 0),
      margin : (Margin 0 20 0 20),
      visibility : GONE,
      textStyle : ParagraphText,
      isClickable : true,
      accessibilityHint : "", 
      suffixImage : {
        visibility : GONE
        , imageUrl : ""
        , height : (V 0)
        , width : (V 0)
        , margin : (Margin 0 0 0 0)
        , padding : (Padding 0 0 0 0)
      },
      prefixImage : {
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
    , gravity : CENTER
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
    , enableRipple : false
    , rippleColor : Color.rippleShade
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
    , gravity : CENTER
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
    , enableRipple : false
    , rippleColor : Color.rippleShade
    }
  , option2 : {
      background : Color.black900
    , text : "Button2"
    , strokeColor : Color.black900
    , color : Color.yellow900
    , visibility : true
    , margin : (Margin 12 0 0 16)
    , gravity : CENTER
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
    , enableRipple : false
    , rippleColor : Color.rippleShade
    }
  , optionWithHtml : {
      background : Color.black900,
      strokeColor : Color.black900,
      textOpt1 : {
        text : "",
        color : Color.black800,
        gravity : CENTER,
        isClickable :true,
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
        }, 
        prefixImage : {
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
        isClickable : true,
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
        }, 
        prefixImage : {
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
    { imageUrl : fetchImage FF_COMMON_ASSET "ny_ic_close"
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
      imageUrl : fetchImage FF_COMMON_ASSET "ny_ic_ride_completed"
    , visibility : GONE
    , height : WRAP_CONTENT
    , width : WRAP_CONTENT
    , margin : (Margin 0 0 0 0)
    , padding : (Padding 0 0 0 0)
    },
    coverLottieConfig : 
    {
      lottieUrl : ""
    , id : ""
    , visibility : GONE
    , repeat : false
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
    , coverMediaConfig : {
        visibility : GONE ,
        height : V 400 ,
        width : WRAP_CONTENT ,
        margin : (Margin 0 0 0 0) ,
        padding : (Padding 0 0 0 0) ,
        mediaType : "",
        mediaUrl : "",
        id : "",
        background : Color.transparent,
        stroke : "1," <> Color.transparent,
        cornerRadius : 16.0
      , audioAutoPlay : false
      , coverMediaText : {
          text : "",
          color : Color.textSecondary,
          gravity : CENTER,
          isClickable : true,
          padding : PaddingHorizontal 16 16,
          margin : MarginVertical 20 20,
          visibility : GONE,
          textStyle : SubHeading2,
          accessibilityHint : "", 
          suffixImage : {
            visibility : GONE
            , imageUrl : ""
            , height : V 0
            , width : V 0
            , margin : Margin 0 0 0 0
            , padding : Padding 0 0 0 0
          }, 
          prefixImage : {
            visibility : GONE
            , imageUrl : ""
            , height : (V 0)
            , width : (V 0)
            , margin : (Margin 0 0 0 0)
            , padding : (Padding 0 0 0 0)
          }
      }
    },
    onlyTopTitle : VISIBLE,
    timerId : "",
    topTextVisibility : false,
    isVisible : false,
    isTipEnabled : true,
    isTipPopup : false
    , coverLottie : {
      id : ""
    , background : Color.transparent
    , cornerRadius : 0.0
    , padding : Padding 0 0 0 0
    , visibility : GONE
    , height : V 0
    , width : V 0
    , margin : Margin 0 0 0 0
    , config : lottieAnimationConfig
    },
    popUpHeaderConfig : {
      height : V 0,
      width : MATCH_PARENT,
      margin : (Margin 0 0 0 0),
      padding : (Padding 0 0 0 0),
      visibility : GONE,
      backgroundColor : Color.white900,
      cornerRadius : 0.0,
      orientation : "VERTICAL",
      headingText : {
      text : "Text1",
      color : Color.black800,
      isClickable : false,
      gravity : CENTER,
      padding : (Padding 0 0 0 0),
      margin : (Margin 0 0 0 0),
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
      },
      prefixImage : {
        visibility : GONE
        , imageUrl : ""
        , height : (V 0)
        , width : (V 0)
        , margin : (Margin 0 0 0 0)
        , padding : (Padding 0 0 0 0)
      }
    },
    subHeadingText :{
      text : "",
      color : Color.textSecondary,
      gravity : CENTER,
      padding : (Padding 16 0 16 0),
      margin : (Margin 0 20 0 20),
      isClickable : false,
      visibility : VISIBLE,
      textStyle : Tags,
      accessibilityHint : "", 
      suffixImage : {
        visibility : GONE
        , imageUrl : ""
        , height : (V 0)
        , width : (V 0)
        , margin : (Margin 0 0 0 0)
        , padding : (Padding 0 0 0 0)
      },
      prefixImage : {
        visibility : GONE
        , imageUrl : ""
        , height : (V 0)
        , width : (V 0)
        , margin : (Margin 0 0 0 0)
        , padding : (Padding 0 0 0 0)
      }
    },
    imageConfig : {
      visibility : GONE
      , imageUrl : ""
      , height : (V 0)
      , width : (V 0)
      , margin : (Margin 0 0 0 0)
      , padding : (Padding 0 0 0 0)
    },
    gravity : CENTER
  }
  , layout : Mb.Nothing
  , upiDetailConfig : {
      visibility : GONE,
      upiID : "",
      accountName : "",
      imageConfig : {
        visibility : GONE
        , imageUrl : ""
        , height : (V 0)
        , width : (V 0)
        , margin : (Margin 0 0 0 0)
        , padding : (Padding 0 0 0 0)
      }
    }
  , deliveryDetailsConfig : dummyDeliveryDetailsConfig
  , externalHeader : Mb.Nothing
  , completeProfileLayout : Mb.Nothing
  , voiceToTextConfig : dummyVoiceToTextConfig 
}

dummyDeliveryDetailsConfig :: DeliveryDetailsConfig
dummyDeliveryDetailsConfig = 
  let config' = dummyDeliveryPrimaryText
  in
    {
      visibility : GONE,
      margin : Margin 0 0 0 0,
      isSource : true,
      locationTitle : "",
      locationDetails : "",
      personNameDetails : config',
      mobileNumberDetails : config',
      addressDetails : config',
      instructionDetails : config',
      checkBoxDetails : {
        text : ""
        , isSelected : false
        , visibility : true
      }
    }

dummyDeliveryPrimaryText :: PrimaryEditTextController.Config
dummyDeliveryPrimaryText = 
  let
      config = PrimaryEditTextController.config
      primaryEditTextConfig' = config
        { editText
          { color = Color.black800
          , singleLine = true
          , placeholder = ""
          , textStyle = FontStyle.SubHeading3
          }
        , background = Color.white900
        , topLabel
          { text = ""
          , color = Color.black800
          , textStyle = FontStyle.Body3
          }
        , stroke = ("1,"<> Color.black500)
        , margin = (Margin 0 8 0 0)
        , errorLabel
          { text = ""
          , margin = (MarginTop 1)
          }
        , showErrorLabel = false
        , width = MATCH_PARENT
        }
      in primaryEditTextConfig'

dummyVoiceToTextConfig :: VoiceToTextConfig
dummyVoiceToTextConfig = 
  {
    id : "",
    enabled: false
  }
