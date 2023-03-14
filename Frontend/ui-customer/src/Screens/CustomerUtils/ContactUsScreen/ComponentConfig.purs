module Screens.CustomerUtils.ContactUsScreen.ComponentConfig where

import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Maybe (Maybe(..))
import Data.String as DS 
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB 
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((<>), (==), (>=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.ContactUsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App

primaryButtonConfigSubmit :: ST.ContactUsScreenState -> PrimaryButton.Config
primaryButtonConfigSubmit state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig
        { text = (getString SUBMIT)
        , color = if state.props.btnActive then Color.yellowRadler else "#FEEBB9"      
        }
      , cornerRadius = 0.0
      , background = if state.props.btnActive then Color.black900 else "#B9BABE"
      , isClickable = state.props.btnActive 
      , margin = (Margin 0 0 0 0)
      , id = "SubmitButtonContactUsScreen"
      , enableLoader = (JB.getBtnLoader "SubmitButtonContactUsScreen")
      }
  in primaryButtonConfig'

primaryEditTextConfigDescription :: ST.ContactUsScreenState -> PrimaryEditText.Config
primaryEditTextConfigDescription state = let
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText 
        { color = Color.black800
        , textSize = FontSize.a_14
        , fontStyle = FontStyle.medium LanguageStyle
        , margin = if EHC.os == "IOS" then (Margin 10 16 10 10) else (Margin 16 16 16 16)
        , singleLine = false
        , placeholder = (getString YOU_CAN_DESCRIBE_THE_ISSUE_YOU_FACED_HERE)
        , pattern = Just "[A-Za-z0-9,. ]*,300"
        }
      , background = Color.white900
      , height = V 120
      , stroke = if (DS.length state.data.description >= 300) then ("1,"<>Color.textDanger) else ("1,"<>Color.borderColorLight)
      , topLabel
        { text = (getString DESCRIBE_YOUR_ISSUE)
        , textSize = FontSize.a_12
        , color = Color.black800
        , fontStyle = FontStyle.regular LanguageStyle
        }  
      , margin = (Margin 10 32 10 0)
      , showErrorLabel = (DS.length state.data.description >= 300)
      , errorLabel 
        { text = ((getString MAX_CHAR_LIMIT_REACHED) <> " 300 " <> (getString OF) <> " 300")
        , fontStyle = FontStyle.regular LanguageStyle 
        , color = Color.textDanger
        }
      } 
    in primaryEditTextConfig'

primaryEditTextConfigEmail :: ST.ContactUsScreenState -> PrimaryEditText.Config
primaryEditTextConfigEmail state = let
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText 
        { color = Color.black800
        , textSize = FontSize.a_14
        , fontStyle = FontStyle.medium LanguageStyle
        , margin = (Margin 16 16 16 16)
        , placeholder = "example@xyz.com"
        }
      , background = Color.white900
      , topLabel
        { text = (getString YOUR_EMAIL_ID)
        , textSize = FontSize.a_12
        , color = Color.black800
        , fontStyle = FontStyle.regular LanguageStyle
        }  
      , margin = (Margin 10 32 10 0)
      } 
    in primaryEditTextConfig'

primaryEditTextConfig :: ST.ContactUsScreenState -> PrimaryEditText.Config
primaryEditTextConfig state = let
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText 
        { color = Color.black800
        , textSize = FontSize.a_14
        , fontStyle = FontStyle.medium LanguageStyle
        , margin = if EHC.os == "ANDROID" then (Margin 16 4 16 4) else (Margin 16 16 16 4)
        , gravity = CENTER_VERTICAL
        , placeholder = (getString I_AM_NOT_RECEIVING_ANY_RIDES)
        , pattern = Just "[a-zA-Z0-9 ]*,100"
        , padding = (Padding 0 0 0 0)
        , singleLine = false
        }
      , height = (V 54)
      , margin = (Margin 10 32 10 0)
      , background = Color.white900
      , topLabel
        { text = (getString SUBJECT)
        , textSize = FontSize.a_12
        , color = Color.black800
        , fontStyle = FontStyle.regular LanguageStyle
        }  
      , errorLabel
        { text = ((getString MAX_CHAR_LIMIT_REACHED) <> " 100 " <> (getString OF) <> " 100")
        , fontStyle = FontStyle.regular LanguageStyle
        , color = Color.textDanger
        }
      , showErrorLabel = ((DS.length state.data.subject) >= 100 )
      , stroke = if ((DS.length state.data.subject) >= 100 ) then ("1," <> Color.textDanger) else  ("1," <> Color.borderColorLight) 
      } 
    in primaryEditTextConfig'

primaryButtonConfig :: ST.ContactUsScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig
        { text = (getString GO_TO_HOME__)
        , color = Color.yellowRadler           
        }
      , cornerRadius = 0.0
      , background = Color.black900
      , margin = (Margin 0 0 0 0)
      , id = "GotoHomeThankyouScreen"
      , enableLoader = (JB.getBtnLoader "GotoHomeThankyouScreen")
      }
  in primaryButtonConfig'

genericHeaderConfig :: ST.ContactUsScreenState -> GenericHeader.Config 
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
       visibility = VISIBLE
      , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
      , height = (V 25)
      , width = (V 25)
      , margin = (Margin 12 12 12 12)
      } 
    , padding = (Padding 0 5 0 5)
    , textConfig {
        text = (getString WRITE_TO_US)
      , textSize = FontSize.a_18
      , color = Color.darkDescriptionText
      , fontStyle = FontStyle.bold LanguageStyle
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'
