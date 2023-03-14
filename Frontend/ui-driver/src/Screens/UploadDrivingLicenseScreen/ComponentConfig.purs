module Screens.UploadDrivingLicenseScreen.ComponentConfig where

import Common.Types.App
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings
import Language.Types (STR(..))
import Screens.Types as ST
import Styles.Colors as Color
import Prelude
import PrestoDOM

------------------------------ primaryButtonConfig --------------------------------
primaryButtonConfig :: ST.UploadDrivingLicenseState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString NEXT), textSize = FontSize.a_16}
      , width = MATCH_PARENT
      , background = Color.black900
      , margin = (Margin 0 0 0 0)
      , cornerRadius = 0.0
      , height = (V 60)
      , isClickable = state.data.imageFront /= "" && state.data.dob /= "" && DS.length state.data.driver_license_number >= 9 && (DS.toLower(state.data.driver_license_number) == DS.toLower(state.data.reEnterDriverLicenseNumber)) && (state.data.dateOfIssue /= Just "")
      , alpha = if (state.data.imageFront /= "" && state.data.dob /= "" && DS.length state.data.driver_license_number >= 9) && (DS.toLower(state.data.driver_license_number) == DS.toLower(state.data.reEnterDriverLicenseNumber)) && (state.data.dateOfIssue /= Just "") then 1.0 else 0.8
      }
  in primaryButtonConfig'

------------------------------ primaryEditTextConfig --------------------------------
primaryEditTextConfig :: ST.UploadDrivingLicenseState -> PrimaryEditText.Config
primaryEditTextConfig state = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { singleLine = true
          , pattern = Just "[A-Z0-9]*,16"
          , fontStyle = FontStyle.bold LanguageStyle
          , textSize = FontSize.a_16
          , placeholder = (getString ENTER_DL_NUMBER)
          , capsLock = true
        }
      , topLabel
        { textSize = FontSize.a_12
        , text = (getString DRIVING_LICENSE_NUMBER)
        , color = Color.greyTextColor
        }
      , margin = (MarginBottom 15)
      , background = Color.white900
      , id = (EHC.getNewIDWithTag "EnterDrivingLicenseEditText")
      }
    in primaryEditTextConfig'

------------------------------ primaryEditTextConfigReEnterDl --------------------------------
primaryEditTextConfigReEnterDl :: ST.UploadDrivingLicenseState -> PrimaryEditText.Config
primaryEditTextConfigReEnterDl state = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { singleLine = true
          , pattern = Just "[A-Z0-9]*,16"
          , fontStyle = FontStyle.bold LanguageStyle
          , textSize = FontSize.a_16
          , placeholder = (getString ENTER_DL_NUMBER)
          , capsLock = true
          , color = Color.black800
        }
      , stroke = if (DS.toLower(state.data.driver_license_number) /= DS.toLower(state.data.reEnterDriverLicenseNumber)) then ("1," <> Color.red) else ("1," <> Color.borderColorLight)
      , topLabel
        { textSize = FontSize.a_12
        , text = (getString RE_ENTER_DRIVING_LICENSE_NUMBER)
        , color = Color.greyTextColor
        }
      , margin = (MarginBottom 15)
      , background = Color.white900
      , id = (EHC.getNewIDWithTag "ReEnterDrivingLicenseEditText")
      }
    in primaryEditTextConfig'