module Screens.ApplicationStatusScreen.ComponentConfig where
import Screens.Types as ST
import Components.ReferralMobileNumber as ReferralMobileNumber
import Components.PrimaryButton as PrimaryButton
import Components.PopUpModal as PopUpModal
import Prelude((<>),(||))
import Language.Types (STR(..))
import PrestoDOM (Gravity(..), Length(..), LetterSpacing(..), Margin(..), Orientation(..), Padding(..), PrestoDOM,  Visibility(..),background, color, fontStyle, gravity, height,  margin, orientation, padding, text, textSize, textView, width, visibility, cornerRadius)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Language.Strings (getString)
import Styles.Colors as Color
import Font.Size as FontSize
import Data.Maybe

primaryButtonConfig :: ST.ApplicationStatusScreenState -> PrimaryButton.Config
primaryButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = if state.props.onBoardingFailure then getString COMPLETE_ONBOARDING else getString ADD_ALTERNATE_NUMBER
      , color = Color.primaryButtonColor
      }
      , cornerRadius = 8.0
      , background = Color.black900
      , height = (V 48)
      , id = "ApplicationStatusScreenButton"
      }
  in primaryButtonConfig'

alternateMobileNumberConfig :: ST.ApplicationStatusScreenState -> ReferralMobileNumber.Config
alternateMobileNumberConfig state = let
  config' =ReferralMobileNumber.config
  referalNumberConfig' = config'{
      mainText = if state.props.enterOtp then getString ENTER_OTP else getString ENTER_ALTERNATE_MOBILE_NUMBER
    , isApplyButtonActive = state.props.buttonVisibilty
    , primaryButtonText =  getString NEXT
    , isValid = if state.props.enterOtp then state.props.isValidOtp else (state.props.isValidAlternateNumber || state.props.isAlternateMobileNumberExists)
    , errorText = if state.props.enterOtp then getString PLEASE_ENTER_VALID_OTP else if state.props.isAlternateMobileNumberExists then (getString PERSON_WITH_THIS_NUMBER_ALREADY_EXISTS) else (getString INVALID_MOBILE_NUMBER)
    , placeholder = if  state.props.enterOtp then getString ENTER_OTP else getString ENTER_ALTERNATE_MOBILE_NUMBER
    , letterSpacing = PX if  state.props.enterOtp then 5.0 else 0.0
    , subTextView = state.props.enterOtp
    , pattern = if state.props.enterOtp then Just "[0-9]*,4" else Just "[0-9]*,10"
    , subText1 =  getString OTP_SENT_TO <> state.data.mobileNumber
    , subText2 = getString RESEND_OTP
  }
  in referalNumberConfig'

completeOnboardingConfig :: ST.ApplicationStatusScreenState -> PopUpModal.Config
completeOnboardingConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
    gravity = CENTER
  , cornerRadius = (Corners 15.0 true true true true)
  , margin = (Margin 16 0 16 0)
  , primaryText {
      text = getString CONTACT_SUPPORT <>"?"
    }
  , secondaryText {
      text = getString (YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT")
    }
    , option1 {
     text =  getString CANCEL
    }
  , option2 {
      text =  getString CALL_SUPPORT
    }
  }
  in popUpConfig'
