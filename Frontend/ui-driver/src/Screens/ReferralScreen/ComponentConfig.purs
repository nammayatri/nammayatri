{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReferralScreen.ComponentConfig where

import Components.GenericHeader as GenericHeader
import Components.PrimaryEditText.Views as PrimaryEditText
import Components.PrimaryButton as PrimaryButton
import Components.PopUpModal as PopUpModal
import Screens.ReferralScreen.Controller (Action(..), ScreenOutput, eval)
import Effect (Effect)
import Common.Types.App
import Data.Maybe
import Data.String
import Font.Style as FontStyle
import PrestoDOM.Types.DomAttributes (Corners(..))
import Font.Size as FontSize
import Language.Strings
import Language.Types (STR(..))
import Prelude
import PrestoDOM
import Screens.Types as ST
import Styles.Colors as Color



passwordPopUpConfig :: ST.ReferralScreenState -> PopUpModal.Config
passwordPopUpConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
    gravity = CENTER,
    margin = (MarginHorizontal 16 16),
    buttonLayoutMargin = (Margin 0 16 16 0),
    editTextVisibility = VISIBLE,
    dismissPopupConfig {visibility = VISIBLE, height = V 12 , width = V 12, margin = (Margin 0 26 22 0)},
    eTextConfig { editText{text = "" ,placeholder= (getString ENTER_PASSWORD), pattern = Just "[^\n]*,5"}, topLabel{ text = (getString PASSWORD) ,  color = Color.black900}, margin = (Margin 16 16 16 0), type = ""},
    primaryText {text = (getString REFERRAL_CODE_LINKING) , gravity = LEFT ,margin = (Margin 16 21 0 0)},
    secondaryText {visibility = GONE},
    option1 {visibility = false},
    option2 {text = (getString CONFIRM_PASSWORD), background = Color.white900, color=Color.blue800, strokeColor = Color.white900, padding = (PaddingHorizontal 16 16)
    ,  isClickable = state.props.confirmBtnActive},
    cornerRadius = (Corners 15.0 true true true true)
  }
  in popUpConfig'

contactSupportConfig :: ST.ReferralScreenState->  PopUpModal.Config
contactSupportConfig state  =
  let
  config' =  PopUpModal.config
  popUpConfig' = config' {
    primaryText {
      text = (getString CONTACT_SUPPORT)<>"?"
    , margin = (Margin 40 23 40 12)
    }
    , option1 {
      text = (getString CANCEL)
    , margin = (MarginHorizontal 16 16) }
    , option2 {
      text = (getString CALL_SUPPORT)
    , margin = (MarginHorizontal 12 0) }
    , backgroundClickable = true
    , secondaryText {
      text = (getString YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT)
    , margin = (Margin 40 0 40 32) }
    , gravity = CENTER
    , margin = (MarginHorizontal 16 16)
    , cornerRadius = (Corners 20.0 true true true true)
  }
  in popUpConfig'


--------------------------------------------------- genericHeaderConfig -----------------------------------------------------
genericHeaderConfig :: ST.ReferralScreenState -> GenericHeader.Config
genericHeaderConfig state = let
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , width = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_chevron_left"
      , margin = (Margin 12 12 12 12)
      , visibility = if state.props.stage == ST.ReferralFlow then VISIBLE else GONE
      }
    , padding = if state.props.stage == ST.ReferralFlow then (Padding 0 5 0 5) else (Padding 16 16 0 16)
    , textConfig {
        text = if state.props.stage == ST.LeaderBoard then (getString RANKINGS) else if state.props.stage == ST.ReferralFlow then (getString REFERRAL_ENROLMENT) else (getString REFERRALS)
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'




primaryButtonViewConfig :: ST.ReferralScreenState -> PrimaryButton.Config
primaryButtonViewConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig{ text = (getString LINK_REFERRAL_CODE) }
      , id = "PrimaryButtonReferralScreen"
      , isClickable = state.props.primarybtnActive
      , alpha = if state.props.primarybtnActive then 1.0 else 0.6
      , height = (V 60)
      , cornerRadius = 8.0
      , margin = (Margin 0 0 0 0)
      }
  in primaryButtonConfig'