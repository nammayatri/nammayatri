{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.Referral.Controller where

import Screens.Types(ReferralComponentState, ReferralStage(..))
import Components.PopUpModal as PopUpModal
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((<>), (==))
import Data.Maybe (maybe, Maybe(..))
import PrestoDOM
import PrestoDOM.Types.DomAttributes
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Components.PrimaryButton as PrimaryButton
import ConfigProvider

data Action = NoAction
            | PopUpModalAction PopUpModal.Action
            | OnClickDone String
            | ApplyAction PrimaryButton.Action
            | SkipAction PrimaryButton.Action
            | ReferredUserInfo PopUpModal.Action
            | ReferralProgramInfo PopUpModal.Action
            | OpenReferralProgramInfo
            | ReferralTextFocused Boolean

config :: ReferralComponentState
config =
  { stage : ENTER_REFERRAL_CODE
  , referralCode : Nothing
  , applyButtonActive : false
  , showReferredUserInfoPopup : false
  , showReferralProgramInfoPopup : false
  , isInvalidCode : false
  , isFocused : false
  }

referralPopUpConfig :: ReferralComponentState -> PopUpModal.Config 
referralPopUpConfig state = 
  let status = state.stage
      config' = PopUpModal.config 
      primaryButtonText = case status of
                            INVALID_POPUP -> getString EDIT
                            _ -> getString OKAY
      secondaryButtonText = getString TRY_LATER
      secondaryButtonVisibility = status == INVALID_POPUP
      primaryText = case status of
                      APPLIED_POPUP -> getString REFERRAL_CODE_IS_APPLIED
                                            <> maybe "" (\code -> "- " <> code) state.referralCode
                      INVALID_POPUP -> getString INVALID_REFERRAL_CODE
                      _ -> getString YOU_HAVE_ALREADY_USED_DIFFERENT_REFERRAL_CODE
      image = case status of
                INVALID_POPUP -> "ny_ic_referral_invalid"
                ALREADY_APPLIED_POPUP -> "ny_ic_referral_already_applied"
                _ -> "ny_ic_referral"
      popUpConfig' = config'{
        backgroundClickable = false,
        gravity = CENTER,
        buttonLayoutMargin = Margin 0 0 0 0,
        cornerRadius = Corners 16.0 true true true true, 
        padding = Padding 16 24 16 16,
        optionButtonOrientation = "VERTICAL",
        margin = MarginHorizontal 24 24, 
        primaryText {
          text = primaryText, 
          margin = MarginVertical 16 16
        },
        secondaryText { visibility = GONE },
        option1 {
          background = Color.black900,
          text = primaryButtonText,
          color = Color.yellow900,
          textStyle = FontStyle.SubHeading1,
          height = WRAP_CONTENT,
          width = MATCH_PARENT,
          padding = Padding 16 10 16 10
        } , 
        option2 {
          background = Color.white900, 
          text = secondaryButtonText,
          width = MATCH_PARENT,
          height = if secondaryButtonVisibility then WRAP_CONTENT else V 1,
          color =  Color.black650,
          strokeColor = Color.white900, 
          padding = Padding 16 10 16 10, 
          margin = Margin 0 8 0 0,
          textStyle = FontStyle.SubHeading1
        }, 
        coverImageConfig {
          visibility = VISIBLE,
          imageUrl = fetchImage FF_ASSET image,
          height = V 182,
          width = V 280
        }
      }
  in popUpConfig'

applyButtonConfig :: ReferralComponentState -> PrimaryButton.Config
applyButtonConfig state =
  let config = getAppConfig appConfig
  in PrimaryButton.config
      { textConfig { 
          text = getString APPLY
        ,  color = config.primaryTextColor
        }
      , background = config.primaryBackground
      , isClickable = state.applyButtonActive
      , alpha = if state.applyButtonActive then 1.0 else 0.4
      , id = "ReferralApplyButton"
      , margin = (MarginTop 5)
      }

skipButtonConfig :: ReferralComponentState -> PrimaryButton.Config
skipButtonConfig state =
  let config = getAppConfig appConfig
  in PrimaryButton.config
      { textConfig { 
          text = getString SKIP
        ,  color = Color.black700
        }
      , background = Color.white900
      , isClickable = true
      , alpha = 1.0
      , id = "ReferralSkipButton"
      }

referralInfoPopupConfig :: String -> String -> PopUpModal.Config
referralInfoPopupConfig primaryText' secondaryText' = let
  config' = PopUpModal.config
  popUpConfig' = config'{
      gravity = CENTER,
      margin = MarginHorizontal 24 16,
      buttonLayoutMargin = MarginBottom 10,
      cornerRadius = Corners 15.0 true true true true,
      primaryText {
        text = primaryText'
      , margin = Margin 24 16 24 0},
      secondaryText {
        text = secondaryText'
      , margin = MarginVertical 8 8
      , color = Color.black700},
      option1 { visibility = false },
      option2 {
          text = getString GOT_IT
        , width = MATCH_PARENT
        , color = Color.blue800
        , strokeColor = Color.white900
        , background = Color.white900
        , margin = MarginHorizontal 12 12
      }
  }
  in popUpConfig'