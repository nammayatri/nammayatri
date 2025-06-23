{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.Referral.View where

import Components.Referral.Controller
import Screens.Types (ReferralComponentState, ReferralStage(..))
import Effect (Effect)
import Prelude
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import PrestoDOM
import PrestoDOM.Properties
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((<>))
import Engineering.Helpers.Commons as EHC
import PrestoDOM.Types.DomAttributes
import Common.Types.App (LazyCheck(..))
import JBridge as JB
import PrestoDOM.Animation as PrestoAnim
import Animation (translateYAnim,translateYAnimFromTop, fadeIn)
import Animation.Config (translateYAnimConfig)
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Components.PopUpModal as PopUpModal
import Data.String (length)
import Components.PrimaryButton as PrimaryButton
import Mobility.Prelude
import Data.Array (any)
import ConfigProvider

view :: forall w. (Action -> Effect Unit) -> ReferralComponentState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  ]
  [ enterReferralCodeView push state
   , referralPopUp push state
   , referralInfoPopup push state
   ]


enterReferralCodeView :: forall w. (Action -> Effect Unit) -> ReferralComponentState -> PrestoDOM (Effect Unit) w
enterReferralCodeView push state =
  PrestoAnim.animationSet [ translateYAnim translateYAnimConfig ] $
  linearLayout
  ([ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity CENTER
  , orientation VERTICAL
  , background Color.blackLessTrans
  , padding $ PaddingTop $ EHC.safeMarginTop
  , visibility $ boolToVisibility $ state.stage == ENTER_REFERRAL_CODE && state.stage /= NO_REFERRAL_STAGE
  , clickable true
  ] <> if EHC.os == "IOS" then [] else [adjustViewWithKeyboard "true"]) 
  [ 
    scrollView
    [ width MATCH_PARENT
    , height MATCH_PARENT
    ]
  [ 
     linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , cornerRadius 16.0
      , background Color.white900
      , orientation VERTICAL
      , gravity CENTER_HORIZONTAL
      , margin $ Margin 24 24 24 15
      , padding $ Padding 16 16 16 8
      , alignParentBottom "true,-1"
      ][  imageView
          [ height $ V 160 
          , width $ V $ (EHC.screenWidth unit) - 20
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_refer_friend"
          , cornerRadius 10.0
          ]
        , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , margin $ MarginTop 5
          , padding $ PaddingVertical 5 5
          , gravity CENTER
          , rippleColor Color.rippleShade
          , onClick push $ const OpenReferralProgramInfo
          ][ textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , color Color.black900
            , text $ getString HAVE_REFERRAL_CODE
            ] <> FontStyle.body7 TypoGraphy
          , imageView
            [ height $ V 30
            , width $ V 30
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_blue"
            ]
          ]
        , textView $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , color Color.black700
          , margin $ MarginTop 5
          , visibility GONE
          , text $ getString ENTER_6_DIGIT_REFERRAL_CODE_BELOW
          ] <> FontStyle.body1 TypoGraphy
        , editText $
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , margin $ MarginVertical 10 10
          , color Color.black800
          , padding $ PaddingVertical 5 5
          , letterSpacing $ PX 1.0
          , onChange push $ OnClickDone
          , gravity CENTER
          , cornerRadius 8.0
          , hint "------"
          , hintColor Color.black600
          , cursorColor Color.blue900
          , onFocus push  ReferralTextFocused
          , pattern "[a-zA-Z0-9]*,9"
          , id $ EHC.getNewIDWithTag "RefferalCode"
          , stroke $ "1," <> (if state.isInvalidCode then Color.red900 else if state.isFocused then Color.blue900 else Color.grey900)
          , afterRender (\_ -> 
                if state.stage == ENTER_REFERRAL_CODE then
                  void $ pure $ JB.showKeyboard (EHC.getNewIDWithTag "RefferalCode")
                else pure unit
                ) (const NoAction)
          ] <> FontStyle.body10 LanguageStyle 
        , textView $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , color Color.red900
          , text $ getString INVALID_CODE
          , margin $ MarginBottom 10
          , visibility $ boolToVisibility state.isInvalidCode
          ] <> FontStyle.body1 TypoGraphy
        , PrimaryButton.view (push <<< ApplyAction) (applyButtonConfig state)
        , PrimaryButton.view (push <<< SkipAction) (skipButtonConfig state)
      ]
   ]
  ]

dummyView :: forall w. PrestoDOM (Effect Unit) w
dummyView = linearLayout [ visibility GONE ] []

referralPopUp :: forall w . (Action -> Effect Unit) -> ReferralComponentState -> PrestoDOM (Effect Unit) w
referralPopUp push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity CENTER
  , orientation VERTICAL
  , background Color.blackLessTrans
  , clickable true
  , visibility $ boolToVisibility $ any (_ == state.stage) [INVALID_POPUP, APPLIED_POPUP, ALREADY_APPLIED_POPUP] && state.stage /= NO_REFERRAL_STAGE
  ][  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , background Color.blackLessTrans
      ][PopUpModal.view (push <<< PopUpModalAction) (referralPopUpConfig state)]
   ]

referralInfoPopup :: forall w . (Action -> Effect Unit) -> ReferralComponentState -> PrestoDOM (Effect Unit) w
referralInfoPopup push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.blackLessTrans
  , visibility $ boolToVisibility $ state.showReferredUserInfoPopup || state.showReferralProgramInfoPopup
  , clickable true
  ][PopUpModal.view (push <<< action) (referralInfoPopupConfig primaryText secondaryText)]
  where
    config = getAppConfig appConfig
    action = if state.showReferredUserInfoPopup then ReferredUserInfo else ReferralProgramInfo
    primaryText = if state.showReferredUserInfoPopup then 
                    getString REFERRED_USERS
                  else
                    getString WHAT_IS_REFERRAL_PROGRAM
    secondaryText = if state.showReferredUserInfoPopup then
                      getString (USERS_WHO_DOWNLOAD_APP_AND_COMPLETE_THEIR_FIRST_RIDE_USING_REFERRAL_CODE config.appData.name)
                    else 
                      getString (THE_REFERRAL_PROGRAM_INCENTIVISES_DRIVERS_TO_ACCEPT_MORE_RIDES config.appData.name)
