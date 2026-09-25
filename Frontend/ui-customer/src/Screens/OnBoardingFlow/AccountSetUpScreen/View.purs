{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AccountSetUpScreen.View where

import Screens.AccountSetUpScreen.ComponentConfig

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.GenericRadioButton as GenericRadioButton
import Components.MenuButton as MenuButton
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.StepsHeaderModel as StepsHeaderModel
import Components.SelectListModal as SelectListModal
import Data.Array as DA
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Data.String as DS
import Prelude (Unit, const, unit, not, ($), (<<<), (<>), (==), (/=), (||), (&&), (-), (>=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), Accessiblity(..), PrestoDOM, Screen, afterRender, alignParentBottom, background, color, gravity, height, linearLayout, margin, onBackPressed, orientation, padding, relativeLayout, scrollView, singleLine, text, textView, weight, width, fontStyle, textSize, stroke, cornerRadius, imageView, imageWithFallback, visibility, onClick, editText, hint, id, pattern, hintColor, onChange, onFocus, onAnimationEnd, lineHeight, alpha, adjustViewWithKeyboard, accessibilityHint ,accessibility)
import Screens.AccountSetUpScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import Screens.AccountSetUpScreen.ComponentConfig
import Screens.Types (ReferralEnum(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, map, not, show, unit, ($), (&&), (/=), (<<<), (<>), (==), (||))
import PrestoDOM (InputType(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), inputType, clickable, progressBar, adjustViewWithKeyboard, afterRender, alignParentBottom, alpha, background, color, cornerRadius, editText, fontStyle, gravity, height, hint, hintColor, id, imageView, imageWithFallback, lineHeight, linearLayout, margin, onAnimationEnd, onBackPressed, onChange, onClick, onFocus, orientation, padding, pattern, relativeLayout, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, progressBarColor)
import PrestoDOM.Animation as PrestoAnim
import Resources.Constants as RSRC
import Screens.AccountSetUpScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Debug (spy)
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Properties (cornerRadii)
import Data.String as DS
import Components.CommonComponentConfig as CommonComponentConfig

screen :: ST.AccountSetUpScreenState -> Screen Action ST.AccountSetUpScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "AccountSetUpScreen"
  , globalEvents: []
  , eval : (\state  action -> do
      let _ = spy "AccountSetupScreen state -----" state
      let _ = spy "AccountSetupScreen--------action" action
      eval state action)
  }

view ::
  forall w.
  (Action -> Effect Unit) -> ST.AccountSetUpScreenState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , background Color.white900
        , onBackPressed push (const BackPressed)
        , afterRender push (const AfterRender)
        ]
        ([ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , accessibility if state.props.backPressed then DISABLE_DESCENDANT else DISABLE
            , margin $ MarginBottom 24
            , padding (Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom)
            , background Color.white900
            , onBackPressed push (const BackPressed)
            ][ StepsHeaderModel.view (push <<< StepsHeaderModelAC) (StepsHeaderModel.stepsHeaderData 2)
            , Anim.screenAnimation $
              scrollView
                [ width MATCH_PARENT
                , height $ if EHC.os == "IOS" then V (EHC.screenHeight unit) else MATCH_PARENT
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    , padding (Padding 16 0 16 75)
                    ]
                    [ nameEditTextView state push
                    , genderCaptureView state push
                    , referralView state push
                    , if (not state.props.genderOptionExpanded) then disabilityOptionView state push else textView[]
                    , linearLayout
                        [ width MATCH_PARENT
                        , weight 1.0
                        , orientation VERTICAL
                        ][]
                    ]
                ]
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , alignParentBottom "true,-1"
            , background Color.white900
            , padding (Padding 16 0 16 26)
            , adjustViewWithKeyboard "true"
            , accessibility if state.props.backPressed then DISABLE_DESCENDANT else DISABLE
            ]
            [ PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state) ]
        , if state.props.backPressed then goBackPopUpView push state else emptyTextView
        , if state.props.isSpecialAssistList then specialAssistanceView state push else emptyTextView
        ])

goBackPopUpView :: forall w. (Action -> Effect Unit) -> ST.AccountSetUpScreenState -> PrestoDOM (Effect Unit) w
goBackPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity CENTER
    ]
    [ PopUpModal.view (push <<< PopUpModalAction) (goBackPopUpModelConfig state) ]

------------------------ emptyTextView ---------------------------
emptyTextView :: forall w. PrestoDOM (Effect Unit) w
emptyTextView = textView []

referralView :: forall w. ST.AccountSetUpScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
referralView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 30
    ]
    [ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginBottom 12
      ][ textView $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ getString REFERRAL_CODE
          , singleLine true
          , color Color.greyTextColor
          ] <> FontStyle.body3 TypoGraphy,
          textView $ 
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text (getString OPTIONAL_BRACKETTED)
          , singleLine true
          , color Color.black600
          , margin $ MarginLeft 7
          ] <> FontStyle.body3 TypoGraphy

      ]
    , linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , cornerRadius 8.0
        , gravity CENTER_VERTICAL
        , stroke if state.props.activeField == Just ST.ReferralSection then "1,"<> Color.blue800 else "1,"<> Color.borderColorLight
        ]
        [ editText $ 
          [ height MATCH_PARENT
          , width WRAP_CONTENT
          , weight 1.0
          , padding $ Padding 20 15 20 15
          , color Color.black800
          , onChange push $ ReferralTextChanged
          , onFocus push $ ReferralTextFocusChanged
          , gravity LEFT
          , cornerRadius 8.0
          , hint $ getString ENTER_REFERRAL_CODE_
          , hintColor Color.black600
          , alpha if state.data.isReferred == Verified then 0.4 else 1.0
          , pattern "[a-zA-Z0-9]*,9"
          , inputType if state.data.isReferred == Verified then Disabled else TypeText
          , id $ EHC.getNewIDWithTag "ReferralEditText"
          , accessibility ENABLE
          , accessibilityHint $ 
              case state.data.isReferred of 
                Verified -> "Referral code verified" 
                NotVerified -> "Enter referral code" 
                ReferralFailed -> "Referral code verification failed" 
                Verifying -> "Verifying referral code"
          ] <> FontStyle.subHeading1 LanguageStyle 
            <> if EHC.os == "IOS" then [] else [onClick push $ const ReferralSectionClick] 
            <> if state.data.isReferred == Verified then [text state.data.referralCode] else []
        , linearLayout
          [ height MATCH_PARENT
          , width WRAP_CONTENT
          , cornerRadius 20.0
          , gravity CENTER_VERTICAL
          , margin $ Margin 10 10 10 10
          , padding $ Padding 12 4 12 4
          , onClick push (const VerifyReferralClick)
          , clickable (DS.length state.data.referralCode >= 6 && state.data.isReferred == NotVerified)
          , alpha case state.data.isReferred of
                    ST.NotVerified -> if state.data.referralTextFocussed && DS.length state.data.referralCode >= 6 then 1.0 else 0.4
                    _ -> if state.data.referralTextFocussed then 1.0 else 0.4
          , background case state.data.isReferred of
                          ST.Verified -> Color.green900
                          ST.NotVerified -> Color.blue800
                          ST.ReferralFailed -> Color.red900
                          ST.Verifying -> Color.black700
          ]
          [ 
            imageView $
            [ width $ V (case state.data.isReferred of
                              ST.NotVerified -> 0
                              ST.Verifying -> 0
                              _ -> 18)
            , height $ V (case state.data.isReferred of
                              ST.NotVerified -> 0
                              ST.Verifying -> 0
                              _ -> 18)
            , margin $ Margin 0 2 2 2
            , imageWithFallback $ case state.data.isReferred of
                                    ST.Verified -> fetchImage COMMON_ASSET "ny_ic_checkcircle"
                                    ST.NotVerified -> fetchImage COMMON_ASSET "ny_ic_info_white"
                                    ST.ReferralFailed -> fetchImage COMMON_ASSET "ny_ic_subtract"
                                    ST.Verifying -> fetchImage COMMON_ASSET "ny_ic_info_white" 
            , visibility case state.data.isReferred of
                              ST.NotVerified -> GONE
                              ST.Verifying -> GONE
                              _ -> VISIBLE
            ]
            ,progressBar $
            [ width $ V 16
            , height $ V 16
            , margin $ Margin 0 2 2 2
            , visibility case state.data.isReferred of 
                            ST.Verifying -> VISIBLE
                            _ -> GONE
            , progressBarColor Color.white900
            ]
            ,textView $ 
            [ height MATCH_PARENT
            , width WRAP_CONTENT
            , margin $ Margin 0 0 0 2
            , text case state.data.isReferred of
                      ST.Verified -> getString VERIFIED
                      ST.NotVerified -> getString VERIFY
                      ST.ReferralFailed -> getString FAILED_STR
                      ST.Verifying -> getString VERIFYING
            , singleLine true
            , color Color.white900
            ] <> FontStyle.subHeading3 TypoGraphy
          ]
        ]
    , linearLayout 
        [ height $ V 18
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        ][  linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation HORIZONTAL
              , gravity CENTER_VERTICAL
              ][
                textView $
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text case state.data.isReferred of 
                            ST.Verified -> getString REFERRAL_CODE_VERIFIED
                            ST.NotVerified -> getString ENTER_6_DIGIT_REFERRAL_CODE
                            ST.ReferralFailed -> getString REFERRAL_CODE_FAILED
                            ST.Verifying -> getString ENTER_6_DIGIT_REFERRAL_CODE
                  , color Color.black600
                  -- , fontStyle $ FontStyle.bold LanguageStyle
                  , gravity LEFT
                  , margin $ Margin 0 0 0 0
                  , lineHeight "28"
                  , singleLine true
                  , alpha 1.0
                  ]  <> FontStyle.body3 TypoGraphy
              ]           
          ]
    ]

nameEditTextView :: forall w. ST.AccountSetUpScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
nameEditTextView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 30
    ]
    [ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginBottom 12
      ][ textView $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ getString FULL_NAME
          , singleLine true
          , color Color.greyTextColor
          ] <> FontStyle.body3 TypoGraphy,
          textView $ 
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text (getString HELPS_DRIVER_CONFIRM_ITS_YOU)
          , singleLine true
          , color Color.black600
          , margin $ MarginLeft 7
          ] <> FontStyle.body3 TypoGraphy

      ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , cornerRadius 8.0
        , gravity CENTER_VERTICAL
        , stroke if state.props.activeField == Just ST.NameSection then "1,"<> Color.blue800 else "1,"<> Color.borderColorLight
        ]
        [ editText $ 
          [ height MATCH_PARENT
          , width WRAP_CONTENT
          , weight 1.0
          , padding $ Padding 20 15 20 15
          , color Color.black800
          , onChange push $ TextChanged
          , onFocus push $ EditTextFocusChanged
          , gravity LEFT
          , cornerRadius 8.0
          , hint $ getString ENTER_YOUR_NAME
          , hintColor Color.black600
          , pattern "[a-zA-Z. ]*,30"
          , id $ EHC.getNewIDWithTag "NameEditText"
          ] <> FontStyle.subHeading1 LanguageStyle 
            <> if EHC.os == "IOS" then [] else [onClick push $ const NameSectionClick]
        ]
    , linearLayout 
        [ height $ V 18
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        ][  linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation HORIZONTAL
              , gravity CENTER_VERTICAL
              , visibility if state.data.nameErrorMessage /= Nothing then VISIBLE else GONE
              ][  imageView $
                  [ width $ V 20
                  , height MATCH_PARENT
                  , padding $ Padding 0 5 0 3
                  , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info" 
                  ]
                , textView $
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text case state.data.nameErrorMessage of 
                      Just ST.INVALID_NAME -> getString NAME_SHOULD_BE_MORE_THAN_2_CHARACTERS
                      _ -> ""
                  , color Color.black600
                  -- , fontStyle $ FontStyle.bold LanguageStyle
                  , gravity LEFT
                  , margin $ Margin 0 0 0 0
                  , lineHeight "28"
                  , singleLine true
                  , alpha 1.0
                  ]  <> FontStyle.body3 TypoGraphy
              ]           
          ]
    ]


------------------------ genderCaptureView ---------------------------

genderCaptureView :: forall w. ST.AccountSetUpScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
genderCaptureView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginTop 14
    , orientation VERTICAL
    ] $
    [ textView $
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , text $ getString GENDER_STR
      , color Color.black800
      , gravity LEFT
      , singleLine true
      , margin $ MarginBottom 12
      ] <> FontStyle.body3 TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 20 15 20 15
        , cornerRadius 8.0
        , onClick push (const ShowOptions)
        , stroke if state.props.activeField == Just ST.DropDown then "1,"<> Color.blue800 else "1,"<> Color.borderColorLight
        , gravity CENTER_VERTICAL
        ]
        [ textView $
          [ text $ RSRC.getGender state.data.gender (getString SELECT_YOUR_GENDER)
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          , color if state.data.gender == Nothing then Color.black600 else Color.black800
          , accessibility ENABLE
          , accessibilityHint $ if state.data.gender == Nothing then "Select your gender : Drop-Down menu" else "Gender Selected : " <> RSRC.getGender state.data.gender (getString SELECT_YOUR_GENDER) <> " : " <>  if state.props.genderOptionExpanded then "Double Tap To Collapse DropDown" else " Double Tap To Expand DropDown"
          ] <> FontStyle.subHeading1 TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , weight 1.0
            , gravity RIGHT
            ]
            [ imageView
              [ imageWithFallback $ fetchImage FF_COMMON_ASSET $ if state.props.genderOptionExpanded then "ny_ic_chevron_up" else "ny_ic_chevron_down"
              , height $ V 24
              , width $ V 15
              ]
            ]

        ]

    ] <> (if state.props.expandEnabled then [ genderOptionsView state push] else [])



genderOptionsView :: forall w. ST.AccountSetUpScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
genderOptionsView state push =
  PrestoAnim.animationSet
  ([] <> if EHC.os == "IOS" then
        [Anim.fadeIn state.props.genderOptionExpanded
        , Anim.fadeOut  (not state.props.genderOptionExpanded)]
        else
          [Anim.listExpandingAnimation $  translateFullYAnimWithDurationConfigs state] )
            $
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginTop 8
    , background Color.grey700
    , orientation VERTICAL
    , visibility $ if (state.props.genderOptionExpanded || state.props.showOptions) then VISIBLE else GONE
    , onAnimationEnd push AnimationEnd
    , stroke $ "1,"<>Color.grey900
    , cornerRadius 8.0
    ]
    (DA.mapWithIndex(\index item ->
       linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , onClick push $ const $ GenderSelected item.value
        , orientation VERTICAL
        ]
        [ textView $
          [ text item.text
          , color Color.black900
          , margin $ Margin 16 15 16 15
          ] <> FontStyle.paragraphText TypoGraphy
        , linearLayout
          [ height $ V 1
          , width MATCH_PARENT
          , background Color.grey900
          , visibility if index == 3 then GONE else VISIBLE
          , margin $ MarginHorizontal 16 16
          ][]
        ]
       )(genderOptionsArray state)

    )

genderOptionsArray :: ST.AccountSetUpScreenState ->  Array {text :: String , value :: ST.Gender}
genderOptionsArray _ =
  [ {text : (getString FEMALE) , value : ST.FEMALE}
  , {text : (getString MALE) , value : ST.MALE}
  , {text : (getString OTHER) , value : ST.OTHER}
  , {text : (getString PREFER_NOT_TO_SAY) , value : ST.PREFER_NOT_TO_SAY}
  ]

disabilityOptionView :: forall w. ST.AccountSetUpScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
disabilityOptionView state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin (MarginTop 28)
  ]$[ textView $
    [ text (getString ARE_YOU_A_PERSON_WITH_DISABILITY)
    , height WRAP_CONTENT
    , width WRAP_CONTENT
    , margin $ MarginBottom 16
    ] <> FontStyle.body3 TypoGraphy
  ] <> (DA.mapWithIndex (\index item -> GenericRadioButton.view (push <<< GenericRadioButtonAC) (getRadioButtonConfig index item state)) [ (getString NO), (getString YES)])
    <> if state.data.disabilityOptions.activeIndex == 1 then [claimerView state push] else []

specialAssistanceView :: forall w. ST.AccountSetUpScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
specialAssistanceView state push = do 
  SelectListModal.view (push <<< SpecialAssistanceListAC) (CommonComponentConfig.accessibilityListConfig state.data.disabilityOptions (fromMaybe "" state.data.disabilityOptions.otherDisabilityReason) state.data.config)

claimerView :: forall w. ST.AccountSetUpScreenState  -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
claimerView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding $ Padding 20 16 20 16
  , cornerRadius 12.0 
  , background Color.pink
  , alignParentBottom "true,-1"
  , gravity CENTER 
  ][  textView
      ([ text (getString DISABILITY_CLAIMER_TEXT)
      , height WRAP_CONTENT
      , width WRAP_CONTENT
      ] <> FontStyle.body3 TypoGraphy)
  ]