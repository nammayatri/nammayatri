{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AccountSetUpScreen.View where

import Animation as Anim
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.MenuButton as MenuButton
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, unit, not, ($), (<<<), (<>), (==), (/=), (||), (&&))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), PrestoDOM, Screen, afterRender, alignParentBottom, background, color, gravity, height, linearLayout, margin, onBackPressed, orientation, padding, relativeLayout, scrollView, singleLine, text, textView, weight, width, fontStyle, textSize, stroke, cornerRadius, imageView, imageWithFallback, visibility, onClick, editText, hint, id, pattern, hintColor, onChange, onFocus, onAnimationEnd, lineHeight, alpha, adjustViewWithKeyboard)
import Screens.AccountSetUpScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import Screens.AccountSetUpScreen.ComponentConfig
import Font.Size as FontSize
import Font.Style as FontStyle
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (mapWithIndex)
import PrestoDOM.Animation as PrestoAnim
import Resources.Constants as RSRC
import Components.StepsHeaderModel as StepsHeaderModel


screen :: ST.AccountSetUpScreenState -> Screen Action ST.AccountSetUpScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "AccountSetUpScreen"
  , globalEvents: []
  , eval
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
        [ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , margin $ MarginBottom 24
            , padding (Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom)
            , background Color.white900
            , onBackPressed push (const BackPressed)
            ][ StepsHeaderModel.view (push <<< StepsHeaderModelAC) (StepsHeaderModel.config 2)
            , Anim.screenAnimation $
              scrollView
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                ]
                [ linearLayout
                    [ height $ if EHC.os == "IOS" then V (EHC.screenHeight unit) else WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    , padding (Padding 16 0 16 0)
                    ]
                    [ nameEditTextView state push
                    , genderCaptureView state push
                    , linearLayout
                        [ height WRAP_CONTENT
                        , width MATCH_PARENT
                        , weight 1.0
                        , orientation VERTICAL
                        ]
                        []
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
            ]
            [ PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state) ]
        , if state.props.backPressed then goBackPopUpView push state else emptyTextView
        ]

goBackPopUpView :: forall w. (Action -> Effect Unit) -> ST.AccountSetUpScreenState -> PrestoDOM (Effect Unit) w
goBackPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity CENTER
    ]
    [ PopUpModal.view (push <<< PopUpModalAction) goBackPopUpModelConfig ]

------------------------ emptyTextView ---------------------------
emptyTextView :: forall w. PrestoDOM (Effect Unit) w
emptyTextView = textView []

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
          , text "(Helps driver confirm it is you)"
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
          , textSize FontSize.a_16
          , padding $ Padding 20 15 20 15
          , color Color.black800
          , onChange push $ TextChanged
          , onFocus push $ const $ EditTextFocusChanged
          , gravity LEFT
          , cornerRadius 8.0
          , fontStyle $ FontStyle.semiBold LanguageStyle
          , hint $ getString ENTER_YOUR_NAME
          , hintColor Color.black600
          , pattern "[a-zA-Z ]*,30"
          , id $ EHC.getNewIDWithTag "NameEditText"
          ] <> if EHC.os == "IOS" then [] else [onClick push $ const NameSectionClick]
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
                  , imageWithFallback "ny_ic_info,https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png" 
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
    [ textView
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , text $ getString GENDER_STR
      , color Color.black800
      , gravity LEFT
      , fontStyle $ FontStyle.regular LanguageStyle
      , singleLine true
      , textSize FontSize.a_12
      , margin $ MarginBottom 12
      ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 20 15 20 15
        , cornerRadius 8.0
        , onClick push (const ShowOptions)
        , stroke if state.props.activeField == Just ST.DropDown then "1,"<> Color.blue800 else "1,"<> Color.borderColorLight
        , gravity CENTER_VERTICAL
        ]
        [ textView
          [ text $ RSRC.getGender state.data.gender (getString SELECT_YOUR_GENDER)
          , textSize FontSize.a_16
          , fontStyle $ FontStyle.semiBold LanguageStyle
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          , color if state.data.gender == Nothing then Color.black600 else Color.black800
          ]
        , linearLayout
            [ height WRAP_CONTENT
            , weight 1.0
            , gravity RIGHT
            ]
            [ imageView
              [ imageWithFallback if state.props.genderOptionExpanded then "ny_ic_chevron_up,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_up.png" else "ny_ic_chevron_down,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_down.png"
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
    (mapWithIndex(\index item ->
       linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , onClick push $ const $ GenderSelected item.value
        , orientation VERTICAL
        ]
        [ textView
          [ text item.text
          , textSize FontSize.a_14
          , fontStyle $ FontStyle.regular LanguageStyle
          , color Color.black900
          , margin $ Margin 16 15 16 15
          ]
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

