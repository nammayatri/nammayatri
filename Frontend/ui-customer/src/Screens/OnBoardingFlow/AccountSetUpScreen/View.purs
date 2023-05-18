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
import Prelude (Unit, const, unit, not, ($), (<<<), (<>), (==), (/=), (||))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), PrestoDOM, Screen, afterRender, alignParentBottom, background, color, gravity, height, linearLayout, margin, onBackPressed, orientation, padding, relativeLayout, scrollView, singleLine, text, textView, weight, width, fontStyle, textSize, stroke, cornerRadius, imageView, imageWithFallback, visibility, onClick, editText, hint, id, pattern, hintColor, onChange, onFocus, onAnimationEnd)
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
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))


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
  Anim.screenAnimation
    $ relativeLayout
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
            , margin (Margin 0 16 0 24)
            , padding (Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom)
            , background Color.white900
            , onBackPressed push (const BackPressed)
            ]
            [ GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig)
            , scrollView
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                ]
                [ linearLayout
                    [ height $ if EHC.os == "IOS" then V (EHC.screenHeight unit) else WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    , padding (Padding 16 0 16 0)
                    ]
                    [ textView
                        $ [ height WRAP_CONTENT
                          , width MATCH_PARENT
                          , text (getString SET_UP_YOUR_ACCOUNT)
                          , color Color.black800
                          , gravity LEFT
                          , singleLine true
                          ]
                        <> FontStyle.h1 TypoGraphy
                    , nameEditTextView state push
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
    [ PopUpModal.view (push <<< PopUpModalAction) (goBackPopUpModelConfig state) ]

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
    [ textView $
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , text (getString HOW_SHOULD_WE_ADDRESS_YOU)
      , singleLine true
      , color Color.greyTextColor
      , gravity LEFT
      , margin $ MarginBottom 12
      ] <> FontStyle.body3 TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , cornerRadius 8.0
        , gravity CENTER_VERTICAL
        , stroke $ "1,"<> Color.borderColorLight
        ]
        [ editText $ 
          [ height MATCH_PARENT
          , width WRAP_CONTENT
          , weight 1.0
          , padding $ Padding 20 15 20 15
          , color Color.black800
          , onChange push $ TextChanged
          , onFocus push $ const $ EditTextFocusChanged
          , gravity LEFT
          , cornerRadius 8.0
          , hint $ getString ENTER_YOUR_NAME
          , hintColor Color.black600
          , pattern "[a-zA-Z ]*,30"
          , id $ EHC.getNewIDWithTag "NameEditText"
          ] <> FontStyle.subHeading1 LanguageStyle
        ]
    ]


------------------------ genderCaptureView ---------------------------

genderCaptureView :: forall w. ST.AccountSetUpScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
genderCaptureView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginTop 32
    , orientation VERTICAL
    ] $
    [ textView $
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , text $ getString HOW_DO_YOU_IDENTIFY_YOURSELF
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
        , stroke $ "1,"<> Color.borderColorLight
        , gravity CENTER_VERTICAL
        ]
        [ textView $
          [ text $ RSRC.getGender state.data.gender (getString SELECT_YOUR_GENDER)
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          , color if state.data.gender == Nothing then Color.black600 else Color.black800
          ] <> FontStyle.subHeading1 TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , weight 1.0
            , gravity RIGHT
            ]
            [ imageView
              [ imageWithFallback $ if state.props.genderOptionExpanded then "ny_ic_chevron_up," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_up.png" else "ny_ic_chevron_down," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_down.png"
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

