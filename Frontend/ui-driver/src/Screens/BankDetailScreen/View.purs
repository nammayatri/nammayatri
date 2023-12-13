{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.BankDetailScreen.View where

import Prelude (Unit, const, ($), (<<<), (<>))
import PrestoDOM (BottomSheetState(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), editText, frameLayout, linearLayout, onBackPressed, onChange, scrollView, textView, afterRender, alpha, background, color, cornerRadius, fontStyle, gravity, height, hint, id, margin, orientation, padding, pattern, stroke, text, textSize, visibility, weight, width)
import PrestoDOM.Properties (sheetState) as PP
import Effect (Effect)
import Screens.BankDetailScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Style as FontStyle
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Components.PrimaryButton as PrimaryButton
import Components.RegistrationModal.View as RegistrationModal
import Components.OnboardingHeader.View as OnboardingHeader
import Animation as Anim
import Language.Strings (getString)
import Language.Types (STR(..))
import Common.Types.App
import Screens.BankDetailScreen.ComponentConfig

screen :: ST.BankDetailScreenState -> Screen Action ST.BankDetailScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "BankDetailsScreen"
  , globalEvents: []
  , eval
  }

view ::
  forall w.
  (Action -> Effect Unit) ->
  ST.BankDetailScreenState ->
  PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ frameLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , afterRender push (const AfterRender)
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , PP.sheetState EXPANDED
            , background Color.white900
            , onBackPressed push (const BackPressed)
            ]
            [ onboardingHeaderView state push
            , linearLayout
                [ width MATCH_PARENT
                , weight 1.0
                , orientation VERTICAL
                ]
                [ scrollView
                    [ width MATCH_PARENT
                    , height MATCH_PARENT
                    ]
                    [ linearLayout
                        [ height MATCH_PARENT
                        , width MATCH_PARENT
                        , orientation VERTICAL
                        ]
                        [ textView
                            ( [ height $ V 40
                              , width WRAP_CONTENT
                              , text (getString ADD_BANK_DETAILS)
                              , color Color.greyTextColor
                              , margin (Margin 16 33 0 0)
                              ]
                                <> FontStyle.h1 TypoGraphy
                            )
                        , textView
                            ( [ text (getString EARNINGS_CREDITED_IN_ACCOUNT)
                              , margin (MarginLeft 16)
                              , color Color.black700
                              ]
                                <> FontStyle.body3 TypoGraphy
                            )
                        , enterBeneficiaryNumber state push
                        , reEnterBeneficiaryNumber state push
                        , enterIfsc state push
                        ]
                    ]
                ]
            , nextButton state push
            ]
        , if state.props.openRegistrationModal then
            linearLayout
              [ width MATCH_PARENT
              , height MATCH_PARENT
              ]
              [ registrationModalView state push ]
          else
            linearLayout [] []
        ]

----------------------------------------------- benificiaryNumber ----------------------------------------------
enterBeneficiaryNumber :: ST.BankDetailScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
enterBeneficiaryNumber state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , padding (Padding 20 30 20 10)
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            ]
            [ textView
                ( [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text (getString BENIFICIARY_NUMBER)
                  , color Color.greyTextColor
                  , margin (MarginBottom 10)
                  ]
                    <> FontStyle.body3 TypoGraphy
                )
            , textView
                $
                 -- (Required Field Indication) [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text "*"
                  , color Color.warningRed
                  , alpha 0.8
                  , visibility GONE
                  , margin (MarginBottom 10)
                  ]
                <> FontStyle.h2 TypoGraphy
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , stroke ("1,#E4E4E4")
            , cornerRadius 4.0
            ]
            [ textView
                [ width $ V 20
                , height WRAP_CONTENT
                ]
            , editText
                ( [ width MATCH_PARENT
                  , height (V 60)
                  , padding (Padding 0 10 20 10)
                  , color Color.greyTextColor
                  , text state.props.inputData
                  , hint (getString ENTER_ACCOUNT_NUMBER)
                  , weight 1.0
                  , cornerRadius 4.0
                  --   , inputType Password -- TODO
                  , pattern "[0-9]"
                  , stroke ("1,#FFFFFF")
                  , id (EHC.getNewIDWithTag "beneficiaryNumber")
                  , onChange push (const BeneficiaryNumber state.props.inputData)
                  ]
                    <> FontStyle.subHeading1 TypoGraphy
                )
            ]
        ]
    ]

----------------------------------------------- ReEnterBenificiaryNumber ----------------------------------------------
reEnterBeneficiaryNumber :: ST.BankDetailScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
reEnterBeneficiaryNumber state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , padding (Padding 20 30 20 10)
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            ]
            [ textView
                ( [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text (getString RE_ENTER_BENIFICIARY_NUMBER)
                  , color Color.greyTextColor
                  , margin (MarginBottom 10)
                  ]
                    <> FontStyle.body3 TypoGraphy
                )
            , textView
                $
                 -- (Required Field Indication) [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text "*"
                  , color Color.warningRed
                  , alpha 0.8
                  , visibility GONE
                  , margin (MarginBottom 10)
                  ]
                <> FontStyle.h2 TypoGraphy
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , stroke ("1,#E4E4E4")
            , cornerRadius 4.0
            ]
            [ textView
                [ width $ V 20
                , height WRAP_CONTENT
                ]
            , editText
                ( [ width MATCH_PARENT
                  , height (V 60)
                  , padding (Padding 0 10 20 10)
                  , color Color.greyTextColor
                  , text state.props.inputData
                  , hint (getString RE_ENTER_BENIFICIARY_NUMBER)
                  , weight 1.0
                  , cornerRadius 4.0
                  , pattern "[0-9]"
                  , stroke ("1,#FFFFFF")
                  , id (EHC.getNewIDWithTag "verifybeneficiary")
                  , onChange push (const ReEnterBeneficiaryNumber state.props.inputData)
                  ]
                    <> FontStyle.subHeading1 TypoGraphy
                )
            ]
        ]
    ]

----------------------------------------------- IFSC CODE ----------------------------------------------
enterIfsc :: ST.BankDetailScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
enterIfsc state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , padding (Padding 20 30 20 10)
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            ]
            [ textView
                ( [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text (getString IFSC_CODE)
                  , color Color.greyTextColor
                  , margin (MarginBottom 10)
                  ]
                    <> FontStyle.body3 TypoGraphy
                )
            , textView
                $
                 -- (Required Field Indication) [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text "*"
                  , color Color.warningRed
                  , alpha 0.8
                  , visibility GONE
                  , margin (MarginBottom 10)
                  ]
                <> FontStyle.h2 TypoGraphy
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , stroke ("1,#E4E4E4")
            , cornerRadius 4.0
            ]
            [ textView
                [ width $ V 20
                , height WRAP_CONTENT
                ]
            , editText
                ( [ width MATCH_PARENT
                  , height (V 60)
                  , padding (Padding 0 10 20 10)
                  , color Color.greyTextColor
                  , text state.props.inputData
                  , fontStyle $ FontStyle.bold LanguageStyle
                  , hint (getString ENTER_IFSC_CODE)
                  , weight 1.0
                  , cornerRadius 4.0
                  , pattern "[a-z, 0-9, A-Z]"
                  , stroke ("1,#FFFFFF")
                  , id (EHC.getNewIDWithTag "ifscCode")
                  , onChange push (const IFSCNumber state.props.inputData)
                  ]
                    <> FontStyle.subHeading1 TypoGraphy
                )
            ]
        ]
    ]

---------------------------------------------------------- nextButton -------------------------------------------------------
nextButton :: ST.BankDetailScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
nextButton state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER
    ]
    [ PrimaryButton.view (push <<< PrimaryButtonAction) (primaryButtonConfig state) ]

onboardingHeaderView :: ST.BankDetailScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
onboardingHeaderView state push =
  OnboardingHeader.view (push <<< OnboardingHeaderAction)
    ( { stepNumber: "3"
      , barNumber: 3
      }
    )

registrationModalView :: ST.BankDetailScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
registrationModalView state push =
  RegistrationModal.view (push <<< RegistrationModalAction)
    ( { openRegistrationModal: state.props.openRegistrationModal
      }
    )
