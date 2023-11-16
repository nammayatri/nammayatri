{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AddVehicleDetailsScreen.Views where

import Common.Types.App
import Data.Maybe
import Screens.AddVehicleDetailsScreen.ComponentConfig

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericMessageModal.View as GenericMessageModal
import Components.PrimaryButton as PrimaryButton
import Components.ReferralMobileNumber.View as ReferralMobileNumber
import Components.TutorialModal.View as TutorialModal
import Data.String as DS
import Effect (Effect)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import PaymentPage (consumeBP)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (getValueFromConfig)
import Prelude ((<>))
import Prelude (Unit, bind, const, pure, unit, ($), (<<<), (<>), (==), not, (>=), (&&), (/=))
import PrestoDOM (BottomSheetState(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alignParentBottom, alignParentRight, alpha, background, clickable, color, cornerRadius, editText, ellipsize, fontStyle, frameLayout, gravity, height, hint, id, imageUrl, imageView, imageWithFallback, inputTypeI, layoutGravity, linearLayout, margin, maxLines, onBackPressed, onChange, onClick, orientation, padding, pattern, relativeLayout, scrollView, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM.Properties as PP
import PrestoDOM.Types.DomAttributes as PTD
import Screens.AddVehicleDetailsScreen.Controller (Action(..), eval, ScreenOutput, validateRegistrationNumber)
import Screens.Types (AddVehicleDetailsScreenState)
import Styles.Colors as Color
import Effect.Uncurried (runEffectFn1)

screen :: AddVehicleDetailsScreenState -> Screen Action AddVehicleDetailsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "AddVehicleDetailsScreen"
  , globalEvents : [(\push -> do
    _ <- JB.storeCallBackImageUpload push CallBackImageUpload
    _ <- runEffectFn1 consumeBP unit
    pure $ pure unit
  )]
  , eval
  }

view
  :: forall w
  . (Action -> Effect Unit)
  -> AddVehicleDetailsScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  frameLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , PP.sheetState EXPANDED
      , background Color.white900
      , onBackPressed push (const BackPressed state.props.openRCManual)
      , onClick push (const ScreenClick)
      , afterRender push (const AfterRender)
      ][  headerLayout state push
        , linearLayout
          [ width MATCH_PARENT
          , weight 1.0
          , orientation VERTICAL
          ][  scrollView
              [ height MATCH_PARENT
              , width MATCH_PARENT
              ][ linearLayout
                  [ height MATCH_PARENT
                  , width MATCH_PARENT
                  , orientation VERTICAL
                  ][  textView
                      ([ height $ V 40
                      , width WRAP_CONTENT
                      , text (getString ADD_VEHICLE_DETAILS)
                      , color Color.black800
                      , margin (Margin 16 27 0 0)
                      ] <> FontStyle.h1 TypoGraphy )
                    , textView
                      ([ width WRAP_CONTENT
                      , textFromHtml (getString PROVIDE_DATE_OF_REGISTRATION_TEXT)
                      , color Color.black800
                      , margin (Margin 16 20 16 20)
                      , visibility if state.data.dateOfRegistration == Nothing then GONE else VISIBLE
                      ] <> FontStyle.subHeading2 TypoGraphy)
                    , vehicleRegistrationNumber state push
                    , dateOfRCRegistrationView push state
                    , uploadRC state push 
                    , referralAppliedView state push
                    , applyReferralView state push
                    ]
                ]
          ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER
          , orientation VERTICAL
          ][ textView
           [ width MATCH_PARENT
           , height WRAP_CONTENT
           , text state.data.errorMessage
           , visibility $ if state.props.errorVisibility then VISIBLE else GONE
           , color Color.red
           , padding( PaddingHorizontal 20 20)
           , margin (MarginBottom 10)
           ]
           , PrimaryButton.view (push <<< PrimaryButtonAction) (primaryButtonConfig state)]
    ]   
    , if state.props.openRCManual then 
        linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        ] [TutorialModal.view (push <<< TutorialModalAction) {imageUrl : fetchImage FF_ASSET "ny_ic_vehicle_registration_card"}] else linearLayout [][]
    , if state.props.openRegistrationDateManual then 
        linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        ] [TutorialModal.view (push <<< TutorialModalAction) {imageUrl : fetchImage FF_ASSET "ny_ic_date_of_registration"}] else linearLayout [][]
    , if state.props.limitExceedModal then 
        linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        ] [GenericMessageModal.view (push <<< GenericMessageModalAction) {text : (getString ISSUE_WITH_RC_IMAGE), openGenericMessageModal : state.props.limitExceedModal, buttonText : (getString NEXT) }] else linearLayout [][]

    , if state.props.openReferralMobileNumber then
        linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT] 
        [ReferralMobileNumber.view (push <<< ReferralMobileNumberAction) (referalNumberConfig state)] else linearLayout [][]
    ] 

applyReferralView :: AddVehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w 
applyReferralView state push = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity BOTTOM
  , orientation HORIZONTAL
  , alignParentBottom "true,-1"
  , padding (Padding 20 0 16 20)
  , margin (MarginBottom 40)
  , visibility if state.props.referralViewstatus then GONE else VISIBLE
  , cornerRadius 8.0
  ][  textView $
      [ text (getString HAVE_A_REFERRAL)
      , color Color.black700
      , margin (MarginRight 8)
      ] <> FontStyle.body1 TypoGraphy
    , textView $
      [ text (getString ADD_HERE)
      , color Color.blue900
      , onClick push (const ReferralMobileNumber)
      ] <> FontStyle.body1 TypoGraphy]

referralAppliedView :: AddVehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w 
referralAppliedView state push = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , gravity BOTTOM
  , visibility if state.props.referralViewstatus then VISIBLE else GONE
  ][  relativeLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.grey700
      , padding (Padding 20 16 20 16 )
      , cornerRadius 8.0
      , gravity CENTER_VERTICAL
      , margin (Margin 16 0 16 33)
      ][  linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , gravity CENTER
          , orientation HORIZONTAL
          ][  imageView
              [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_check_green"
              , width (V 11)
              , height (V 8)
              ]
            , textView $
              [ margin (MarginLeft 11)
              , text (getString REFERRAL_APPLIED)
              , color Color.darkMint
              ] <> FontStyle.body1 TypoGraphy
            ]
        , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation HORIZONTAL
          , alignParentRight "true,-1"
          ][  textView $
              [ margin (MarginRight 8)
              , text state.data.referral_mobile_number
              , color Color.black800
              ] <> FontStyle.body1 TypoGraphy
            , textView $
              [ text (getString SMALLEDIT)
              , color Color.blue900
              , onClick push (const ReferralMobileNumber)
              ] <> FontStyle.body1 TypoGraphy
            ]
        ]
    ]


----------------------------------------------- vehicleRegistrationNumber ----------------------------------------------


vehicleRegistrationNumber :: AddVehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
vehicleRegistrationNumber state push = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , padding (Padding 20 32 20 0)
  , visibility if state.data.dateOfRegistration /= Nothing then GONE else VISIBLE
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][  linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          ][  textView
              ([ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text (getString VEHICLE_REGISTRATION_NUMBER)
              , color Color.greyTextColor
              , margin (MarginBottom 10)
              ] <> FontStyle.body3 TypoGraphy)
          ,   textView $ -- (Required Field Indication)
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text "*"
              , color Color.warningRed
              , alpha 0.8
              , visibility GONE
              , margin (MarginBottom 10)
              ] <> FontStyle.h2 TypoGraphy
            ] 
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , stroke ("1," <> if ((DS.length state.data.vehicle_registration_number >= 2) && not validateRegistrationNumber (DS.take 2 state.data.vehicle_registration_number)) then Color.warningRed else Color.borderColorLight) 
          , cornerRadius 4.0
          ][  textView
              [ width $ V 20
              , height WRAP_CONTENT
              ]
            , editText
              ([ width MATCH_PARENT
              , height WRAP_CONTENT
              , padding (Padding 0 17 0 17)
              , color Color.greyTextColor
              , text state.props.input_data
              , hint (getString ENTER_VEHICLE_NO)
              , weight 1.0
              , cornerRadius 4.0
              , pattern "[0-9A-Z]*,11"
              , stroke ("1," <> Color.white900)
              , id (EHC.getNewIDWithTag "VehicleRegistrationNumber")
              , onChange push (const VehicleRegistrationNumber state.props.input_data)
              , inputTypeI 4097
              ] <> FontStyle.subHeading1 TypoGraphy)
            ]
          , textView $ -- (Error Indication)
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text (getString CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER)
            , color Color.warningRed
            , fontStyle $ FontStyle.regular LanguageStyle
            , margin (MarginTop 10)
            , visibility if ((DS.length state.data.vehicle_registration_number >= 2) && not validateRegistrationNumber (DS.take 2 state.data.vehicle_registration_number)) then VISIBLE else GONE
            ] <> FontStyle.paragraphText TypoGraphy
          , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          ] [
              linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation HORIZONTAL
              ][  textView
                  ([ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text (getString RE_ENTER_VEHICLE_REGISTRATION_NUMBER)
                  , color Color.greyTextColor
                  , margin (MarginVertical 10 10)
                  ] <> FontStyle.body3 TypoGraphy)
              ,   textView $ -- (Required Field Indication)
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text "*"
                  , color Color.warningRed
                  , alpha 0.8
                  , visibility GONE
                  , margin (MarginBottom 10)
                  ] <> FontStyle.h2 TypoGraphy
                
                ] 
              , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                , stroke ("1," <> Color.borderColorLight) 
                , cornerRadius 4.0
                ][  textView
                    [ width $ V 20
                    , height WRAP_CONTENT
                    ]
                  , editText
                    ([ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , padding (Padding 0 17 0 17)
                    , color Color.greyTextColor
                    , text state.props.input_data
                    , hint  (getString ENTER_VEHICLE_NO)
                    , weight 1.0
                    , cornerRadius 4.0
                    , pattern "[0-9A-Z]*,11"
                    , stroke ("1," <> Color.white900)
                    , id (EHC.getNewIDWithTag "VehicleRegistrationNumber")
                    , onChange push (const ReEnterVehicleRegistrationNumber state.props.input_data)
                    , inputTypeI 4097
                    ] <> FontStyle.subHeading1 TypoGraphy)
                  ]
              , textView
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , text (getString SAME_REENTERED_RC_MESSAGE)
                , visibility $ if (DS.toLower(state.data.vehicle_registration_number) /= DS.toLower(state.data.reEnterVehicleRegistrationNumber) && not (DS.null state.data.reEnterVehicleRegistrationNumber)) then VISIBLE else GONE
                , color Color.warningRed
                ]
          ]
        ]
      ]






----------------------------------------------------------------- uploadRC ------------------------------------------------------
uploadRC :: AddVehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
uploadRC state push = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , padding (Padding 20 30 20 10)
  , clickable $ not state.props.rcAvailable
  , visibility if state.data.dateOfRegistration /= Nothing then GONE else VISIBLE
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][  linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          ][ textView
            ([ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $ (getString UPLOAD_REGISTRATION_CERTIFICATE) <> if getValueFromConfig "imageUploadOptional" then (getString OPTIONAL) else ""
            , color Color.greyTextColor
            , margin (MarginBottom 10)
            ] <> FontStyle.body3 TypoGraphy)
          , textView $ -- (Required Field Indication)
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text "*"
            , color Color.warningRed
            , alpha 0.8
            , visibility GONE
            , margin (MarginBottom 10)
            ] <> FontStyle.h2 TypoGraphy
            ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER
          , orientation VERTICAL
          , onClick push (const UploadFile)
          ][  linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , gravity CENTER
              , background Color.grey700
              , PP.cornerRadii $ PTD.Corners 4.0 true true false false
              ][ imageView
                [ width ( V 328 )
                , height ( V 166 )
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_rc_demo"
                ]
              ]
            , linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation HORIZONTAL
              , stroke ("1," <> Color.borderColorLight) 
              , PP.cornerRadii $ PTD.Corners 4.0 false false true true
              , gravity CENTER
              ][  textView
                  [ width $ V 20
                  , height WRAP_CONTENT
                  ]
                , textView
                  ([ width MATCH_PARENT
                  , height (V 60)
                  , padding (Padding 0 17 20 0)
                  , color if state.props.rcAvailable then Color.greyTextColor else Color.darkGrey
                  , fontStyle $ FontStyle.semiBold LanguageStyle
                  , text if state.props.rcAvailable then state.props.rc_name else (getString UPLOAD_RC)
                  , maxLines 1
                  , ellipsize true
                  , weight 1.0
                  , cornerRadius 4.0
                  , pattern "[a-z, 0-9, A-Z]"
                  , stroke ("1," <> Color.white900)
                  --, id "111127"
                  ] <> FontStyle.subHeading1 TypoGraphy) 
                , linearLayout
                  [ height MATCH_PARENT
                  , width WRAP_CONTENT
                  , margin (Margin 0 10 20 10)
                  ][ uploadIcon state push
                    , previewIcon state push
                    ]
                ]
          ]
        , textView $ -- (Error Indication)
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text "some_text"
          , color Color.warningRed
          , margin (MarginTop 10)
          , visibility GONE
          ] <> FontStyle.paragraphText TypoGraphy
        ]
     ]

---------------------------------------------------------- uploadIcon -------------------------------------------------------
uploadIcon :: AddVehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
uploadIcon state push = 
  linearLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , gravity CENTER
    , visibility if (state.props.rcAvailable ) then GONE else VISIBLE
    ][  imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_camera_front"
          , height (V 20)
          , width (V 20)
        ]
      ]

---------------------------------------------------------- previewIcon -------------------------------------------------------
previewIcon :: AddVehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
previewIcon state push = 
  linearLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , gravity CENTER
    , visibility if state.props.rcAvailable then VISIBLE else GONE
    ][ textView
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text (getString PREVIEW)
        , color Color.blueTextColor
        , onClick (\action-> do
                      _ <- liftEffect $ JB.previewImage state.data.rc_base64
                      pure unit)(const PreviewImageAction)
        ] 
      , imageView
        [ height (V 10)
        , width (V 10)
        , margin (Margin 10 0 0 0)
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close"
        , onClick push (const RemoveUploadedFile)
        ]
    ]



referralView :: AddVehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
referralView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity BOTTOM
  , orientation HORIZONTAL
  , padding (Padding 20 10 20 40)
  , visibility if state.props.referralViewstatus then GONE else VISIBLE
  ][  textView
      ([ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text (getString HAVE_A_REFERRAL)
        , color Color.black700
        ] <> FontStyle.subHeading2 TypoGraphy)
    , textView
      ([ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text (getString ADD_HERE)
        , color Color.blue900
        , margin (MarginRight 10)
        , onClick push (const ReferralMobileNumber)
        ] <> FontStyle.subHeading2 TypoGraphy)
  ]
  

dateOfRCRegistrationView :: (Action -> Effect Unit) -> AddVehicleDetailsScreenState -> forall w . PrestoDOM (Effect Unit) w
dateOfRCRegistrationView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , visibility if state.data.dateOfRegistration == Nothing then GONE else VISIBLE
  , padding (PaddingHorizontal 20 20)
  ][ textView $
    [ text $ getString DATE_OF_REGISTRATION
    , color Color.greyTextColor
    ] <> FontStyle.body3 TypoGraphy
  , linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , margin $ MarginVertical 10 10
    , padding $ Padding 20 16 16 16
    , cornerRadius 4.0
    , stroke $ "1," <> Color.borderGreyColor
    ][ linearLayout
      [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        , onClick (\action -> do
                      _ <- push action
                      JB.datePicker "MAXIMUM_PRESENT_DATE" push $ DatePicker 
                      ) $ const DatePickerAction
        , clickable state.props.isDateClickable
      ][ textView $
        [ text if state.data.dateOfRegistration == Just "" then (getString SELECT_DATE_OF_REGISTRATION) else state.data.dateOfRegistrationView
        , color if state.data.dateOfRegistration == Just "" then Color.darkGrey else Color.greyTextColor
        , weight 1.0
        , padding $ PaddingRight 15
        ] <> FontStyle.subHeading1 TypoGraphy
      , imageView
        [ width $ V 20
        , height $ V 20
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_calendar"
        ]
      ]
    ]
  , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , onClick push (const $ TutorialModal "REGISTERATION_DATE")
      ][ textView $
        [ text (getString WHERE_IS_MY_REGISTRATION_DATE)
        , weight 1.0
        , color Color.blue900
        , gravity RIGHT
        ] <> FontStyle.tags TypoGraphy
      ]
  ]


headerLayout :: AddVehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerLayout state push =
 linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginTop 10
  , layoutGravity "center_vertical"
  , padding $ Padding 5 5 5 5
  ][  imageView
      [ width $ V 25
      , height $ V 25
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_back"
      , layoutGravity "center_vertical"
      , padding $ PaddingHorizontal 2 2
      , margin $ MarginLeft 5
      , onClick push $ const $ BackPressed state.props.openRCManual
      ]
    , textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , weight 1.0
      ]
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , cornerRadius 4.0
      , padding $ Padding 6 4 6 4
      , margin $ MarginRight 10
      , background Color.lightBlue
      , stroke $ "1," <> Color.blueBtn
      , alpha 0.8
      ][ textView
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , text $ getString HELP
          , color Color.blueBtn
          , fontStyle $ FontStyle.semiBold LanguageStyle
          , clickable true
          , onClick push $ const $ TutorialModal "RC"
          ]
      ]
    ]