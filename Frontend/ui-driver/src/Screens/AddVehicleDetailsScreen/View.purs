module Screens.AddVehicleDetailsScreen.Views where

import Prelude (Unit, bind, const, pure, unit, ($), (<<<), (<>), (==), not, (>=), (&&), (/=))
import PrestoDOM (BottomSheetState(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), editText, frameLayout, imageView, linearLayout, onBackPressed, onChange, onClick, scrollView, textView, afterRender, alignParentRight, relativeLayout, alignParentBottom, maxLines, ellipsize, layoutGravity, inputTypeI, alpha, background, clickable, color, cornerRadius, fontStyle, gravity, height, hint, id, imageUrl, margin, orientation, padding, pattern, stroke, text, textSize, visibility, weight, width, textFromHtml, imageWithFallback)
import Effect (Effect)
import Screens.AddVehicleDetailsScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.Types (AddVehicleDetailsScreenState)
import Styles.Colors as Color
import Font.Style as FontStyle
import Font.Size as FontSize
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import PrestoDOM.Properties as PP
import PrestoDOM.Types.DomAttributes as PTD
import Components.PrimaryButton as PrimaryButton
import Components.TutorialModal.View as TutorialModal
import Components.ReferralMobileNumber.View as ReferralMobileNumber
import Components.GenericMessageModal.View as GenericMessageModal
import Animation as Anim
import Language.Strings (getString)
import Language.Types (STR(..))
import Effect.Class(liftEffect)
import Data.String as DS
import Data.Maybe
import Common.Types.App
import Screens.AddVehicleDetailsScreen.ComponentConfig

screen :: AddVehicleDetailsScreenState -> Screen Action AddVehicleDetailsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "AddVehicleDetailsScreen"
  , globalEvents : [(\push -> do
    _ <- JB.storeCallBackImageUpload push CallBackImageUpload
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
        ] [TutorialModal.view (push <<< TutorialModalAction) {imageUrl : "ny_ic_vehicle_registration_card,https://assets.juspay.in/nammayatri/images/driver/ny_ic_vehicle_registration_card.png"}] else linearLayout [][]
    , if state.props.openRegistrationDateManual then 
        linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        ] [TutorialModal.view (push <<< TutorialModalAction) {imageUrl : "ny_ic_date_of_registration,https://assets.juspay.in/nammayatri/images/driver/ny_ic_date_of_registration.png"}] else linearLayout [][]
    , if state.props.limitExceedModal then 
        linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        ] [GenericMessageModal.view (push <<< GenericMessageModalAction) {text : (getString ISSUE_WITH_RC_IMAGE), openGenericMessageModal : state.props.limitExceedModal, buttonText : (getString NEXT) }] else linearLayout [][]

    , if state.props.openReferralMobileNumber then
        linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT] 
        [ReferralMobileNumber.view (push <<< ReferralMobileNumberAction) {isApplyButtonActive : state.props.btnActive, referralNumber : if state.props.isEdit then state.data.referral_mobile_number else ""}] else linearLayout [][]
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
  , visibility if(state.props.referralViewstatus == true) then GONE else VISIBLE
  , cornerRadius 8.0
  ][  textView 
      [ text (getString HAVE_A_REFERRAL)
      , color Color.black700
      , textSize FontSize.a_14
      , fontStyle $ FontStyle.medium LanguageStyle
      , margin (MarginRight 8)
      ]
    , textView
      [ text (getString ADD_HERE)
      , color Color.blue900
      , textSize FontSize.a_14
      , onClick push (const ReferralMobileNumber)
      , fontStyle $ FontStyle.medium LanguageStyle
      ]]

referralAppliedView :: AddVehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w 
referralAppliedView state push = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , gravity BOTTOM
  , visibility if(state.props.referralViewstatus == true) then VISIBLE else GONE
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
              [ imageWithFallback  "ny_ic_check_green,https://assets.juspay.in/nammayatri/images/driver/ny_ic_check_green.png"
              , width (V 11)
              , height (V 8)
              ]
            , textView
              [ margin (MarginLeft 11)
              , text (getString REFERRAL_APPLIED)
              , textSize FontSize.a_14
              , color Color.darkMint
              , fontStyle $ FontStyle.medium LanguageStyle
              ]
            ]
        , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation HORIZONTAL
          , alignParentRight "true,-1"
          ][  textView 
              [ margin (MarginRight 8)
              , text state.data.referral_mobile_number
              , textSize FontSize.a_14
              , color Color.black800
              , fontStyle $ FontStyle.medium LanguageStyle
              ]
            , textView
              [ text (getString SMALLEDIT)
              , color Color.blue900
              , textSize FontSize.a_14
              , fontStyle $ FontStyle.medium LanguageStyle
              , onClick push (const ReferralMobileNumber)
              ]
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
          ,   textView -- (Required Field Indication)
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text "*"
              , color Color.warningRed
              , textSize FontSize.a_18
              , fontStyle $ FontStyle.bold LanguageStyle
              , alpha 0.8
              , visibility GONE
              , margin (MarginBottom 10)
              ]
            ] 
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , stroke ("1," <> if ((DS.length state.data.vehicle_registration_number >= 2) && (DS.take 2 state.data.vehicle_registration_number /= "KA")) then Color.warningRed else Color.borderColorLight) 
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
              , textSize FontSize.a_16
              , weight 1.0
              , cornerRadius 4.0
              , pattern "[0-9A-Z]*,11"
              , stroke ("1," <> Color.white900)
              , id (EHC.getNewIDWithTag "VehicleRegistrationNumber")
              , onChange push (const VehicleRegistrationNumber state.props.input_data)
              , inputTypeI 4097
              ] <> FontStyle.subHeading1 TypoGraphy)
            ]
          , textView  -- (Error Indication)
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text (getString CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER)
            , textSize FontSize.a_14
            , color Color.warningRed
            , fontStyle $ FontStyle.regular LanguageStyle
            , margin (MarginTop 10)
            , visibility if ((DS.length state.data.vehicle_registration_number >= 2) && (DS.take 2 state.data.vehicle_registration_number /= "KA")) then VISIBLE else GONE
            ]
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
              ,   textView -- (Required Field Indication)
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text "*"
                  , color Color.warningRed
                  , textSize FontSize.a_18
                  , fontStyle $ FontStyle.bold LanguageStyle
                  , alpha 0.8
                  , visibility GONE
                  , margin (MarginBottom 10)
                  ]
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
                    , textSize FontSize.a_16
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
                , visibility $ if (DS.toLower(state.data.vehicle_registration_number) /= DS.toLower(state.data.reEnterVehicleRegistrationNumber)) then VISIBLE else GONE
                , color Color.darkGrey
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
  , onClick push (const UploadFile)
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
            , text (getString UPLOAD_REGISTRATION_CERTIFICATE)
            , color Color.greyTextColor
            , margin (MarginBottom 10)
            ] <> FontStyle.body3 TypoGraphy)
          , textView -- (Required Field Indication)
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text "*"
            , color Color.warningRed
            , textSize FontSize.a_18
            , fontStyle $ FontStyle.bold LanguageStyle
            , alpha 0.8
            , visibility GONE
            , margin (MarginBottom 10)
            ]
            ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER
          , background Color.grey700
          , PP.cornerRadii $ PTD.Corners 4.0 true true false false
          ][ imageView
            [ width ( V 328 )
            , height ( V 166 )
            , imageWithFallback  "ny_ic_rc_demo,https://assets.juspay.in/nammayatri/images/driver/ny_ic_rc_demo.png"
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
              , textSize FontSize.a_16
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
        , textView  -- (Error Indication)
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text "some_text"
          , textSize FontSize.a_14
          , color Color.warningRed
          , fontStyle $ FontStyle.regular LanguageStyle
          , margin (MarginTop 10)
          , visibility GONE
          ]
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
        [ imageWithFallback "ny_ic_camera_front,https://assets.juspay.in/nammayatri/images/driver/ny_ic_camera_front.png"
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
        , imageWithFallback "ny_ic_close,https://assets.juspay.in/nammayatri/images/common/ny_ic_close.png"
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
  , visibility if(state.props.referralViewstatus == true) then GONE else VISIBLE
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
                        _ <- JB.datePicker "MAXIMUM_PRESENT_DATE" push $ DatePicker 
                        pure unit
                      ) $ const DatePickerAction
      ][ textView $
        [ text if state.data.dateOfRegistration == Just "" then (getString SELECT_DATE_OF_REGISTRATION) else state.data.dateOfRegistrationView
        , color if state.data.dateOfRegistration == Just "" then Color.darkGrey else Color.greyTextColor
        , weight 1.0
        , padding $ PaddingRight 15
        ] <> FontStyle.subHeading1 TypoGraphy
      , imageView
        [ width $ V 20
        , height $ V 20
        , imageWithFallback "ny_ic_calendar,https://assets.juspay.in/nammayatri/images/driver/ny_ic_calendar.png"
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
      , imageWithFallback "ny_ic_back,https://assets.juspay.in/nammayatri/images/driver/ny_ic_back.png"
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