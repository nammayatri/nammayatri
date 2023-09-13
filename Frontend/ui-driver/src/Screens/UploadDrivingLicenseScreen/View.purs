{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.UploadDrivingLicenseScreen.View where

import Prelude (Unit, bind, const, pure, unit, ($), (<<<), (<>), (/=), (==), (&&), (>), (<),discard,void,not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen,Visibility(..), afterRender, background, clickable, color, cornerRadius, editText, fontStyle, frameLayout, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onChange, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, weight, width, layoutGravity, alpha, singleLine, visibility, scrollBarY, textFromHtml, imageWithFallback)
import PrestoDOM.Types.DomAttributes as PTD
import PrestoDOM.Properties as PP
import Animation as Anim
import Effect (Effect)
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.UploadDrivingLicenseScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Style as FontStyle
import Components.RegistrationModal.View as RegistrationModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.TutorialModal.View as TutorialModal
import JBridge as JB
import Components.GenericMessageModal as GenericMessageModal
import Effect.Class (liftEffect)
import Data.Maybe
import Log (printLog)
import Data.String as DS
import Common.Types.App
import Control.Transformers.Back.Trans (runBackT)
import Presto.Core.Types.Language.Flow (doAff)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Except (runExceptT)
import Engineering.Helpers.Commons as EHC
import Types.App (defaultGlobalState)
import Effect.Aff (launchAff)
import PrestoDOM.Animation as PrestoAnim
import Font.Size as FontSize
import Components.StepsHeaderModel as StepsHeaderModel
import Screens.UploadDrivingLicenseScreen.ComponentConfig
import Components.PopUpModal as PopUpModal
import Components.ValidateDocumentModal as ValidateDocumentModal

screen :: ST.UploadDrivingLicenseState -> Screen Action ST.UploadDrivingLicenseState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "UploadDrivingLicenseScreen"
  , globalEvents : [(\push -> do
    _ <- JB.storeCallBackImageUpload push CallBackImageUpload
    void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $
              if (initialState.props.validateProfilePicturePopUp == true)  then  lift $ lift $ doAff do liftEffect $ push $ AfterRender  else pure unit 
    pure $ pure unit)]
  , eval : \action state -> do
      let _ = printLog  "UploadDrivingLicenseScreen state -----" state
      eval action state
  }

view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.UploadDrivingLicenseState
  -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  frameLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ]([
linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , onBackPressed push (const BackPressed state.props.openLicenseManual)
    , afterRender  (\action -> do
                      _<- push action
                      pure unit
                      ) $ const (AfterRender)
    ][ --headerLayout state push
       PrestoAnim.animationSet
          [ Anim.fadeIn true
          ] $ StepsHeaderModel.view (push <<< StepsHeaderModelAC) (stepsHeaderModelConfig state (if state.props.openHowToUploadManual then 4 else 3))
      , linearLayout
        [ width MATCH_PARENT
        , weight 1.0
        , orientation VERTICAL
        ][ scrollView
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , scrollBarY false
            ][ linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , padding (PaddingHorizontal 20 20)
                ][ enterLicenceNumber state push
                , reEnterLicenceNumber state push
                , dateOfBirth push state
                --, dateOfIssue push state
               -- , frontUploadSection state push
                -- , backUploadSection state push
                , howToUpload push state
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
           , visibility $ GONE--if state.props.errorVisibility then VISIBLE else GONE
           , color Color.red
           , padding( PaddingHorizontal 20 20)
           , margin (MarginBottom 10)
           ]
           , PrimaryButton.view (push <<< PrimaryButtonAction) (primaryButtonConfig state)]

    ]   
    , if state.props.openRegistrationModal then 
    linearLayout[
      width MATCH_PARENT
    , height MATCH_PARENT
      ] [registrationModalView state push] else linearLayout [][]
    , if state.props.openLicenseManual then 
      linearLayout[
      width MATCH_PARENT
    , height MATCH_PARENT
      ] [TutorialModal.view (push <<< TutorialModalAction) {imageUrl : "ny_ic_driver_license_card,https://assets.juspay.in/nammayatri/images/driver/ny_ic_driver_license_card.png"}] else linearLayout [][]
    , if state.props.openDateOfIssueManual then 
      linearLayout[
      width MATCH_PARENT
    , height MATCH_PARENT
      ] [TutorialModal.view (push <<< TutorialModalAction) {imageUrl : "ny_ic_date_of_issue,https://assets.juspay.in/nammayatri/images/driver/ny_ic_date_of_issue.png"}] else linearLayout [][]
    , if state.props.openGenericMessageModal then 
      linearLayout[
      width MATCH_PARENT
    , height MATCH_PARENT
      ] [GenericMessageModal.view (push <<< GenericMessageModalAction) {text : (getString ISSUE_WITH_DL_IMAGE), openGenericMessageModal : state.props.openGenericMessageModal, buttonText : (getString NEXT) }] else linearLayout [][]
  ] <> if state.props.logoutPopupModal then [logoutPopupModal push state] else []
    <> if state.props.imageCaptureLayoutView then [imageCaptureLayout push state] else []
    <> if state.props.validateProfilePicturePopUp then [validateProfilePictureModal push state] else []
    <> if state.props.fileCameraPopupModal then [fileCameraLayout push state] else [] )
  
registrationModalView :: ST.UploadDrivingLicenseState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
registrationModalView state push = 
  RegistrationModal.view (push <<< RegistrationModalAction) ({
    openRegistrationModal: state.props.openRegistrationModal
  })

enterLicenceNumber :: ST.UploadDrivingLicenseState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
enterLicenceNumber state push = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin (MarginTop 30)
  , visibility if state.props.openHowToUploadManual then GONE else VISIBLE
  ][  textView
      ([ width WRAP_CONTENT
      , textFromHtml (getString PROVIDE_DATE_OF_ISSUE_TEXT)
      , color Color.black800
      , margin (MarginVertical 20 10)
      , visibility if state.data.dateOfIssue == Nothing then GONE else VISIBLE
      ] <> FontStyle.subHeading2 TypoGraphy)
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , visibility if state.data.dateOfIssue /= Nothing then GONE else VISIBLE
        ][
          PrimaryEditText.view (push <<< PrimaryEditTextActionController) (primaryEditTextConfig state)
        ]
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity RIGHT
        , orientation VERTICAL
        , margin (MarginBottom 10)
        ][ textView
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text (getString WHERE_IS_MY_LICENSE_NUMBER)
            , color Color.blue900
            , fontStyle $ FontStyle.semiBold LanguageStyle
            , clickable true 
            , onClick push (const $ TutorialModal "LICENSE")
            , visibility GONE
            ]
        ]
 ]
reEnterLicenceNumber :: ST.UploadDrivingLicenseState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
reEnterLicenceNumber state push = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , visibility if state.props.openHowToUploadManual then GONE else VISIBLE
  --, visibility if state.data.dateOfIssue /= Nothing then GONE else VISIBLE
  , margin (MarginBottom 10)
  ][ PrimaryEditText.view (push <<< PrimaryEditTextActionControllerReEnter) (primaryEditTextConfigReEnterDl state)
   , textView
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , text (getString SAME_REENTERED_DL_MESSAGE)
      , visibility $ if (DS.toLower(state.data.driver_license_number) /= DS.toLower(state.data.reEnterDriverLicenseNumber)) then VISIBLE else GONE
      , color Color.red
      
      ]
 ]

frontUploadSection :: ST.UploadDrivingLicenseState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
frontUploadSection state push =
  linearLayout
  [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin (MarginTop 20)
    , onClick push (const( UploadFileAction "front"))
    , clickable $ state.data.imageFront == ""
    , visibility if state.props.openHowToUploadManual then GONE else VISIBLE
   -- , visibility if state.data.dateOfIssue /= Nothing then GONE else VISIBLE
  ][
    textView
    ([ text (getString FRONT_SIDE)
    , color Color.greyTextColor
    ] <> FontStyle.body3 TypoGraphy)
  , linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , margin (MarginTop 10)
    , background Color.grey700
    , PP.cornerRadii $ PTD.Corners 4.0 true true false false
    ][ imageView
      [ width MATCH_PARENT
      , height ( V 166 )
      , imageWithFallback "ny_ic_dl_demo,https://assets.juspay.in/nammayatri/images/driver/ny_ic_dl_demo.png"
      ]
    ]
  , linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , margin (MarginBottom 10)
    , padding (Padding 16 16 16 16)
    , PP.cornerRadii $ PTD.Corners 4.0 false false true true
    , stroke ("1," <> Color.borderGreyColor)
    ][ 
      textView
      ([ text if (state.data.imageFront == "") then (getString UPLOAD_FRONT_SIDE) else state.data.imageNameFront
      , color if (state.data.imageFront == "") then Color.darkGrey else Color.greyTextColor
      , weight 1.0
      , singleLine true
      , padding (PaddingRight 15)
      ] <> FontStyle.subHeading1 TypoGraphy)
    , if (state.data.imageFront /= "") then previewIcon state push "front" else
      imageView
      [ width ( V 20 )
      , height ( V 20 )
      , imageWithFallback "ny_ic_camera_front,https://assets.juspay.in/nammayatri/images/driver/ny_ic_camera_front.png"
      ]
    ]
  ]

backUploadSection :: ST.UploadDrivingLicenseState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
backUploadSection state push = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin (MarginTop 20)
  , orientation VERTICAL
  , onClick push (const (UploadFileAction "back"))
  , clickable $ state.data.imageBack == ""
  , visibility if state.props.openHowToUploadManual then GONE else VISIBLE
  ][
    textView
    ([ text (getString BACK_SIDE)
    , color Color.greyTextColor
    ] <> FontStyle.body3 TypoGraphy)
  , linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , margin (MarginVertical 10 10)
    , padding (Padding 16 16 16 16)
    , cornerRadius 4.0
    , stroke ("1," <> Color.borderGreyColor)
    ][
      textView
      ([ text if (state.data.imageBack == "") then (getString UPLOAD_BACK_SIDE) else state.data.imageNameBack
      , color if (state.data.imageBack == "") then Color.darkGrey else Color.greyTextColor
      , weight 1.0
      , padding (PaddingRight 15)
      , singleLine true
      ] <> FontStyle.subHeading1 TypoGraphy)
    , if (state.data.imageBack /= "") then previewIcon state push "back" else
      imageView
      [ width ( V 20 )
      , height ( V 20 )
      , imageWithFallback "ny_ic_camera_front,https://assets.juspay.in/nammayatri/images/driver/ny_ic_camera_front.png"
      ]
    ]
  ]

previewIcon :: ST.UploadDrivingLicenseState -> (Action -> Effect Unit) -> String -> forall w . PrestoDOM (Effect Unit) w
previewIcon state push previewType = 
  linearLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , gravity CENTER
    ][ textView
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text (getString PREVIEW)
        , color Color.blueTextColor
        , onClick (\action-> do
                      _ <- liftEffect $ JB.previewImage $ if(previewType == "front") then state.data.imageFrontUrl else state.data.imageBack
                      pure unit)(const NoAction)
        ] 
      , imageView
          [ height (V 10)
          , width (V 10)
          , margin (MarginLeft 10)
          , imageWithFallback "ny_ic_close,https://assets.juspay.in/nammayatri/images/common/ny_ic_close.png"
          , onClick push (const( RemoveUploadedFile previewType))
          ]
    ]

headerLayout :: ST.UploadDrivingLicenseState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerLayout state push =
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , orientation VERTICAL
 , margin (MarginTop 10)
 , layoutGravity "center_vertical"
 ][ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding (Padding 5 5 5 5)
    ][ imageView
        [ width $ V 25
        , height MATCH_PARENT
        , imageWithFallback "ny_ic_back,https://assets.juspay.in/nammayatri/images/driver/ny_ic_back.png"
        , layoutGravity "center_vertical"
        , padding (PaddingHorizontal 2 2)
        , margin (MarginLeft 5)
        , onClick push (const $ BackPressed state.props.openLicenseManual)
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
        , padding (Padding 6 4 6 4)
        , margin (MarginRight 10)
        , background Color.lightBlue
        , stroke ("1," <> Color.blueBtn)
        , alpha 0.8
        ][ textView
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , text (getString HELP)
            , color Color.blueBtn
            , gravity CENTER
            , fontStyle $ FontStyle.semiBold LanguageStyle
            , clickable true
            , onClick push (const $ TutorialModal "LICENSE")
            ]
        ]
    ]
 ]

dateOfBirth :: (Action -> Effect Unit) -> ST.UploadDrivingLicenseState -> forall w . PrestoDOM (Effect Unit) w
dateOfBirth push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , visibility if state.props.openHowToUploadManual then GONE else VISIBLE
  --, visibility if state.data.dateOfIssue /= Nothing then GONE else VISIBLE
  ][ textView
    ([ text (getString DATE_OF_BIRTH)
    , color Color.greyTextColor
    ] <> FontStyle.body3 TypoGraphy)
  , linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , margin (MarginVertical 10 10)
    , padding (Padding 20 16 16 16)
    , cornerRadius 4.0
    , stroke ("1," <> Color.borderGreyColor)
    ][ linearLayout
      [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        , onClick (\action -> do
                        _ <- JB.datePicker "MINIMUM_EIGHTEEN_YEARS" push $ DatePicker "DATE_OF_BIRTH"
                        pure unit
                      ) (const SelectDateOfBirthAction)
      ][ textView
        ([ text if state.data.dob == "" then (getString SELECT_DATE_OF_BIRTH) else state.data.dobView
        , color if (state.data.dob == "") then Color.darkGrey else Color.greyTextColor
        , weight 1.0
        , padding (PaddingRight 15)
        ] <> FontStyle.subHeading1 TypoGraphy)
      , imageView
        [ width ( V 20 )
        , height ( V 20 )
        , imageWithFallback "ny_ic_calendar,https://assets.juspay.in/nammayatri/images/driver/ny_ic_calendar.png"
        ]
      ]
    ]
  ]

dateOfIssue :: (Action -> Effect Unit) -> ST.UploadDrivingLicenseState -> forall w . PrestoDOM (Effect Unit) w
dateOfIssue push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , visibility if state.data.dateOfIssue == Nothing then GONE else VISIBLE
  ][ textView $ 
    [ text $ getString DATE_OF_ISSUE
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
                        _ <- JB.datePicker "MAXIMUM_PRESENT_DATE" push $ DatePicker "DATE_OF_ISSUE"
                        pure unit
                      ) $ const SelectDateOfIssueAction
      ][ textView $
        [ text if state.data.dateOfIssue == Just "" then (getString SELECT_DATE_OF_ISSUE) else state.data.dateOfIssueView
        , color if state.data.dateOfIssue == Just "" then Color.darkGrey else Color.greyTextColor
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
      , onClick push (const $ TutorialModal "DATE_OF_ISSUE")
      ][ textView $
        [ text (getString WHERE_IS_MY_ISSUE_DATE)
        , weight 1.0
        , color Color.blue900
        , gravity RIGHT
        ] <> FontStyle.tags TypoGraphy
      ]
  ]

howToUpload :: (Action -> Effect Unit) -> ST.UploadDrivingLicenseState -> forall w . PrestoDOM (Effect Unit) w
howToUpload push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin (MarginTop 20) 
  , visibility if state.props.openHowToUploadManual then VISIBLE else GONE 
  ][ textView $ 
    [ text "How to upload"
    , color Color.greyTextColor
    ] <> FontStyle.h3 TypoGraphy
  , linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginVertical 0 10
    , padding $ Padding 0 16 0 16
    ][ 
      textView $ 
    [ text "Take a clear picture of the photo side of your Driving Licence on a flat surface."
    , color Color.greyTextColor
    , margin $ MarginBottom 18
    , textSize FontSize.a_14
    ] <> FontStyle.body3 TypoGraphy

    , textView $ 
    [ text "Ensure that the lighting is adequate and all the details are clearly visible"
    , color Color.greyTextColor
    , margin $ MarginBottom 18
    , textSize FontSize.a_14
    ] <> FontStyle.body3 TypoGraphy

    , textView $ 
    [ text "Fit the Driving Licence in the marked area correctly as shown below"
    , color Color.greyTextColor
    , margin $ MarginBottom 40
    , textSize FontSize.a_14
    ] <> FontStyle.body3 TypoGraphy

    , imageView
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , imageWithFallback "ny_ic_upload_document,https://assets.juspay.in/nammayatri/images/driver/ny_ic_back.png"
        , layoutGravity "center_vertical"
        ]
     ]
  ]

logoutPopupModal :: forall w . (Action -> Effect Unit) -> ST.UploadDrivingLicenseState -> PrestoDOM (Effect Unit) w
logoutPopupModal push state =
       linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , background Color.blackLessTrans
        ][ PopUpModal.view (push <<<PopUpModalLogoutAction) (logoutPopUp state) ]


validateProfilePictureModal :: forall w . (Action -> Effect Unit) -> ST.UploadDrivingLicenseState -> PrestoDOM (Effect Unit) w
validateProfilePictureModal push state =
  ValidateDocumentModal.view (push <<< ValidateDocumentModalAction) (validateProfilePictureModalState state)

validateProfilePictureModalState :: ST.UploadDrivingLicenseState -> ValidateDocumentModal.ValidateDocumentModalState
validateProfilePictureModalState state = let
      config' = ValidateDocumentModal.config
      inAppModalConfig' = config'{
        background = Color.black,
        profilePictureCapture = false,
        verificationStatus = not state.props.errorVisibility,
        failureReason = state.data.errorMessage,
        headerConfig {
         imageConfig {
         color = Color.white900
        },
          headTextConfig {
            text = "Confirm Selfie",
            color = Color.white900
          }
        }
      }
      in inAppModalConfig'

imageCaptureLayout :: forall w . (Action -> Effect Unit) -> ST.UploadDrivingLicenseState -> PrestoDOM (Effect Unit) w
imageCaptureLayout push state  =ValidateDocumentModal.view (push <<< ValidateDocumentModalAction) (ValidateDocumentModal.config{background = Color.black,profilePictureCapture =true ,headerConfig {headTextConfig {text = ("TAKE_SELFIE")}}})

fileCameraLayout :: forall w . (Action -> Effect Unit) -> ST.UploadDrivingLicenseState -> PrestoDOM (Effect Unit) w
fileCameraLayout push state =
  PopUpModal.view (push <<< PopUpModalActions)  (fileCameraLayoutConfig state)
