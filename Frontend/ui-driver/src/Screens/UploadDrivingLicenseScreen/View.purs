{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.UploadDrivingLicenseScreen.View where

import Common.Types.App
import Data.Maybe
import Debug
import Screens.UploadDrivingLicenseScreen.ComponentConfig
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericMessageModal as GenericMessageModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.RegistrationModal.View as RegistrationModal
import Components.TutorialModal.View as TutorialModal
import Components.ValidateDocumentModal as ValidateDocumentModal
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.String as DS
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import PaymentPage (consumeBP)
import Effect.Uncurried (runEffectFn1)
import Engineering.Helpers.Commons (flowRunner)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..) )
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import Prelude (Unit, bind, const, pure, unit, ($), (<<<), (<>), (/=), (==), (&&), (>), (<), discard, void, not, (||))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alpha, background, clickable, color, cornerRadius, editText, fontStyle, frameLayout, gravity, height, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onChange, onClick, orientation, padding, scrollBarY, scrollView, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties as PP
import PrestoDOM.Types.DomAttributes as PTD
import Screens.AddVehicleDetailsScreen.Views (redirectScreen, rightWrongView)
import Screens.Types as ST
import Screens.UploadDrivingLicenseScreen.Controller (Action(..), eval, ScreenOutput)
import Styles.Colors as Color
import Types.App (defaultGlobalState)
import Screens.RegistrationScreen.ComponentConfig (logoutPopUp) as LP
import Data.String.Common as DSC
import ConfigProvider
import Components.OptionsMenu as OptionsMenu
import Data.Array as DA
import Screens.RegistrationScreen.ComponentConfig (changeVehicleConfig)
import Components.BottomDrawerList as BottomDrawerList
import Engineering.Helpers.Events as EHE
import Helpers.Utils as HU


screen :: ST.UploadDrivingLicenseState -> Screen Action ST.UploadDrivingLicenseState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "UploadDrivingLicenseScreen"
  , globalEvents : [(\push -> do
    _ <- JB.storeCallBackImageUpload push CallBackImageUpload
    _ <- runEffectFn1 consumeBP unit
    let _ = EHE.addEvent (EHE.defaultEventObject $ HU.getRegisterationStepScreenLoadedEventName ST.DRIVING_LICENSE_OPTION)
    if initialState.props.successfulValidation then do
      _ <- launchAff $ flowRunner defaultGlobalState $ redirectScreen push RedirectScreen
      pure unit
    else pure unit
    void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $
              if initialState.props.validateProfilePicturePopUp  then  lift $ lift $ doAff do liftEffect $ push $ AfterRender  else pure unit 
    pure $ pure unit)]
  , eval : \action state -> do
      let _ = spy  "UploadDrivingLicenseScreen state -----" state
          _ = spy "UploadDrivingLicenseScreen action -----" action
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
  ] $ [
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
    ][ PrestoAnim.animationSet  [ Anim.fadeIn true ] 
      $ headerView state push 
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
                ][ textView $
                      [ width MATCH_PARENT
                      , height WRAP_CONTENT
                      , text $ getString DL_VERIFICATION_FAILED
                      , color Color.black800
                      , background Color.redOpacity10
                      , padding $ Padding 16 12 16 12
                      , margin $ Margin 16 16 16 16
                      , cornerRadius 8.0
                      , visibility $ fromMaybeVisibility state.data.dateOfIssue
                      ] <> FontStyle.body3 TypoGraphy
                , enterLicenceNumber state push
                , dateOfBirth push state
                , dateOfIssue push state
                , howToUpload push state
                ]
              ]
          ]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , orientation VERTICAL
        ][  textView
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text state.data.errorMessage
            , visibility $ fromBooleanVisibility (not $ DSC.null state.data.errorMessage)
            , color Color.red
            , padding( PaddingHorizontal 20 20)
            , margin (MarginBottom 10)
            ]
          , PrimaryButton.view (push <<< PrimaryButtonAction) (primaryButtonConfig state)
          , if state.props.openHowToUploadManual && not state.data.cityConfig.uploadRCandDL then skipButton push state else dummyLinearLayout]

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
      ] [TutorialModal.view (push <<< TutorialModalAction) {imageUrl : fetchImage FF_ASSET "ny_ic_driver_license_card" }] else linearLayout [][]
    , if state.props.openDateOfIssueManual then 
      linearLayout[
      width MATCH_PARENT
    , height MATCH_PARENT
      ] [TutorialModal.view (push <<< TutorialModalAction) {imageUrl : fetchImage FF_ASSET "ny_ic_date_of_issue"}] else linearLayout [][]
    , if state.props.contactSupportModal /= ST.HIDE then BottomDrawerList.view (push <<< BottomDrawerListAC) (bottomDrawerListConfig state) else linearLayout[][]
    , if state.props.openGenericMessageModal then 
      linearLayout[
      width MATCH_PARENT
    , height MATCH_PARENT
      ] [GenericMessageModal.view (push <<< GenericMessageModalAction) {text : (getString ISSUE_WITH_DL_IMAGE), openGenericMessageModal : state.props.openGenericMessageModal, buttonText : (getString NEXT) }] else linearLayout [][]
  ] <> if DA.any (_ == true) [state.props.logoutPopupModal, state.props.confirmChangeVehicle] then [ popupModal push state ] else []
    <> if state.props.imageCaptureLayoutView then [imageCaptureLayout push state] else []
    <> if state.props.validateProfilePicturePopUp then [validateProfilePictureModal push state] else []
    <> if state.props.fileCameraPopupModal then [fileCameraLayout push state] else []
    <> if state.props.menuOptions then [menuOptionModal push state] else []

menuOptionModal :: forall w. (Action -> Effect Unit) -> ST.UploadDrivingLicenseState -> PrestoDOM (Effect Unit) w
menuOptionModal push state = 
  linearLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , padding $ PaddingTop 55
    ][ OptionsMenu.view (push <<< OptionsMenuAction) (optionsMenuConfig state) ]

headerView :: forall w. ST.UploadDrivingLicenseState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerView state push = AppOnboardingNavBar.view (push <<< AppOnboardingNavBarAC) (appOnboardingNavBarConfig state)

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
  , visibility $ fromBooleanVisibility $ not state.props.openHowToUploadManual
  ][   linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
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
  , visibility $ fromBooleanVisibility (not state.props.openHowToUploadManual)
  , margin (MarginBottom 10)
  ][ PrimaryEditText.view (push <<< PrimaryEditTextActionControllerReEnter) (primaryEditTextConfigReEnterDl state)
   , textView
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , text (getString SAME_REENTERED_DL_MESSAGE)
      , visibility $ fromBooleanVisibility (DS.toLower(state.data.driver_license_number) /= DS.toLower(state.data.reEnterDriverLicenseNumber) && not (DSC.null state.data.reEnterDriverLicenseNumber))
      , color Color.red
      ]
 ]

frontUploadSection :: ST.UploadDrivingLicenseState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
frontUploadSection state push =
  let feature = (getAppConfig appConfig).feature 
  in
  linearLayout
  [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin (MarginTop 20)
    , onClick push (const( UploadFileAction "front"))
    , clickable $ DSC.null state.data.imageFront
    , visibility $ fromBooleanVisibility (not state.props.openHowToUploadManual)
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
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_dl_demo"
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
      ([ text if (DSC.null state.data.imageFront) then (getString UPLOAD_FRONT_SIDE) else state.data.imageNameFront
      , color if (DSC.null state.data.imageFront) then Color.darkGrey else Color.greyTextColor
      , weight 1.0
      , singleLine true
      , padding (PaddingRight 15)
      ] <> FontStyle.subHeading1 TypoGraphy)
    , if not (DSC.null state.data.imageFront) then previewIcon state push "front" else
      imageView
      [ width ( V 20 )
      , height ( V 20 )
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_camera_front"
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
  , clickable $ DSC.null state.data.imageBack
  , visibility $ fromBooleanVisibility (not state.props.openHowToUploadManual)
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
      ([ text if (DSC.null state.data.imageBack) then (getString UPLOAD_BACK_SIDE) else state.data.imageNameBack
      , color if (DSC.null state.data.imageBack) then Color.darkGrey else Color.greyTextColor
      , weight 1.0
      , padding (PaddingRight 15)
      , singleLine true
      ] <> FontStyle.subHeading1 TypoGraphy)
    , if not (DSC.null state.data.imageBack) then previewIcon state push "back" else
      imageView
      [ width ( V 20 )
      , height ( V 20 )
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_camera_front"
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
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close"
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
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_back"
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
        , stroke ("1," <> Color.brightBlue)
        , alpha 0.8
        ][ textView
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , text (getString HELP)
            , color Color.brightBlue
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
  , visibility $ fromBooleanVisibility (not state.props.openHowToUploadManual) 
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
    , stroke ("1," <> state.data.config.themeColors.editTextNormalStroke)
    , background state.data.config.themeColors.radioInactiveBackground
    ][ linearLayout
      [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        , onClick (\action -> do
                        _ <- push action
                        JB.datePicker "MINIMUM_EIGHTEEN_YEARS" push $ DatePicker "DATE_OF_BIRTH"
                      ) (const SelectDateOfBirthAction)
        , clickable state.props.isDateClickable 
      ][ textView
        ([ text if DSC.null state.data.dob then (getString SELECT_DATE_OF_BIRTH) else state.data.dobView
        , color if DSC.null state.data.dob then Color.darkGrey else Color.greyTextColor
        , weight 1.0
        , padding (PaddingRight 15)
        ] <> FontStyle.subHeading1 TypoGraphy)
      , imageView
        [ width ( V 20 )
        , height ( V 20 )
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_calendar"
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
  , visibility $ fromMaybeVisibility state.data.dateOfIssue
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
                        _ <- push action 
                        JB.datePicker "MAXIMUM_PRESENT_DATE" push $ DatePicker "DATE_OF_ISSUE"
                      ) $ const SelectDateOfIssueAction
        , clickable state.props.isDateClickable
      ][ textView $
        [ text if state.data.dateOfIssue == Just "" then (getString SELECT_DATE_OF_ISSUE) else state.data.dateOfIssueView
        , color if state.data.dateOfIssue == Just "" then Color.darkGrey else Color.greyTextColor
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
  , visibility $ fromBooleanVisibility state.props.openHowToUploadManual
  ][ textView $ 
    [ text $ getString HOW_TO_UPLOAD
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
      [ text $ getString TAKE_CLEAR_PICTURE_DL
      , color Color.black800
      , margin $ MarginBottom 18
      ] <> FontStyle.body3 TypoGraphy
    , textView $ 
      [ text $ getString ENSURE_ADEQUATE_LIGHT
      , color Color.black800
      , margin $ MarginBottom 18
      ] <> FontStyle.body3 TypoGraphy
    , textView $ 
      [ text $ getString FIT_DL_CORRECTLY
      , color Color.black800
      , margin $ MarginBottom 40
      ] <> FontStyle.body3 TypoGraphy
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , cornerRadius 4.0
      , margin $ MarginTop 20
      , stroke $ "1," <> Color.borderGreyColor
      , padding $ Padding 16 16 16 0
      ][ rightWrongView true
       , rightWrongView false
      ]  
    ]
  ]

popupModal :: forall w . (Action -> Effect Unit) -> ST.UploadDrivingLicenseState -> PrestoDOM (Effect Unit) w
popupModal push state =
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.blackLessTrans
    ][ PopUpModal.view (push <<< action) popupConfig ] 
    where 
      action = if state.props.logoutPopupModal then PopUpModalLogoutAction 
                else ChangeVehicleAC
      popupConfig = if state.props.logoutPopupModal then (LP.logoutPopUp Language)
                    else changeVehicleConfig FunctionCall

validateProfilePictureModal :: forall w . (Action -> Effect Unit) -> ST.UploadDrivingLicenseState -> PrestoDOM (Effect Unit) w
validateProfilePictureModal push state =
  ValidateDocumentModal.view (push <<< ValidateDocumentModalAction) (validateProfilePictureModalState state)

validateProfilePictureModalState :: ST.UploadDrivingLicenseState -> ValidateDocumentModal.ValidateDocumentModalState
validateProfilePictureModalState state = let
  status = if state.props.validating then ST.InProgress 
                          else if not (DSC.null state.data.errorMessage) then ST.Failure
                          else if (not (DSC.null state.data.imageIDFront)) || (not ( DSC.null state.data.imageIDBack)) then ST.Success
                          else ST.None
  config' = ValidateDocumentModal.config
  inAppModalConfig' = config'{
    background = Color.black,
    profilePictureCapture = false,
    verificationStatus = status,
    verificationType = "DL",
    failureReason = state.data.errorMessage,
    headerConfig {
      imageConfig {
      color = Color.white900
    },
      headTextConfig {
        text = getString TAKE_PHOTO,
        color = Color.white900
      }
    }
  }
  in inAppModalConfig'

skipButton :: forall w . (Action -> Effect Unit) -> ST.UploadDrivingLicenseState -> PrestoDOM (Effect Unit) w
skipButton push state =
  textView $
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , color Color.black
  , gravity CENTER
  , onClick push $ const SkipButton
  , text $ getString SKIP
  , margin $ MarginBottom 15
  ] <> FontStyle.body1 TypoGraphy

imageCaptureLayout :: forall w . (Action -> Effect Unit) -> ST.UploadDrivingLicenseState -> PrestoDOM (Effect Unit) w
imageCaptureLayout push state  =  
  ValidateDocumentModal.view  (push <<< ValidateDocumentModalAction) 
      (ValidateDocumentModal.config 
        { background = Color.black
        , profilePictureCapture = true 
        , headerConfig 
          { headTextConfig 
            { text = getString TAKE_PHOTO
            }
          }
        })

fileCameraLayout :: forall w . (Action -> Effect Unit) -> ST.UploadDrivingLicenseState -> PrestoDOM (Effect Unit) w
fileCameraLayout push state =
  PopUpModal.view (push <<< PopUpModalActions)  (fileCameraLayoutConfig state)

fromMaybeVisibility :: forall a . Maybe a -> Visibility
fromMaybeVisibility val = if isNothing val
                        then GONE else VISIBLE

fromBooleanVisibility :: Boolean -> Visibility
fromBooleanVisibility val = if val then VISIBLE else GONE

dummyLinearLayout :: forall w . PrestoDOM (Effect Unit) w
dummyLinearLayout =
  linearLayout
    [ width WRAP_CONTENT
    , height $ V 0
    ][]