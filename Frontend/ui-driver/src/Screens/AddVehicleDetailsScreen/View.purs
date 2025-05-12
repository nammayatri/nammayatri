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
import Common.Animation.Config hiding (listExpandingAnimationConfig)
import Helpers.Utils as HU
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericMessageModal.View as GenericMessageModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.ReferralMobileNumber.View as ReferralMobileNumber
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Components.TutorialModal.View as TutorialModal
import Components.ValidateDocumentModal as ValidateDocumentModal
import Control.Monad.Trans.Class (lift)
import Data.String as DS
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn1)
import Engineering.Helpers.Commons (flowRunner)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import PaymentPage (consumeBP)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, discard, not, pure, unit, void, ($), (&&), (/=), (<<<), (<>), (==), (>=), (||),show)
import Presto.Core.Types.Language.Flow (Flow, doAff, delay)
import PrestoDOM (BottomSheetState(..), Gravity(..), InputType(..), adjustViewWithKeyboard,accessibility, accessibilityHint,onAnimationEnd,Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alignParentBottom, alignParentRight, alpha, background, clickable, color, cornerRadius, editText, ellipsize, fontStyle, frameLayout, gravity, height, hint, id, imageUrl, imageView, imageWithFallback, inputType, inputTypeI, layoutGravity, linearLayout, margin, maxLines, onBackPressed, onChange, onClick, orientation, padding, pattern, relativeLayout, scrollView, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Properties as PP
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Types.DomAttributes as PTD
import Screens.AddVehicleDetailsScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.RegistrationScreen.ComponentConfig (logoutPopUp)
import Screens.Types (AddVehicleDetailsScreenState, StageStatus(..), ValidationStatus(..))
import Styles.Colors as Color
import Types.App (GlobalState(..), defaultGlobalState)
import Data.String.Common as DSC
import Effect.Uncurried (runEffectFn1)
import ConfigProvider
import Mobility.Prelude
import Components.OptionsMenu as OptionsMenu
import Screens.RegistrationScreen.ComponentConfig (changeVehicleConfig)
import Data.Array as DA
import Components.BottomDrawerList as BottomDrawerList
import Screens.Types as ST
import Components.RequestInfoCard as RequestInfoCard
import Engineering.Helpers.Events as EHE
import Resource.Localizable.StringsV2 (getStringV2)
import Resource.Localizable.TypesV2

screen :: AddVehicleDetailsScreenState -> Screen Action AddVehicleDetailsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "AddVehicleDetailsScreen"
  , globalEvents : [(\push -> do
    _ <- JB.storeCallBackImageUpload push CallBackImageUpload
    _ <- runEffectFn1 consumeBP unit
    let _ = EHE.addEvent (EHE.defaultEventObject $ HU.getRegisterationStepScreenLoadedEventName ST.VEHICLE_DETAILS_OPTION ) { module = "vehicle_registration_page", source = "RC"}
    if initialState.props.successfulValidation then do
      _ <- launchAff $ flowRunner defaultGlobalState $ redirectScreen push RedirectScreen
      pure unit
    else pure unit
    pure $ pure unit
  )]
  , eval:
    ( \state action -> do
        let _ = spy "AddVehicleDetailsScreen ----- state" state
        let _ = spy "AddVehicleDetailsScreen --------action" action
        eval state action
    ) 
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
  ] $ [  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , PP.sheetState EXPANDED
      , background Color.white900
      , onBackPressed push (const BackPressed state.props.openRCManual)
      , onClick push (const ScreenClick)
      , afterRender push (const AfterRender)
      ][  PrestoAnim.animationSet
          [ Anim.fadeIn true
          ] $  headerView state push
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
                  ][  textView $
                      [ width MATCH_PARENT
                      , height WRAP_CONTENT
                      , text $ getString RC_VERIFICATION_FAILED
                      , color Color.black800
                      , background Color.redOpacity10
                      , padding $ Padding 16 12 16 12
                      , margin $ Margin 16 16 16 16
                      , cornerRadius 8.0
                      , visibility if state.data.dateOfRegistration == Nothing then GONE else VISIBLE            
                      ] <> FontStyle.body3 TypoGraphy
                    , vehicleRegistrationNumber state push
                    , howToUpload push state 
                    , dateOfRCRegistrationView push state
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
           , visibility if not (DSC.null state.data.errorMessage) then VISIBLE else GONE
           , color Color.red
           , gravity CENTER
           , padding( PaddingHorizontal 20 20)
           , margin (MarginBottom 10)
           ]
           , PrimaryButton.view (push <<< PrimaryButtonAction) (primaryButtonConfig state)
           , if state.props.openHowToUploadManual && not state.data.cityConfig.uploadRCandDL then skipButton push state else dummyLinearLayout]
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
    , if state.props.contactSupportModal /= ST.HIDE then BottomDrawerList.view (push <<< BottomDrawerListAC) (bottomDrawerListConfig state) else linearLayout[][]
    , if state.props.openReferralMobileNumber then
        linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT] 
        [ReferralMobileNumber.view (push <<< ReferralMobileNumberAction) (referalNumberConfig state)] else linearLayout [][]
    ] <> if DA.any (_ == true) [state.props.logoutModalView, state.props.confirmChangeVehicle] then [ popupModal push state ] else []
      <> if state.props.imageCaptureLayoutView then [imageCaptureLayout push state] else []
      <> if state.props.validateProfilePicturePopUp then [validateProfilePicturePopUp push state] else []
      <> if state.props.fileCameraPopupModal then [fileCameraLayout push state] else [] 
      <> if state.props.multipleRCstatus /= NOT_STARTED then [addRCFromProfileStatusView state push] else []
      <> if state.props.menuOptions then [menuOptionModal push state] else []
      <> if state.props.acModal then [RequestInfoCard.view (push <<< RequestInfoCardAction) (acModalConfig state)] else []
      <> if state.props.ambulanceModal then [RequestInfoCard.view (push <<< RequestAmbulanceFacility) (ambulanceModalConfig state)] else []
      <> if state.props.agreeTermsModal then [PopUpModal.view (push <<< AgreePopUp) (agreeTermsModalConfig state)] else []

menuOptionModal :: forall w. (Action -> Effect Unit) -> AddVehicleDetailsScreenState -> PrestoDOM (Effect Unit) w
menuOptionModal push state = 
  linearLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , padding $ PaddingTop 55
    ][ OptionsMenu.view (push <<< OptionsMenuAction) (optionsMenuConfig state) ]

headerView :: forall w. AddVehicleDetailsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerView state push = AppOnboardingNavBar.view (push <<< AppOnboardingNavBarAC) (appOnboardingNavBarConfig state)
  
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
  let appName = JB.getAppName unit
  in 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , padding (Padding 20 32 20 0)
  , visibility if state.props.openHowToUploadManual then GONE else VISIBLE
  , adjustViewWithKeyboard "true"
  ]
  [scrollView
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ]
  [  linearLayout
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
              , margin (MarginBottom 12)
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
          , stroke ("1," <> if ((DS.length state.data.vehicle_registration_number >= 2) && not validateRegistrationNumber (DS.take 2 state.data.vehicle_registration_number)) then Color.warningRed else state.data.config.themeColors.editTextNormalStroke) 
          , background state.data.config.themeColors.radioInactiveBackground
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
              , pattern "[0-9a-zA-Z]*,10"
              , id (EHC.getNewIDWithTag "VehicleRegistrationNumber")
              , onChange push (const VehicleRegistrationNumber state.props.input_data)
              , inputTypeI 4097
              ] <> FontStyle.subHeading1 TypoGraphy)
            ]
          , textView $ -- (Error Indication)
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text (getString $ CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER")
            , color Color.warningRed
            , fontStyle $ FontStyle.regular LanguageStyle
            , margin (MarginTop 10)
            , visibility if ((DS.length state.data.vehicle_registration_number >= 2) && not validateRegistrationNumber (DS.take 2 state.data.vehicle_registration_number)) then VISIBLE else GONE
            ] <> FontStyle.paragraphText TypoGraphy
          , textView $
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text (getString CHANGE_LOCATION)
            , color state.data.config.themeColors.highlightedTextColor
            , onClick push $ const ChangeLocation
            , visibility GONE
            , margin $ MarginTop 8
            ] <> FontStyle.tags TypoGraphy
          , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , visibility $ boolToVisibility state.data.cityConfig.uploadRCandDL
          ] [
              linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation HORIZONTAL
               , margin (MarginTop 25)
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
                , stroke ("1," <> state.data.config.themeColors.editTextNormalStroke) 
                , background state.data.config.themeColors.radioInactiveBackground
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
                    , pattern "[0-9a-zA-Z]*,10"
                    , id (EHC.getNewIDWithTag "ReenterVehicleRegistrationNumber")
                    , onChange push (const ReEnterVehicleRegistrationNumber state.props.input_data)
                    , inputTypeI 4097
                    ] <> FontStyle.subHeading1 TypoGraphy)
                  ]
              , textView
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , text (getString SAME_REENTERED_RC_MESSAGE)
                , visibility $ if (DS.toLower(state.data.vehicle_registration_number) /= DS.toLower(state.data.reEnterVehicleRegistrationNumber) && not (DSC.null state.data.reEnterVehicleRegistrationNumber )) then VISIBLE else GONE
                , color Color.darkGrey
                ]
          ]
          , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , visibility $ boolToVisibility $ state.data.vehicleCategory == Just (ST.AmbulanceCategory)
          ] [
              linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation HORIZONTAL
              , gravity CENTER_VERTICAL
               , margin (MarginTop 25)
              ][  textView
                  ([ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text (getString A_F)
                  , color Color.greyTextColor
                  , margin (MarginVertical 10 10)
                  ] <> FontStyle.body3 TypoGraphy)
                 , imageView
                    [ width (V 12)
                    , height (V 12)
                    , margin (MarginLeft 3)
                    , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_blue"
                    , color Color.black800
                    , onClick push $ const OpenAmbulanceFacilityModal
                    ]
                ] 
              , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                , stroke ("1," <> Color.borderColorLight) 
                , cornerRadius 4.0
                , gravity CENTER_VERTICAL
                , onClick push $ const $ SelectAmbulanceFacility
                
                ][  textView $
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , padding (Padding 19 17 0 17)
                    , color Color.greyTextColor
                    , text state.props.input_data
                    , textFromHtml $ if state.props.isvariant == "" then (getString SELECT_ONE) else case  state.props.isvariant of
                                                                                                      "AMBULANCE_TAXI" -> getString NON_AC <> "\x00B7" <> getStringV2 no_oxygen
                                                                                                      "AMBULANCE_TAXI_OXY" -> getString NON_AC <> "\x00B7" <> getStringV2 oxygen
                                                                                                      "AMBULANCE_AC" -> getString AC <> "\x00B7" <> getStringV2 no_oxygen
                                                                                                      "AMBULANCE_AC_OXY" -> getString AC <> "\x00B7" <> getStringV2 oxygen
                                                                                                      "AMBULANCE_VENTILATOR" -> getStringV2 ventilator
                                                                                                      _ -> "Other" 
                    , weight 4.0
                    , cornerRadius 6.0
                    , stroke ("3," <> Color.white900)
                    ]
                    , linearLayout
                      [ width WRAP_CONTENT
                      , height WRAP_CONTENT
                      , gravity CENTER_VERTICAL
                      ][ imageView
                          [ width (V 20)
                          , height (V 20)
                          , imageWithFallback $ fetchImage FF_COMMON_ASSET $ if state.props.facilities then "ny_ic_chevron_up_dark" else "ny_ic_chevron_down_light"
                          ]
                      ]
                    ]
                  , facilityListView state push
          ]
        , if appName == "ONDC fleetX" then rcEligibilityCriteriaView state push else checkACView state push
        ]
      ]
  ]
      where validateRegistrationNumber regNum = regNum `DA.elem` state.data.rcNumberPrefixList


facilityListView :: forall w . ST.AddVehicleDetailsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
facilityListView state push  = 
  PrestoAnim.animationSet ([] <>
    if EHC.os == "IOS" then
      [ Anim.fadeIn state.props.facilities 
      , Anim.fadeOut $ not state.props.facilities ]
    else
      [Anim.listExpandingAnimation $ listExpandingAnimationConfig state.props.facilities])
   $ 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , orientation VERTICAL
    , margin $ MarginTop 15 -- 16 16 0 
    , visibility $ boolToVisibility $ state.props.facilities
    , stroke ("1," <> Color.borderColorLight)
    , cornerRadius 4.0
    , onAnimationEnd push $ const ListExpandAinmationEnd
    ](DA.mapWithIndex (\index variant ->
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 16 13 16 13
        , onClick push $ const $ SelectAmbulanceVarient variant
        , orientation VERTICAL
        ][  linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            ][  textView $
                [ accessibilityHint $ show variant <> " : Button"
                , textFromHtml $ case variant of
                                    "AMBULANCE_TAXI" -> getString NON_AC <> "\x00B7" <> getStringV2 no_oxygen
                                    "AMBULANCE_TAXI_OXY" -> getString NON_AC <> "\x00B7" <> getStringV2 oxygen
                                    "AMBULANCE_AC" -> getString AC <> "\x00B7" <> getStringV2 no_oxygen
                                    "AMBULANCE_AC_OXY" -> getString AC <> "\x00B7" <> getStringV2 oxygen
                                    "AMBULANCE_VENTILATOR" -> getStringV2 ventilator
                                    _ -> "Other" 

                , color Color.darkCharcoal
                ] <> FontStyle.paragraphText LanguageStyle

              ]
          ]) ["AMBULANCE_VENTILATOR","AMBULANCE_AC_OXY", "AMBULANCE_AC","AMBULANCE_TAXI_OXY" ,"AMBULANCE_TAXI" ])


checkACView :: AddVehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
checkACView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 22
    , visibility $ boolToVisibility $ state.data.vehicleCategory == Just ST.CarCategory
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        ]
        [ textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ getString IS_YOUR_CAR_AC_WORKING
              , color Color.greyTextColor
              , margin $ MarginVertical 10 10
              ]
            <> FontStyle.body3 TypoGraphy
        , imageView
            [ width $ V 12
            , height $ V 12
            , margin $ MarginLeft 5
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_grey"
            , onClick push $ const OpenAcModal
            ]
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        ]
        $ DA.mapWithIndex
            ( \index item ->
                linearLayout
                  ( [ weight 1.0
                    , height WRAP_CONTENT
                    , orientation VERTICAL
                    , cornerRadius 8.0
                    , gravity CENTER
                    , onClick push $ const $ SelectButton index
                    , margin $ MarginLeft if index == 0 then 0 else 10
                    ]
                      <> case state.props.buttonIndex of
                          Just ind ->
                            [ stroke $ "1," <> if ind == index then Color.blue900 else Color.grey800
                            , background if ind == index then Color.blue600 else Color.white900
                            ]
                          Nothing ->
                            [ stroke $ "1," <> Color.grey800
                            , background Color.white900
                            ]
                  )
                  [ textView
                      $ [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , text item
                        , color Color.black800
                        , padding $ Padding 10 12 10 12
                        ]
                      <> FontStyle.subHeading1 TypoGraphy
                  ]
            )
            [ getString YES, getString NO ]
    ]

rcEligibilityCriteriaView :: AddVehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
rcEligibilityCriteriaView state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity LEFT
    , margin $ MarginTop 16
    ]
    [ textView
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , text "Eligibility criteria for RC"
      , color Color.grey600
      ]
    , textView
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , text "-   Your car must be less than 3 years old"
      , margin $ Margin 16 8 0 0
      ]
    , textView
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , text "-   Your car must be a sedan vehicle"
      , margin $ Margin 16 8 0 0
      ]
    ]


----------------------------------------------------------------- uploadRC ------------------------------------------------------
uploadRC :: AddVehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
uploadRC state push =
  let feature = (getAppConfig appConfig).feature
  in
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
            , text $ (getString UPLOAD_REGISTRATION_CERTIFICATE)
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
          [ width MATCH_PARENT
          , height WRAP_CONTENT
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
                  ] <> FontStyle.subHeading1 TypoGraphy) 
                , linearLayout
                  [ height MATCH_PARENT
                  , width WRAP_CONTENT
                  , margin (Margin 0 10 20 10)
                  ][ uploadIcon state push
                    , previewIcon state push
                    ]
                ]
        ]]
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
  , margin $ MarginTop 20
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
      , stroke $ "1," <> Color.brightBlue
      , alpha 0.8
      ][ textView
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , text $ getString HELP
          , color Color.brightBlue
          , fontStyle $ FontStyle.semiBold LanguageStyle
          , clickable true
          , onClick push $ const $ TutorialModal "RC"
          ]
      ]
    ]

howToUpload :: (Action -> Effect Unit) ->  AddVehicleDetailsScreenState -> forall w . PrestoDOM (Effect Unit) w
howToUpload push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin (Margin 20 20 15 0) 
  , visibility if state.props.openHowToUploadManual && state.data.dateOfRegistration == Nothing then VISIBLE else GONE 
  ][ textView $ 
    [ text $ getString HOW_TO_UPLOAD
    , color Color.greyTextColor
    ] <> FontStyle.h3 TypoGraphy
  , linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginVertical 0 10
    , padding $ PaddingVertical 16 16
    ][ 
      textView $ 
      [ text $ getString TAKE_CLEAR_PICTURE_RC
      , color Color.black800
      , margin $ MarginBottom 18
      ] <> FontStyle.body3 TypoGraphy

      , textView $ 
      [ text $ getString ENSURE_ADEQUATE_LIGHT
      , color Color.black800
      , margin $ MarginBottom 18
      ] <> FontStyle.body3 TypoGraphy

      , textView $ 
      [ text $ getString FIT_RC_CORRECTLY
      , margin $ MarginBottom 40
      , color Color.black800
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

rightWrongView :: Boolean -> forall w . PrestoDOM (Effect Unit) w
rightWrongView isRight = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER_VERTICAL
  , margin $ MarginBottom 16
  ][ imageView
    [ width $ V 120
    , height $ V if isRight then 80 else 100
    , imageWithFallback $ fetchImage FF_ASSET if isRight then "ny_ic_upload_right" else "ny_ic_image_wrong"
    ]
  , linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , padding $ Padding 16 16 0 0
    , gravity CENTER
    ][ rightWrongItemView isRight $ if isRight then (getString CLEAR_IMAGE) else (getString BLURRY_IMAGE)
     , rightWrongItemView isRight $ if isRight then (getString CROPPED_CORRECTLY) else (getString WRONG_CROPPING)
    ]
  ]

rightWrongItemView :: Boolean -> String -> forall w . PrestoDOM (Effect Unit) w
rightWrongItemView isRight text' = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ MarginBottom 5
  , gravity CENTER_VERTICAL
  ][ imageView
    [ width $ V 16
    , height $ V 16
    , imageWithFallback $ fetchImage FF_ASSET $ if isRight then "ny_ic_green_tick" else "ny_ic_payment_failed"
    ]
  , textView $
    [ text text'
    , color Color.black800
    , margin $ MarginLeft 8
    ] <> FontStyle.body1 TypoGraphy
  ]

popupModal :: forall w . (Action -> Effect Unit) -> AddVehicleDetailsScreenState -> PrestoDOM (Effect Unit) w
popupModal push state =
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.blackLessTrans
    ][ PopUpModal.view (push <<< action) popupConfig ] 
    where 
      action = if state.props.logoutModalView then PopUpModalLogoutAction 
                else ChangeVehicleAC
      popupConfig = if state.props.logoutModalView then (logoutPopUp Language)
                    else changeVehicleConfig FunctionCall

skipButton :: forall w . (Action -> Effect Unit) -> AddVehicleDetailsScreenState -> PrestoDOM (Effect Unit) w
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

validateProfilePicturePopUp :: forall w . (Action -> Effect Unit) -> AddVehicleDetailsScreenState -> PrestoDOM (Effect Unit) w
validateProfilePicturePopUp push state =
  ValidateDocumentModal.view (push <<< ValidateDocumentModalAction) (validateProfilePictureModalState state)

validateProfilePictureModalState :: AddVehicleDetailsScreenState -> ValidateDocumentModal.ValidateDocumentModalState
validateProfilePictureModalState state = let
      config' = ValidateDocumentModal.config
      inAppModalConfig' = config'{
        background = Color.black,
        profilePictureCapture = false,
        verificationStatus = if state.props.validating then InProgress 
                             else if state.data.errorMessage /= "" then Failure
                             else if state.data.rcImageID /= "" then Success
                             else None,
        verificationType = "RC",
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

imageCaptureLayout :: forall w . (Action -> Effect Unit) -> AddVehicleDetailsScreenState -> PrestoDOM (Effect Unit) w
imageCaptureLayout push state  =ValidateDocumentModal.view (push <<< ValidateDocumentModalAction) (ValidateDocumentModal.config{background = Color.black,profilePictureCapture =true ,headerConfig {headTextConfig {text = getString TAKE_PHOTO}}})

fileCameraLayout :: forall w . (Action -> Effect Unit) -> AddVehicleDetailsScreenState -> PrestoDOM (Effect Unit) w
fileCameraLayout push state =
  PopUpModal.view (push <<< PopUpModalActions)  (fileCameraLayoutConfig state)

redirectScreen :: forall action. (action -> Effect Unit) ->  action -> Flow GlobalState Unit
redirectScreen push action = do
  void $ delay $ Milliseconds 1000.0
  doAff do liftEffect $ push $ action
  pure unit

addRCFromProfileStatusView :: AddVehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
addRCFromProfileStatusView state push = 
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , background Color.blue600
  ][ linearLayout
     [ width MATCH_PARENT
     , height MATCH_PARENT
     , gravity CENTER
     , orientation VERTICAL
     , margin $ MarginBottom 60
     ][ imageView
        [ imageWithFallback $ fetchImage FF_ASSET case state.props.multipleRCstatus of
                                                        COMPLETED -> "ny_ic_rc_success"
                                                        FAILED -> "ny_ic_rc_failed"
                                                        IN_PROGRESS -> "ny_ic_rc_pending"
                                                        _ -> "ny_ic_rc_pending"
        , height $ V 120
        , width $ V 230
        ]
      , textView $
        [ text $ getString case state.props.multipleRCstatus of
                              COMPLETED -> RC_VERIFICATION_SUCCESS
                              FAILED -> RC_VERIFICATION_FAILED_STATUS
                              IN_PROGRESS -> RC_VERIFICATION_IN_PROGRESS
                              _ -> RC_VERIFICATION_IN_PROGRESS
        , color Color.black800
        , margin $ MarginTop 27
        ] <> FontStyle.h2 TypoGraphy
     ]
  , linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , alignParentBottom "true,-1"
    , cornerRadii $ Corners 24.0 true true false false
    , stroke $ "1," <> Color.borderGreyColor
    , background Color.white900
    , gravity CENTER
    ][ textView $
      [ text $ getString ACTIVATE_RC
      , color Color.black800
      , margin $ MarginTop 27
      ] <> FontStyle.h3 TypoGraphy
    , textView $
      [ text $ case state.props.multipleRCstatus of
                              COMPLETED -> getString CONFIRMATION_FOR_ACTIVATING_RC <> state.data.vehicle_registration_number <> "? " <> getString THIS_WILL_DEACTIVATE_CURRENTLY_ACTIVE_RC
                              FAILED -> getString RC_FAILED_DESC
                              IN_PROGRESS -> getString RC_IN_PROGRESS_DESC
                              _ -> getString RC_IN_PROGRESS_DESC
      , color Color.black800
      , margin $ Margin 16 16 16 32
      ] <> FontStyle.paragraphText TypoGraphy
    , PrimaryButton.view (push <<< ActivateRCbtn) (activateRcButtonConfig state)
    , textView $
      [ text $ getString case state.props.multipleRCstatus of
                              COMPLETED -> SKIP
                              FAILED -> CONTACT_SUPPORT
                              IN_PROGRESS -> CONTACT_SUPPORT
                              _ -> RC_VERIFICATION_IN_PROGRESS
      , color Color.black650
      , margin $ Margin 16 6 16 24
      , padding $ PaddingVertical 8 8
      , onClick push $ const CancelButtonMultiRCPopup
      ] <> FontStyle.subHeading2 TypoGraphy
    ]
  ]

dummyLinearLayout :: forall w . PrestoDOM (Effect Unit) w
dummyLinearLayout =
  linearLayout
    [ width WRAP_CONTENT
    , height $ V 0
    ][]