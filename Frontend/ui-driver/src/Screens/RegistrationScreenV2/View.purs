{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RegistrationScreenV2.View where

import Common.Types.App
import Debug
import Mobility.Prelude
import Screens.RegistrationScreenV2.ComponentConfig

import Animation as Anim
import Common.Animation.Config as AnimConfig
import Animation.Config as AnimConf
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Components.InAppKeyboardModal as InAppKeyboardModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Control.Monad.ST (for)
import Data.Array (all, any, elem, filter, fold, length, mapWithIndex, find)
import Data.Array as DA
import Data.Foldable (foldl, and)
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe)
import Data.String as DS
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..),getCityConfig)
import JBridge (lottieAnimationConfig, startLottieProcess)
import Language.Strings (getString, getVarString)
import Resource.Localizable.StringsV2
import Resource.Localizable.TypesV2 as LT2
import Language.Types (STR(..))
import PaymentPage (consumeBP)
import Prelude (Unit, bind, const, map, not, pure, show, unit, void, ($), (&&), (+), (-), (<<<), (<>), (==), (>=), (||), (/=), (*), (>), (/), discard)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), afterRender, alignParentBottom, background, clickable, color, cornerRadius, editText, fontStyle, gravity, height, hint, id, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, lottieAnimationView, margin, onAnimationEnd, onBackPressed, onChange, onClick, orientation, padding, pattern, relativeLayout, stroke, text, textSize, textView, visibility, weight, width, scrollView, scrollBarY, fillViewport, alpha, textFromHtml, nestedScrollView)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.RegistrationScreenV2.Controller (Action(..), eval, ScreenOutput)
import Screens.Types (RegisterationStep(..), StageStatus(..), ValidationStatus(..))
import Screens.Types as ST
import Services.API as API
import Storage (KeyStore(..), getValueToLocalNativeStore)
import Storage (getValueToLocalStore, KeyStore(..), setValueToLocalStore)
import Styles.Colors as Color
import Screens.RegistrationScreen.ScreenData as SD
import Resource.Constants as Constant
import Data.Int (toNumber, floor, fromString)
import Components.OptionsMenu as OptionsMenu
import Components.BottomDrawerList as BottomDrawerList
import Helpers.Utils as HU
import Engineering.Helpers.Events as EHE
import Effect.Uncurried (runEffectFn2)
import Engineering.Helpers.Commons (flowRunner)
import Types.App (GlobalState, defaultGlobalState)
import Presto.Core.Types.Language.Flow (Flow, delay)
import Effect.Aff (Milliseconds(..), launchAff)
import RemoteConfig as RC
import Log
import DecodeUtil

screen :: ST.RegistrationScreenState -> Screen Action ST.RegistrationScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "RegistrationScreenV2"
  , globalEvents : [ globalActions ]
  , eval :
      ( \state action -> do
          let _ = spy "RegistrationScreenV2 ----- state" state
          let _ = spy "RegistrationScreenV2 --------action" action
          eval state action
      )
  }
  where 
  globalActions push = do
    let eventName = if isNothing initialState.data.vehicleCategory then "vehicle_selection_page_loaded" else "vehicle_registration_page_loaded"
    void $ pure $ EHE.addEvent (EHE.defaultEventObject eventName)
    if (getValueToLocalStore LOGS_TRACKING == "true" && getValueToLocalStore FUNCTION_EXECUTED_IN_SESSION /= "true") then do
      void $ launchAff $ flowRunner defaultGlobalState $ runLogTracking push NoAction
      void $ pure $ setValueToLocalStore FUNCTION_EXECUTED_IN_SESSION "true"
      pure unit
    else pure unit
    pure $ pure unit

view ::
  forall w.
  (Action -> Effect Unit) ->
  ST.RegistrationScreenState ->
  PrestoDOM (Effect Unit) w
view push state =
  let showSubscriptionsOption = (getValueToLocalNativeStore SHOW_SUBSCRIPTIONS == "true") && state.data.config.bottomNavConfig.subscription.isVisible
      completedStatusCount = length $ filter (\doc -> statusCompOrManual (getStatus doc.stage state)) documentList
      categoryCompletedStatusCount = getCategoryCompletedStatusCount state.props.categoryToStepProgressMap state
      variantImage = case state.data.vehicleCategory of
        Just ST.AutoCategory -> "ny_ic_auto_side"
        Just ST.BikeCategory -> "ny_ic_bike_side"
        Just ST.AmbulanceCategory -> "ny_ic_ambulance_side"
        Just ST.CarCategory -> "ny_ic_sedan_side"
        Just ST.TruckCategory -> "ny_ic_truck_side"
        Just ST.BusCategory -> "ny_ic_bus_side"
        Just ST.UnKnown -> ""
        Nothing -> ""
  in
    Anim.screenAnimation
      $ relativeLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , background Color.white900
          , clickable true
          , onBackPressed push (const BackPressed)
          , afterRender
              ( \action -> do
                  _ <- push action
                  pure unit
              )
              $ const (AfterRender)
          ]
      $ [ chooseVehicleView push state
        , linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , gravity CENTER
            , orientation VERTICAL
            , visibility $ boolToVisibility $ isJust state.data.vehicleCategory
            ]
            [ headerView state push                    
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , padding $ Padding 16 16 16 0
                , weight 1.0
                ]
                [ linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , orientation VERTICAL
                    ]
                    [ linearLayout
                        [ width MATCH_PARENT
                        , height WRAP_CONTENT
                        , margin $ MarginBottom 20
                        , gravity CENTER_VERTICAL
                        , visibility $ boolToVisibility $ isNothing state.props.selectedDocumentCategory
                        ]
                        [ imageView
                            [ width $ V 32
                            , height $ V 32
                            , margin $ MarginRight 4
                            , imageWithFallback $ fetchImage FF_ASSET variantImage 
                            ]
                        , textView
                            $ [ width WRAP_CONTENT
                              , height WRAP_CONTENT
                              , text $ getString COMPLETE_THE_STEPS_TO_START_EARNING
                              , weight 1.0
                              ]
                            <> FontStyle.body2 TypoGraphy
                        , textView
                            $ [ width WRAP_CONTENT
                              , height WRAP_CONTENT
                              , text $ show categoryCompletedStatusCount <> "/" <> (show $ DA.length state.props.categoryToStepProgressMap)
                              ]
                            <> FontStyle.body2 TypoGraphy
                        ]
                    , if isNothing state.props.selectedDocumentCategory then categoryListView push state else categorySpecificList push state -- cardsListView push state
                    ]
                ]
            , refreshView push state
            , linearLayout
                [ height $ V 1
                , width MATCH_PARENT
                , background Color.grey900
                , margin $ MarginBottom 16
                , visibility $ boolToVisibility state.props.driverEnabled
                ]
                []
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                , margin $ Margin 8 0 8 16
                , visibility $ boolToVisibility (buttonVisibility categoryCompletedStatusCount)
                ]
                [ PrimaryButton.view (push <<< PrimaryButtonAction) (primaryButtonConfig state) ]
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                , margin $ Margin 16 0 16 16
                , visibility $ boolToVisibility $ (state.props.selectedDocumentCategory == Just API.DRIVER) || (state.props.selectedDocumentCategory == Just API.VEHICLE)
                ]
                [ PrimaryButton.view (push <<< CategorySpecificContinueButtonAC) (continueCategorySpecificButtonConfig state) ]
            ]
            , if state.props.enterReferralCodeModal then enterReferralCodeModal push state else linearLayout[][]
        ]
      <> if any (_ == true) [state.props.logoutModalView, state.props.confirmChangeVehicle, state.data.vehicleTypeMismatch] then [ popupModal push state ] else []
      <> if state.props.contactSupportModal /= ST.HIDE then [contactSupportModal push state] else []
      <> if state.props.menuOptions then [menuOptionModal push state] else []
      where 
        callSupportVisibility = (state.data.drivingLicenseStatus == ST.FAILED && state.data.enteredDL /= "__failed") || (state.data.vehicleDetailsStatus == ST.FAILED && state.data.enteredRC /= "__failed")
        documentList = case state.data.vehicleCategory of
                      Just ST.CarCategory -> state.data.registerationStepsCabs
                      Just ST.TruckCategory -> state.data.registerationStepsTruck
                      Just ST.BikeCategory -> state.data.registerationStepsBike
                      Just ST.AmbulanceCategory -> state.data.registerationStepsAmbulance
                      Just ST.AutoCategory -> state.data.registerationStepsAuto
                      Just ST.BusCategory -> state.data.registerationStepsBus
                      _ -> state.data.registerationStepsAuto
        buttonVisibility categoryCompletedStatusCount = if state.props.manageVehicle then all (\docType -> statusCompOrManual (getStatus docType.stage state)) $ filter(\elem -> elem.isMandatory) documentList
                            else if state.props.driverEnabled then state.props.driverEnabled
                            else isNothing state.props.selectedDocumentCategory && (categoryCompletedStatusCount == (DA.length state.props.categoryToStepProgressMap))  -- Todo: add this if that dummy screen is needed and remove from top. isNothing state.props.selectedDocumentCategory && (categoryCompletedStatusCount == (DA.length state.props.categoryToStepProgressMap))) 


headerView :: forall w. ST.RegistrationScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerView state push = AppOnboardingNavBar.view (push <<< AppOnboardingNavBarAC) (appOnboardingNavBarConfig state)

menuOptionModal :: forall w. (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
menuOptionModal push state = 
  linearLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , padding $ PaddingTop 55
    ][ OptionsMenu.view (push <<< OptionsMenuAction) (optionsMenuConfig state) ]

contactSupportView :: forall w. (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
contactSupportView push state = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , stroke $ "1," <> Color.grey900
    , cornerRadius 4.0
    , padding $ Padding 10 8 10 8
    , gravity CENTER_VERTICAL
    , onClick push $ const $ SupportClick true
    , visibility $ boolToVisibility $ viewVisibility
    ][  textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , color Color.black800
        , text $ getString NEED_HELP
        , weight 1.0
        ] <> FontStyle.body3 TypoGraphy
      , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getString CONTACT_SUPPORT
        , margin $ MarginRight 7
        , color Color.blue900
        ] <> FontStyle.tags TypoGraphy
      ]
      where viewVisibility = state.props.contactSupportView && (state.data.cityConfig.registration.callSupport || state.data.cityConfig.registration.whatsappSupport)

contactSupportModal :: forall w. (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
contactSupportModal push state = BottomDrawerList.view (push <<< BottomDrawerListAC) (bottomDrawerListConfig state)

commonTV :: forall w .  (Action -> Effect Unit) -> String -> String -> (LazyCheck -> forall properties. (Array (Prop properties))) -> Gravity -> Int -> Action -> Boolean -> Padding -> PrestoDOM (Effect Unit) w
commonTV push text' color' theme gravity' marginTop action visibility' padding' = 
  textView $
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , color color'
  , gravity gravity'
  , margin $ MarginTop marginTop
  , padding padding'
  , text text'
  , visibility $ boolToVisibility visibility'
  ] <> theme TypoGraphy
    <>  if action == NoAction then []
        else [onClick push $ const action]

categoryListView :: forall w. (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
categoryListView push state =
  let registerationSteps = state.data.registerationStepsCabs 
      categories = filterCategories registerationSteps state.props.categoryToStepProgressMap state.data.documentStatusList state.props.vehicleImagesUploaded
      referralCodeAlreadyApplied = not $ DA.any (_ == (getValueToLocalStore APPLIED_REFERRAL_CODE)) ["__failed", ""]
  in
  scrollView
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , scrollBarY false
    , fillViewport true
    , margin $ MarginBottom 20
    ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , weight 1.0
        ][ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            ][ 
              linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                ]([linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , orientation HORIZONTAL
                    , padding $ Padding 12 12 12 12
                    , cornerRadius 8.0
                    , background Color.blue500
                    , margin (MarginBottom 20)
                    , gravity CENTER_VERTICAL
                    , onClick push $ const $ EnterReferralCode true
                    , clickable $ not referralCodeAlreadyApplied
                    ][ linearLayout
                        [ height WRAP_CONTENT
                        , weight 1.0
                        , orientation HORIZONTAL
                        ][ textView
                            [ height WRAP_CONTENT
                            , color Color.black800
                            , text $ if referralCodeAlreadyApplied then (getStringV2 LT2.operator_referral_code_applied) <> ": " <> (getValueToLocalStore APPLIED_REFERRAL_CODE) else (getStringV2 LT2.got_an_operator_referral_code)
                            ]
                        , textView
                            [ text $ getStringV2 LT2.optional_str
                            , height WRAP_CONTENT
                            , width WRAP_CONTENT
                            , background $ Color.grey300
                            , cornerRadius 12.0
                            , padding $ Padding 8 2 8 2
                            , color Color.black900
                            , margin $ MarginLeft 8
                            , visibility $ boolToVisibility $ not referralCodeAlreadyApplied
                            ]
                        ]
                      , imageView
                        [ imageWithFallback $ fetchImage COMMON_ASSET ( if referralCodeAlreadyApplied then "ny_ic_green_tick" else "ny_ic_chevron_right")
                        , width (V 20)
                        , height (V 20)
                        ]

                    ]
                  ] <> (map (\item -> categoryListItem push item state) categories))
            ]
          ]
      ]

categoryListItem :: forall w. (Action -> Effect Unit) -> ST.CategoryToStepMap -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
categoryListItem push item state = 
  let itemVisibility = DA.length item.registrationSteps >= 0
  in
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ Padding 12 12 12 12
    , cornerRadius 8.0
    , visibility $ boolToVisibility itemVisibility
    , stroke $ "1," <> getStrokeColor state item
    , background $ getBg state item
    , clickable $ checkClickable state item
    , alpha $ getAlpha item
    , onClick push $ const $ RegistrationActionV2 item
    , margin (MarginBottom 20)
    , gravity CENTER_VERTICAL
    ][  linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , background $ getImageBg state item -- do a case match for this also for permissions 
        , padding $ Padding 8 8 8 8
        , cornerRadius 24.0
        , margin (MarginRight 14)
        ][ imageView
          [ imageWithFallback $ fetchImage FF_ASSET $ getImage item.category
          , width $ V 24
          , height $ V 24
          ]
        ]
    ,  linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER_VERTICAL
      , weight 1.0
      ][ textView $
          [ text $ getText item.category
          , color Color.black800
          ] <> FontStyle.body1 TypoGraphy
      ]
        , imageView
          [ imageWithFallback $ fetchImage COMMON_ASSET (getStatusImg state item)
          , width (V 20)
          , height (V 20)
          ]
    ]
    where 
      getImage category = 
            case category of
              API.VEHICLE -> "ny_ic_my_vehicle"
              API.DRIVER -> "ny_ic_my_profile"
              API.PERMISSION -> "ny_ic_grant_permission_purple"
              API.TRAINING -> "ny_ic_trainings"
              _ -> ""
      getText category = 
            case category of
              API.VEHICLE -> getString MY_VEHICLE
              API.DRIVER -> getString MY_PROFILE
              API.PERMISSION -> getString APP_PERMISSIONS
              API.TRAINING -> getString TRAININGS
              _ -> ""
      getImageBg state item = 
        case item.category of
          API.PERMISSION -> if state.data.permissionsStatus == ST.COMPLETED then Color.white900 else state.data.config.themeColors.onboardingStepImgBg
          API.TRAINING -> if checkIfTrainingsEnabled state && state.data.trainingsCompletionStatus == ST.COMPLETED then Color.white900 else state.data.config.themeColors.onboardingStepImgBg
          _ -> if item.completionStatus == ST.COMPLETED then Color.white900 else state.data.config.themeColors.onboardingStepImgBg
      getAlpha item = 
        case item.category of
          API.TRAINING -> if checkIfTrainingsEnabled state then 1.0 else 0.5
          _ -> 1.0
      getBg state item = 
        case item.category of
          API.PERMISSION -> if state.data.permissionsStatus == ST.COMPLETED then Color.greenOpacity10 else Color.white900
          API.TRAINING -> if checkIfTrainingsEnabled state && state.data.trainingsCompletionStatus == ST.COMPLETED then Color.greenOpacity10 else Color.white900
          _ -> getComponentBgColor item.completionStatus
      checkClickable state item = 
        case item.category of
          API.PERMISSION -> not $ state.data.permissionsStatus == ST.COMPLETED
          API.TRAINING -> checkIfTrainingsEnabled state && (not $ state.data.trainingsCompletionStatus == ST.COMPLETED)
          _ -> not $ statusCompOrManual item.completionStatus -- (Todo: Shikhar change to this after testing)
      getStatusImg state item = 
        case item.category of
          API.PERMISSION -> if state.data.permissionsStatus == ST.COMPLETED then "ny_ic_green_tick" else "ny_ic_chevron_right"
          API.TRAINING -> if checkIfTrainingsEnabled state && state.data.trainingsCompletionStatus == ST.COMPLETED then "ny_ic_green_tick" else "ny_ic_chevron_right"
          _ -> getComponentStatusImg item.completionStatus -- if item.completionStatus == ST.COMPLETED then "ny_ic_green_tick" else "ny_ic_chevron_right"
      getStrokeColor state item = 
        case item.category of
          API.PERMISSION -> if state.data.permissionsStatus == ST.COMPLETED then Color.green900 else Color.grey300
          API.TRAINING -> if checkIfTrainingsEnabled state && state.data.trainingsCompletionStatus == ST.COMPLETED then Color.green900 else Color.grey300
          _ -> getComponentStrokeColor item.completionStatus
      checkIfTrainingsEnabled state =
        let isProfileCompleted = checkCategoryStageStatus state.props.categoryToStepProgressMap API.DRIVER
            isVehicleCompleted = checkCategoryStageStatus state.props.categoryToStepProgressMap API.VEHICLE
        in isProfileCompleted && isVehicleCompleted
      checkCategoryStageStatus categoryToStepProgressMap category = 
        let categoryMap = (fromMaybe {category: API.NONE, registrationSteps: [], completionStatus: ST.NOT_STARTED, showContinueButton: false} $ DA.find (\item -> item.category == category) categoryToStepProgressMap)
        in statusCompOrManual categoryMap.completionStatus


categorySpecificList :: forall w. (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
categorySpecificList push state = 
  let findStepsForCategory = fromMaybe {category: API.NONE, registrationSteps: [], completionStatus: ST.NOT_STARTED, showContinueButton: false} $ DA.find (\item -> Just item.category == state.props.selectedDocumentCategory ) state.props.categoryToStepProgressMap
  in
   scrollView
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , scrollBarY false
    , fillViewport true
    , margin $ MarginBottom 20
    ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , weight 1.0
        ][ vehicleSpecificList push state findStepsForCategory.registrationSteps
        -- , if state.props.selectedDocumentCategory == Just API.VEHICLE then operationhubView push state else lin earLayout[][]
        ]
    ]

vehicleSpecificList :: forall w. (Action -> Effect Unit) -> ST.RegistrationScreenState -> Array ST.StepProgress -> PrestoDOM (Effect Unit) w
vehicleSpecificList push state registerationSteps = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][  linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ](map (\item -> listItem push item state) registerationSteps)
    ]

listItem :: forall w. (Action -> Effect Unit) ->  ST.StepProgress -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
listItem push item state = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ Padding 12 12 12 12
    , cornerRadius 8.0
    -- , visibility $ boolToVisibility $ (( if item.stage == ST.PAN_CARD || item.stage == ST.AADHAAR_CARD || item.stage == ST.PROFILE_PHOTO then state.data.config.showProfileAadhaarPan else true))
    , stroke $ componentStroke state item
    , background $ compBg state item
    , clickable $ compClickable state item
    , alpha $ compAlpha state item
    , onClick push $ const $ (if not state.props.dontAllowHvRelaunch then RegistrationAction item else NoAction) 
    -- , onClick push $ const $ RegistrationAction'
    , margin (MarginBottom 20)
    , gravity CENTER_VERTICAL
    ][  linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , background case getStatus item.stage state of
                              ST.NOT_STARTED -> state.data.config.themeColors.onboardingStepImgBg
                              _ -> Color.white900
        , cornerRadius 24.0
        , padding $ Padding 8 8 8 8
        , margin (MarginRight 14)
        ][ imageView
          [ imageWithFallback $ compImage item
          , width $ V 24
          , height $ V 24
          ]
        ]
    ,  linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER_VERTICAL
      , weight 1.0
      ][ textView $
          [ text item.stageName
          , color Color.black800
          ] <> FontStyle.body1 TypoGraphy
        , textView $
          [ color Color.black700
          ] <> FontStyle.body3 TypoGraphy
            <> case (getVerificationMessage item.stage state) of
                Just txt -> [ textFromHtml $ txt <> if statusFailed then retryStr else ""
                            , visibility $ boolToVisibility $ docUploadStarted]
                Nothing -> [visibility GONE]
      ]
        , imageView
          [ imageWithFallback $ compStatusImg state item
          , width (V 20)
          , height (V 20)
          ]
    ]
    where 
      showRetry = getStatus item.stage state == ST.FAILED && not checkLimitReached item.stage state.props.limitReachedFor
      docUploadStarted = getStatus item.stage state /= ST.NOT_STARTED
      statusFailed = (getStatus item.stage state) == ST.FAILED
      retryStr = " " <> "<span style='color:#171C8F'>"<> (getString UPLOAD_AGAIN) <>"</span>"

      compImage :: ST.StepProgress -> String
      compImage item = 
        fetchImage FF_ASSET $ case item.stage of
          ST.DRIVING_LICENSE_OPTION -> "ny_ic_dl_blue"
          ST.VEHICLE_DETAILS_OPTION -> if state.data.vehicleCategory == Just ST.CarCategory then "ny_ic_my_vehicle" else if state.data.vehicleCategory == Just ST.BikeCategory then "ny_ic_bike_onboard" else if state.data.vehicleCategory == Just ST.AmbulanceCategory then "ny_ic_ambulance_onboard" else if state.data.vehicleCategory == Just ST.TruckCategory then "ny_ic_truck_onboard" else if state.data.vehicleCategory == Just ST.BusCategory then "ny_ic_bus_onboard" else  "ny_ic_vehicle_onboard"
          ST.GRANT_PERMISSION -> "ny_ic_grant_permission"
          ST.SUBSCRIPTION_PLAN -> "ny_ic_plus_circle_blue"
          ST.PROFILE_PHOTO -> "ny_ic_profile_photo"
          ST.AADHAAR_CARD -> "ny_ic_aadhaar_blue"
          ST.PAN_CARD -> "ny_ic_pan"
          ST.VEHICLE_PERMIT -> "ny_ic_permit"
          ST.FITNESS_CERTIFICATE -> "ny_ic_fitness"
          ST.VEHICLE_INSURANCE -> "ny_ic_insurance"
          ST.VEHICLE_PUC -> "ny_ic_puc"
          ST.VEHICLE_PHOTOS -> "ny_ic_vehicle_scan"
          ST.INSPECTION_HUB -> "ny_ic_inspection_hub"
          _ -> ""

      componentStroke :: ST.RegistrationScreenState -> ST.StepProgress -> String
      componentStroke state item = 
        let strokeWidth = "1,"
            status = getStatus item.stage state
            colour = getComponentStrokeColor status
        in strokeWidth <> colour

      compBg :: ST.RegistrationScreenState -> ST.StepProgress -> String
      compBg state item = 
        let status = getStatus item.stage state
        in getComponentBgColor status

      compClickable :: ST.RegistrationScreenState -> ST.StepProgress -> Boolean
      compClickable state item = dependentDocAvailable item state && not item.isDisabled && not 
        case item.stage of
          ST.DRIVING_LICENSE_OPTION -> state.props.limitReachedFor == Just "DL" || any (_ == state.data.drivingLicenseStatus) [COMPLETED, IN_PROGRESS]
          ST.GRANT_PERMISSION -> statusCompOrManual state.data.permissionsStatus
          _ -> statusCompOrManual (getStatus item.stage state)

      compAlpha :: ST.RegistrationScreenState -> ST.StepProgress -> Number
      compAlpha state item = if dependentDocAvailable item state && not item.isDisabled then 1.0 else 0.5

      compStatusImg :: ST.RegistrationScreenState -> ST.StepProgress -> String
      compStatusImg state item = 
        let status = getStatus item.stage state
        in getComponentStatusImg status

      getVerificationMessage :: ST.RegisterationStep -> ST.RegistrationScreenState -> Maybe String
      getVerificationMessage step state = 
        case step of
          GRANT_PERMISSION -> Nothing
          VEHICLE_PHOTOS -> Nothing
          _ -> let currentDoc = find (\docStatus -> docStatus.docType == step) state.data.documentStatusList
                in case currentDoc of
                    Just doc -> doc.verificationMessage
                    _ -> Nothing

getComponentStrokeColor :: ST.StageStatus -> String
getComponentStrokeColor status = 
  case status of
    ST.COMPLETED -> Color.green900
    ST.MANUAL_VERIFICATION_REQUIRED -> Color.green900
    ST.IN_PROGRESS -> Color.yellow900
    ST.NOT_STARTED -> Color.grey300
    ST.FAILED -> Color.red
    _ -> Color.grey300

getComponentBgColor :: ST.StageStatus -> String
getComponentBgColor status = 
  case status of
    ST.COMPLETED -> Color.greenOpacity10
    ST.MANUAL_VERIFICATION_REQUIRED ->  Color.greenOpacity10
    ST.IN_PROGRESS -> Color.yellowOpacity10
    ST.NOT_STARTED -> Color.white900
    ST.FAILED -> Color.redOpacity10
    _ -> Color.white900
  
getComponentStatusImg :: ST.StageStatus -> String
getComponentStatusImg status = 
  case status of
    ST.COMPLETED -> fetchImage COMMON_ASSET "ny_ic_green_tick"
    ST.MANUAL_VERIFICATION_REQUIRED -> fetchImage COMMON_ASSET "ny_ic_green_tick"
    ST.IN_PROGRESS -> fetchImage COMMON_ASSET "ny_ic_pending"
    ST.NOT_STARTED -> fetchImage COMMON_ASSET "ny_ic_chevron_right"
    ST.FAILED -> fetchImage COMMON_ASSET "ny_ic_warning_filled_red"

popupModal :: forall w . (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
popupModal push state =
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.blackLessTrans
    ][ PopUpModal.view (push <<< action) popupConfig ] 
    where 
      action = if state.props.logoutModalView then PopUpModalLogoutAction 
                else if state.props.confirmChangeVehicle then ChangeVehicleAC
                else if state.data.vehicleTypeMismatch then VehicleMismatchAC
                else VehicleMismatchAC
      popupConfig = if state.props.logoutModalView then logoutPopUp Language
                    else if state.props.confirmChangeVehicle then changeVehicleConfig FunctionCall
                    else if state.data.vehicleTypeMismatch then vehicleMismatchConfig state
                    else vehicleMismatchConfig state

refreshView :: forall w . (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
refreshView push state =
  let documentList = case state.data.vehicleCategory of
                      Just ST.CarCategory -> state.data.registerationStepsCabs
                      Just ST.BikeCategory -> state.data.registerationStepsBike
                      Just ST.AutoCategory -> state.data.registerationStepsAuto
                      Just ST.AmbulanceCategory -> state.data.registerationStepsAmbulance
                      Just ST.TruckCategory -> state.data.registerationStepsTruck
                      Just ST.BusCategory -> state.data.registerationStepsBus
                      Just ST.UnKnown -> state.data.registerationStepsCabs
                      Nothing -> state.data.registerationStepsCabs
      showRefresh = any (_ == IN_PROGRESS) $ map (\item -> getStatus item.stage state) documentList
  in 
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , stroke $ "1,"<> Color.grey900
      , padding $ Padding 16 12 16 12
      , cornerRadius 8.0
      , alignParentBottom "true,-1"
      , onClick push $ const Refresh
      , margin $ Margin 16 0 16 16
      , visibility $ boolToVisibility $ showRefresh
      ][ textView $
          [ text $ getString LAST_UPDATED
          , gravity CENTER
          ] <> FontStyle.body3 TypoGraphy
        , textView $
          [ text state.data.lastUpdateTime
          , gravity CENTER
          , margin $ MarginLeft 6
          ] <> FontStyle.body15 TypoGraphy
        , linearLayout
          [ width WRAP_CONTENT
          , weight 1.0
          ][] 
        , PrestoAnim.animationSet [Anim.rotateAnim (AnimConf.rotateAnimConfig state.props.refreshAnimation)] $
          imageView
          [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_refresh"
          , height $ V 16
          , width $ V 16
          ]
        , textView $ 
          [ text $ getString REFRESH_STRING
          , color Color.blue800
          , margin $ MarginLeft 4
          ] <> FontStyle.body9 TypoGraphy
      ]

enterReferralCodeModal :: forall w . (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
enterReferralCodeModal push state =
  InAppKeyboardModal.view (push <<< InAppKeyboardModalAction) (enterReferralStateConfig state)

checkLimitReached :: ST.RegisterationStep -> Maybe String -> Boolean
checkLimitReached step limitReachedFor = 
  case limitReachedFor of
    Just "RC" -> step == ST.VEHICLE_DETAILS_OPTION
    Just "DL" -> step == ST.DRIVING_LICENSE_OPTION
    _ -> false

chooseVehicleView :: forall w . (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
chooseVehicleView push state = 
  relativeLayout 
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , clickable true
  , onBackPressed push (const BackPressed)
  , visibility $ boolToVisibility $ isNothing state.data.vehicleCategory
  ]
  [ linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    ]
    [ headerView state push
    , textView $ 
      [ text $ getString SELECT_YOUR_VEHICLE_TYPE
      , color Color.black700
      , margin $ Margin 16 24 16 8
      , height WRAP_CONTENT
      , width MATCH_PARENT
      ] <> FontStyle.body1 TypoGraphy
    , scrollView
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , scrollBarY false
      ]
      [ variantListView push state ]
    ]
  , linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , alignParentBottom "true,-1"
    , background Color.white900
    ]
    [  linearLayout 
      [ height $ V 1
      , width MATCH_PARENT
      , background Color.grey900
      ]
      []
    , PrimaryButton.view (push <<< ContinueButtonAction) (continueButtonConfig state)
    ]
  ]

variantListView :: forall w . (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
variantListView push state = 
  let cityConfig = getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION)
    in linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , padding $ Padding 16 0 16 94 -- Padding of Primary Button
      , gravity CENTER
      ]( mapWithIndex
          ( \index item -> 
              let stroke' = case state.props.selectedVehicleIndex of
                              Just i -> if i == index 
                                          then "2," <> Color.blue800 
                                          else "1," <> Color.grey900
                              Nothing -> "1," <> Color.grey900
              in
              linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , cornerRadius 8.0
              , stroke stroke'
              , gravity CENTER_VERTICAL
              , margin $ MarginVertical 8 8
              , onClick push $ const $ ChooseVehicleCategory index item
              ][  imageView
                  [ width $ V 116
                  , height $ V 80
                  , imageWithFallback $ fetchImage FF_ASSET 
                      case item of
                        ST.AutoCategory -> cityConfig.assets.onboarding_auto_image
                        ST.CarCategory -> "ny_ic_sedan_side"
                        ST.BikeCategory -> "ny_ic_bike_side"
                        ST.AmbulanceCategory -> "ny_ic_ambulance_side"
                        ST.TruckCategory -> "ny_ic_truck_side"
                        ST.BusCategory -> "ny_ic_bus_side"
                        ST.UnKnown -> "ny_ic_silhouette"
              ]
            , textView $
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text case item of
                        ST.AutoCategory -> getString AUTO_RICKSHAW
                        ST.CarCategory -> getString CAR
                        ST.BikeCategory -> getString BIKE_TAXI
                        ST.AmbulanceCategory -> getString AMBULANCE
                        ST.TruckCategory -> getString TRUCK
                        ST.BusCategory ->  getString BUS__
                        ST.UnKnown -> "Unknown"
              , color Color.black800
              , margin $ MarginLeft 20
              ] <> FontStyle.subHeading1 TypoGraphy
          ]
      ) (state.data.variantList))

getStatus :: ST.RegisterationStep -> ST.RegistrationScreenState -> ST.StageStatus
getStatus step state = 
  case step of
    ST.GRANT_PERMISSION -> state.data.permissionsStatus
    -- ST.SUBSCRIPTION_PLAN -> state.data.subscriptionStatus  //don't check from frontend
    _ -> do
          let documentStatusArr = state.data.documentStatusList
              vehicleDoc = [ ST.VEHICLE_PERMIT, ST.FITNESS_CERTIFICATE, ST.VEHICLE_INSURANCE, ST.VEHICLE_PUC, ST.VEHICLE_DETAILS_OPTION]
              findStatus = if step `elem` vehicleDoc 
                          then find (\docStatus -> docStatus.docType == step && filterCondition docStatus) documentStatusArr
                          else find (\docStatus -> docStatus.docType == step) documentStatusArr
          case step of 
            ST.INSPECTION_HUB -> do
               let inspectionHubCreated = (getValueToLocalStore DRIVER_OPERATION_CREATE_REQUEST_SUCCESS)
               if (inspectionHubCreated == "COMPLETED") then ST.COMPLETED else if (inspectionHubCreated == "FAILED") then ST.FAILED else ST.NOT_STARTED
            ST.VEHICLE_PHOTOS -> if state.props.vehicleImagesUploaded then ST.COMPLETED else ST.NOT_STARTED
            _ -> do
                  case findStatus of
                    Nothing -> ST.NOT_STARTED
                    Just docStatus -> docStatus.status
  where filterCondition item = (state.data.vehicleCategory == item.verifiedVehicleCategory) ||  (isNothing item.verifiedVehicleCategory && item.vehicleType == state.data.vehicleCategory)

dependentDocAvailable :: ST.StepProgress -> ST.RegistrationScreenState -> Boolean
dependentDocAvailable item state = 
  case item.stage of
    ST.AADHAAR_CARD -> (getStatus ST.PROFILE_PHOTO state) == ST.COMPLETED
    ST.PAN_CARD -> (getStatus ST.PROFILE_PHOTO state) == ST.COMPLETED
    _ -> all (\docType -> statusCompOrManual (getStatus docType state)) item.dependencyDocumentType

compVisibility :: ST.RegistrationScreenState -> ST.StepProgress -> Boolean
compVisibility state item = not item.isHidden && dependentDocAvailable item state && (if item.stage == ST.PAN_CARD || item.stage == ST.AADHAAR_CARD || item.stage == ST.PROFILE_PHOTO then state.data.config.showProfileAadhaarPan else true)

statusCompOrManual :: ST.StageStatus -> Boolean
statusCompOrManual status = any (_ == status) [ST.COMPLETED, ST.MANUAL_VERIFICATION_REQUIRED]

runLogTracking :: forall action. (action -> Effect Unit) ->  action -> Flow GlobalState Unit
runLogTracking push action = do
  let eventsConfig = RC.eventsConfig "events_config"
  if (getValueToLocalStore LOGS_TRACKING == "true" && eventsConfig.enabled) then do
    void $ delay $ Milliseconds eventsConfig.loggingIntervalInMs
    let _ = printLog "Logging Onboarding Events" ""
    void $ EHE.pushEvent eventsConfig.pushEventChunkSize
    runLogTracking push action
  else 
    pure unit

filterCategories :: Array ST.StepProgress -> Array ST.CategoryToStepMap -> Array ST.DocumentStatus -> Boolean -> Array ST.CategoryToStepMap -- refactor for vehicle photos (have added a prop have to utilize that)
filterCategories registrationSteps categoryToStepProgressMap documentList vehicleImagesUploaded = do
  map (\item -> do
      let stepsForCategory = (filter (\item' -> item'.documentCategory == Just item.category ) registrationSteps)
          x = getCompletionStatus stepsForCategory documentList
          operationHubCreated = (getValueToLocalNativeStore DRIVER_OPERATION_CREATE_REQUEST_SUCCESS)
          allVehicleImagesUploaded = vehicleImagesUploaded
          stepStatus = if item.category == API.VEHICLE then {completionStatus: getUpdatedCompletionStatus operationHubCreated x.completionStatus allVehicleImagesUploaded, showContinueButton: x.showContinueButton && (operationHubCreated == "COMPLETED") && allVehicleImagesUploaded} else x
      {
        category : item.category,
        registrationSteps : stepsForCategory,
        completionStatus : stepStatus.completionStatus,
        showContinueButton : stepStatus.showContinueButton
      }
  ) categoryToStepProgressMap
  where 
    getUpdatedCompletionStatus operationHubCreated completionStatus allVehicleImagesUploaded = 
      case completionStatus of 
        _ | DA.elem completionStatus [ST.COMPLETED, ST.MANUAL_VERIFICATION_REQUIRED] -> if (operationHubCreated == "COMPLETED") && allVehicleImagesUploaded then completionStatus else if (operationHubCreated == "FAILED") then ST.FAILED else ST.NOT_STARTED
        _ -> completionStatus

getCompletionStatus :: Array ST.StepProgress -> Array ST.DocumentStatus -> {completionStatus :: ST.StageStatus, showContinueButton :: Boolean}
getCompletionStatus stepsForCategory documentList =
  let stagesToSearch' = map (\item -> item.stage) stepsForCategory
      stagesToSearch = filter (\item -> item /= ST.VEHICLE_PHOTOS && item /= ST.INSPECTION_HUB) stagesToSearch' -- remove this line after backend sends status correctly for VEHICLE_PHOTOS
      -- _ = spy "printing stagesToSearch -> " stagesToSearch
      filteredDocumentList = filter (\item -> item.docType `elem` stagesToSearch ) documentList
      -- _ = spy "printing filteredDocumentList -> " filteredDocumentList
      completedDocs = filter(\item -> item.status == ST.COMPLETED) filteredDocumentList
      -- _ = spy "printing completedDocs -> " completedDocs
      failedDocs = filter(\item -> item.status == ST.FAILED) filteredDocumentList
      -- _ = spy "printing failedDocs -> " failedDocs
      notStartedDocs = filter(\item -> item.status == ST.NOT_STARTED) filteredDocumentList
      -- _ = spy "printing notStartedDocs -> " notStartedDocs
      inProgressDocs = filter(\item -> item.status == ST.IN_PROGRESS) filteredDocumentList
      -- _ = spy "printing inProgressDocs -> " inProgressDocs
      manualVerificationRequiredDocs = filter(\item -> item.status == ST.MANUAL_VERIFICATION_REQUIRED) filteredDocumentList
      -- _ = spy "printing manualVerificationRequiredDocs -> " manualVerificationRequiredDocs
      
      completionStatus = 
        if (DA.null stagesToSearch) then ST.NOT_STARTED  -- replace stagesToSearch by stepsForCategory after backend handles VEHICLE_PHOTOS  
          else if not $ DA.null notStartedDocs then ST.NOT_STARTED
          else if not $ DA.null failedDocs then ST.FAILED
          else if not $ DA.null inProgressDocs then ST.IN_PROGRESS
          else if not $ DA.null manualVerificationRequiredDocs then ST.MANUAL_VERIFICATION_REQUIRED
          else if not $ DA.null completedDocs then ST.COMPLETED
          else ST.NOT_STARTED
      showContinueButton = (not DA.null stagesToSearch) && (DA.length stagesToSearch == (DA.length completedDocs + DA.length manualVerificationRequiredDocs))
  in 
  {completionStatus : completionStatus, showContinueButton : showContinueButton}

getCategoryCompletedStatusCount :: Array ST.CategoryToStepMap -> ST.RegistrationScreenState -> Int
getCategoryCompletedStatusCount categoryStepMap state =
  let completedStatusArr = map (\item -> do
                  case item.category of
                    API.PERMISSION -> state.data.permissionsStatus == ST.COMPLETED
                    API.TRAINING -> state.data.trainingsCompletionStatus == ST.COMPLETED
                    _ -> statusCompOrManual item.completionStatus -- == ST.COMPLETED
              ) categoryStepMap
  in 
  DA.length $ filter(\x -> x == true) completedStatusArr