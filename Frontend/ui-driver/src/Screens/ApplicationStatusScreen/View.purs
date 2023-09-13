{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ApplicationStatusScreen.View where

import Animation as Anim
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), const, (<>), (/=), (==), (<<<), (||), (&&), discard, bind, pure, unit, not, void,(>=),(+),show)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, imageUrl, imageView, layoutGravity, linearLayout, margin, orientation, padding, text, textSize, textView, weight, width, onClick, visibility, afterRender, lineHeight, stroke, cornerRadius, alignParentRight, onBackPressed, imageWithFallback,relativeLayout,clickable,alpha, lottieAnimationView)
import Screens.ApplicationStatusScreen.Controller (Action(..), ScreenOutput, eval,isClickable,completePercentage)
import Screens.Types as ST
import Services.APITypes (DriverRegistrationStatusResp(..), DriverRegistrationStatusReq(..))
import Services.Backend (driverRegistrationStatusBT)
import Styles.Colors as Color
import Common.Types.App
import Types.App (defaultGlobalState)
import Components.PrimaryButton as PrimaryButton
import Components.PopUpModal as PopUpModal
import Components.ReferralMobileNumber as ReferralMobileNumber
import PrestoDOM.Types.DomAttributes (Corners(..))
import Data.Maybe
import Data.Array (mapWithIndex,length) as DA
import Data.String (length)
import PrestoDOM.Animation as PrestoAnim
import Components.StepsHeaderModel as StepsHeaderModel
import Screens.ApplicationStatusScreen.ComponentConfig

screen :: ST.ApplicationStatusScreenState -> String -> Screen Action ST.ApplicationStatusScreenState ScreenOutput
screen initialState screenType =
  { initialState
  , view : view screenType
  , name : "ApplicationStatusScreen"
  , globalEvents : [
      ( \push -> do
        if screenType == "StatusScreen" then do
          void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
            if(initialState.props.enterMobileNumberView || initialState.props.enterOtp) then pure unit
              else do
              (DriverRegistrationStatusResp driverRegistrationStatusResp ) <- driverRegistrationStatusBT (DriverRegistrationStatusReq { })
              lift $ lift $ doAff do liftEffect $ push $ DriverRegistrationStatusAction (DriverRegistrationStatusResp driverRegistrationStatusResp)
          else pure unit
        if initialState.props.isAlternateMobileNumberExists then do
          EHC.setText' (EHC.getNewIDWithTag "Referalnumber") initialState.data.mobileNumber
          else pure unit
        pure $ pure unit
      )
  ]
  , eval
  }

view :: forall w . String -> (Action -> Effect Unit) -> ST.ApplicationStatusScreenState -> PrestoDOM (Effect Unit) w
view screenType push state =
  Anim.screenAnimation $
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ]([  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , afterRender push (const AfterRender)
    , onBackPressed push (const BackPressed)
    ]([ PrestoAnim.animationSet
          [ Anim.fadeIn true
          ] $ StepsHeaderModel.view (push <<< StepsHeaderModelAC) (stepsHeaderModelConfig state) 
    , if screenType == "ApprovedScreen" then applicationApprovedView state push else applicationStatusView state push
  --  , if (state.props.isVerificationFailed) || (not state.props.onBoardingFailure && state.props.alternateNumberAdded) then textView[] else completeOnboardingView state push
    , uploadStatus state push
  --  , completeLottieStatusView state push 
    ]
    )
  ] <> if state.props.popupview then [popupmodal push state] else []
    <> if state.props.enterMobileNumberView then [alternateNumber push state] else []
    <> if state.props.logoutModalView then [logoutPopupModal push state] else []
    )


----------------------------------------- applicationStatusView -----------------------
applicationStatusView :: ST.ApplicationStatusScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
applicationStatusView state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  ][  
      textView
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , padding (PaddingHorizontal 16 16)
      , visibility GONE
      , text if state.data.dlVerificationStatus == "PENDING" || state.data.rcVerificationStatus == "PENDING" then (getString YOUR_APPLICATION_HAS_BEEN_SUBMITTED_SUCCESSFULLY_AND_IS_UNDER_VERIFICATION) else if (state.data.dlVerificationStatus == "FAILED" || state.data.rcVerificationStatus == "FAILED") then (getString OOPS_YOUR_APPLICATION_HAS_BEEN_REJECTED) else ""
      , fontStyle $ FontStyle.medium LanguageStyle
      , textSize FontSize.a_16
      , lineHeight "20"
      , color Color.black800
      ]
    , applicationStatus state push
  ]

applicationStatus :: ST.ApplicationStatusScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
applicationStatus state push =
  linearLayout
  [ orientation VERTICAL
  , width MATCH_PARENT
  , padding (Padding 16 0 16 24)
  , margin (MarginTop 30)
  , height MATCH_PARENT
  ][  linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , margin $ MarginBottom 20
            ][
               textView
              [   width WRAP_CONTENT
               , height WRAP_CONTENT
               , text ("Start earning in 3 simple step")--"Start earning in 3 simple step"
               , textSize FontSize.a_14
               , fontStyle $ FontStyle.medium LanguageStyle
              ]
              ,  textView
              [  width MATCH_PARENT
               , height WRAP_CONTENT
               , gravity RIGHT
               , text (show (completePercentage state) <> " % " <> ("Complete"))
               , textSize FontSize.a_14
               , fontStyle $ FontStyle.medium LanguageStyle
              ]
            ]
          , linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , margin $ MarginBottom 20
            , weight 1.0
            ](DA.mapWithIndex (\index item -> 
              linearLayout
              [ height $ V 5
              , weight 1.0
              , cornerRadius 2.0
              , background  case item of 
                                "DL" -> case state.data.dlVerificationStatus of
                                                 "VALID"   -> Color.green900
                                                 "NO_DOC_AVAILABLE"  -> Color.grey900
                                                 "PENDING" -> Color.yellow900
                                                 _ -> Color.red

                                "RC" ->  case state.data.rcVerificationStatus of
                                                 "VALID"   -> Color.green900
                                                 "NO_DOC_AVAILABLE"  -> Color.grey900
                                                 "PENDING" -> Color.yellow900
                                                 _         -> Color.red

                                "GP" ->   case state.props.isPermissionGranted of
                                                 true  -> Color.green900
                                                 false -> Color.grey900
                                _    ->  Color.grey900 

              , margin $ if ((index + 1) /= DA.length state.data.stepsArray  ) then (MarginRight 15) else (MarginRight 0)
              ][]) (state.data.stepsArray))
  --     , textView
  --     [ text (getString APPLICATION_STATUS)
  --     , textSize FontSize.a_14
  --     , visibility if state.data.dlVerificationStatus == "PENDING" && state.data.rcVerificationStatus == "PENDING" then GONE else VISIBLE
  --     , color Color.black800
  --     , fontStyle $ FontStyle.regular LanguageStyle
  --     ]
    , detailsView state (drivingLicenseCardDetails state) push
    , detailsView state (vehicleCardDetails state) push
    , detailsView state (grantPermissionsStatus state) push
    , linearLayout
      [ orientation HORIZONTAL
      , width    MATCH_PARENT
      , height $ WRAP_CONTENT
      , gravity BOTTOM
      , margin (MarginTop 220) 
      , visibility $ if (state.data.rcVerificationStatus == "VALID" && state.data.dlVerificationStatus == "VALID" && state.props.isPermissionGranted) then VISIBLE else GONE
      ][
        PrimaryButton.view (push <<< PrimaryButtonCompleteRegistrationAC ) (primaryButtonRegistrationConfig state)
      ]
    ]

detailsView :: ST.ApplicationStatusScreenState -> ST.RegCardDetails -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
detailsView state config push =
  linearLayout
  [ orientation VERTICAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity BOTTOM
  , margin (Margin 0 16 0 0)
  , padding (Padding 10 8 16 8)
  , stroke $ "1,"<>config.strokeColor
  , cornerRadius 6.0
  , background $ config.backgroundColor
  , clickable $ (isClickable state config.docType)
  , onClick push (const $ ReTry config.docType)
  ][  linearLayout
      [ orientation HORIZONTAL
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      , height WRAP_CONTENT
      ][
        imageView
              [ imageWithFallback config.titleImage
              , height $ V 60
              , width $ V 60
              , padding (PaddingRight 15)
              ]
         , 
         linearLayout
         [   orientation VERTICAL
           , width WRAP_CONTENT
           , gravity CENTER_VERTICAL
           , height WRAP_CONTENT
         ][
         textView
          [ text config.title
          , textSize FontSize.a_18
          , color Color.black800
          , lineHeight "18"
          ]
          , textView
          [ text ("Retry Upload")--"Retry Upload"--(getString TRY_AGAIN)
            , height WRAP_CONTENT
            , width WRAP_CONTENT
            , textSize FontSize.a_16
            , color Color.blue900
            , fontStyle $ FontStyle.medium LanguageStyle
            , visibility if ((config.status == "FAILED" || config.status == "INVALID" ) && config.docType /= "GP" ) then VISIBLE else GONE -- $ if (config.verificationStatus == "PENDING") then GONE  else VISIBLE
            , lineHeight "18"
      ]
         ]
        -- , textView
        --   [ text $ " (" <> config.verificationStatus <> ")"
        --   , textSize FontSize.a_12
        --   , visibility $ if config.verificationStatus /= "" then VISIBLE else GONE
        --   , color if config.verificationStatus == "FAILED" then Color.red else Color.black800
        --   , lineHeight "18"
        --   ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity RIGHT
          , orientation HORIZONTAL
          ][  imageView
              [ imageWithFallback config.image
              , height $ V 20
              , width $ V 20
              ]
            ]
          ]
    , textView
      [ text config.reason
      , textSize FontSize.a_12
      , color Color.black700
      , margin (MarginTop 8)
      , fontStyle $ FontStyle.regular LanguageStyle
      , visibility $ if config.reason /= "" then VISIBLE else GONE
      , lineHeight "16"
      ]
    ]
-- ----------------------------------------- supportTextView -----------------------
supportTextView state push =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity BOTTOM
  ][ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , orientation HORIZONTAL
    , padding (PaddingBottom 20)
    ][ textView
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text (getString FOR_SUPPORT)
      , textSize FontSize.a_12
      , color Color.black700
      , fontStyle $ FontStyle.regular LanguageStyle
      ]
    , textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text (getString CONTACT_US)
      , color Color.blueTextColor
      , fontStyle $ FontStyle.regular LanguageStyle
      , textSize FontSize.a_12
      , onClick push (const SupportCall)
      ]
    ]
  ]

drivingLicenseCardDetails state =
  {
    "title" : (getString DRIVING_LICENSE),
    "titleImage" : "ny_ic_dl_blue,https://assets.juspay.in/nammayatri/images/driver/ny_ic_dl_blue.png",
    "image" : case state.data.dlVerificationStatus of
                "VALID" -> "ny_ic_check_mark,https://assets.juspay.in/nammayatri/images/driver/ny_ic_check_mark.png"
                "PENDING" -> "ny_ic_pending,https://assets.juspay.in/nammayatri/images/driver/ny_ic_pending.png"
                "FAILED" -> "ny_ic_api_failure_popup,https://assets.juspay.in/nammayatri/images/driver/ny_ic_api_failure_popup.png"
                "NO_DOC_AVAILABLE"  -> "ny_ic_chevron_right,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_right"
                "INVALID" -> "ny_ic_api_failure_popup,https://assets.juspay.in/nammayatri/images/driver/ny_ic_api_failure_popup.png"
                "LIMIT_EXCEED" -> "ny_ic_help_circle,https://assets.juspay.in/nammayatri/images/driver/ny_ic_help_circle.png"
                _ -> "ny_ic_api_failure_popup,https://assets.juspay.in/nammayatri/images/driver/ny_ic_api_failure_popup.png",
    "verificationStatus" :  case state.data.dlVerificationStatus of
                "VALID" -> ""
                "PENDING" -> (getString VERIFICATION_PENDING)
                "FAILED" -> (getString VERIFICATION_FAILED)
                "NO_DOC_AVAILABLE" -> (getString NO_DOC_AVAILABLE)
                "INVALID" -> (getString INVALID_DRIVING_LICENSE)
                "LIMIT_EXCEED" -> (getString LIMIT_EXCEEDED_FOR_DL_UPLOAD)
                _ -> (getString VERIFICATION_FAILED),
    "backgroundColor" : case state.data.dlVerificationStatus of
                       "VALID"   -> Color.green200
                       "PENDING" -> Color.yellow200
                       "NO_DOC_AVAILABLE"  -> Color.white900
                       _         -> Color.lightRed,
    "strokeColor" :  case state.data.dlVerificationStatus of
                     "VALID"   -> Color.green900
                     "PENDING" -> Color.yellow900 
                     "NO_DOC_AVAILABLE"  -> Color.grey900
                     _         -> Color.red,
    "reason" : "",
    "visibility" : if state.data.dlVerificationStatus == "PENDING"  && state.data.rcVerificationStatus == "PENDING" then "GONE" else "VISIBLE",
    "docType" : "DL",
    "status" : state.data.dlVerificationStatus
  }

vehicleCardDetails state=
  {
    "title" : "Vehicle Registration Certificate",--(--getString VEHICLE_DETAILS),
    "titleImage" : "ny_ic_vehicle_onboard,https://assets.juspay.in/nammayatri/images/driver/ny_ic_auto_onboard.png",
    "image" : case state.data.rcVerificationStatus of
                "VALID" -> "ny_ic_check_mark,https://assets.juspay.in/nammayatri/images/driver/ny_ic_check_mark.png"
                "PENDING" -> "ny_ic_pending,https://assets.juspay.in/nammayatri/images/driver/ny_ic_pending.png"
                "FAILED" -> "ny_ic_api_failure_popup,https://assets.juspay.in/nammayatri/images/driver/ny_ic_api_failure_popup.png"
                "NO_DOC_AVAILABLE" -> "ny_ic_chevron_right,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_right"
                "INVALID" -> "ny_ic_api_failure_popup,https://assets.juspay.in/nammayatri/images/driver/ny_ic_api_failure_popup.png"
                "LIMIT_EXCEED" -> "ny_ic_help_circle,https://assets.juspay.in/nammayatri/images/driver/ny_ic_help_circle.png"
                _ -> "ny_ic_api_failure_popup,https://assets.juspay.in/nammayatri/images/driver/ny_ic_api_failure_popup.png",
    "verificationStatus" :  case state.data.rcVerificationStatus of
                "VALID" -> ""
                "PENDING" -> (getString VERIFICATION_PENDING)
                "FAILED" -> (getString VERIFICATION_FAILED)
                "NO_DOC_AVAILABLE" -> (getString NO_DOC_AVAILABLE)
                "INVALID" -> (getString INVALID_VEHICLE_REGISTRATION_CERTIFICATE)
                "LIMIT_EXCEED" -> (getString LIMIT_EXCEEDED_FOR_RC_UPLOAD)
                _ -> (getString VERIFICATION_FAILED),
    "backgroundColor" : case state.data.rcVerificationStatus of
                       "VALID"   -> Color.green900
                       "PENDING" -> Color.yellow200
                       "NO_DOC_AVAILABLE"  -> Color.white900
                       _         -> Color.lightRed,
    "strokeColor" :  case state.data.rcVerificationStatus of
                     "VALID"   -> Color.green900
                     "PENDING" -> Color.yellow900
                     "NO_DOC_AVAILABLE"  -> Color.grey900
                     _         -> Color.red,
    "reason" : "",
    "visibility" : if state.data.dlVerificationStatus == "PENDING"  && state.data.rcVerificationStatus == "PENDING" then "GONE" else "VISIBLE",
    "docType" : "RC",
    "status" : state.data.rcVerificationStatus
  }


grantPermissionsStatus state=
  {
    "title" : "Grant Permissions",--(getString VEHICLE_DETAILS),
    "titleImage" : "ny_ic_vehicle_onboard,https://assets.juspay.in/nammayatri/images/driver/ny_ic_auto_onboard.png",
    "image" : case state.props.isPermissionGranted of
                true  -> "ny_ic_check_mark,https://assets.juspay.in/nammayatri/images/driver/ny_ic_check_mark.png"
                _     -> "ny_ic_chevron_right,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_right",
               
    "verificationStatus" :  case state.props.isPermissionGranted of
                true  -> ""
                _     -> "",
    "backgroundColor" : case state.props.isPermissionGranted of
                        true   -> Color.green200
                        _      -> Color.white900,
    "strokeColor" :  case state.props.isPermissionGranted of
                        true   -> Color.green900
                        _      -> Color.grey900,
    "reason" : "",
    "visibility" : if state.data.dlVerificationStatus == "PENDING"  && state.data.rcVerificationStatus == "PENDING" then "GONE" else "VISIBLE",
    "docType" : "GP",
    "status" : state.data.rcVerificationStatus
  }


----------------------------------------- applicationApprovedView -----------------------
applicationApprovedView :: ST.ApplicationStatusScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
applicationApprovedView state push =
 linearLayout
  [ width MATCH_PARENT
  , weight 1.0
  ][ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER
      ][ imageView
          [ width (V 340)
          , height (V 150)
          , layoutGravity "center_horizontal"
          , imageWithFallback "ny_ic_coming_soon,https://assets.juspay.in/nammayatri/images/driver/ny_ic_coming_soon.png"
          ]
          , textView (
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , gravity CENTER
          , padding (Padding 15 10 15 10)
          , text (getString THANK_YOU_FOR_REGISTERING_US)
          , color Color.black
          ] <> FontStyle.h1 TypoGraphy
          )
          , textView (
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER
          , padding (PaddingHorizontal 15 15)
          , text (getString YOUR_DOCUMENTS_ARE_APPROVED)
          ] <> FontStyle.paragraphText TypoGraphy
          )
      ]
  ]

completeOnboardingView:: ST.ApplicationStatusScreenState  -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
completeOnboardingView state push =
 linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER
      , background  Color.blue600
      , cornerRadius  4.0
      , margin (MarginHorizontal 16 16)
      , padding (Padding 16 16 16 16)
      ][ textView
         [ width WRAP_CONTENT
         , height WRAP_CONTENT
         , text   if state.props.onBoardingFailure then (getString VERIFICATION_IS_TAKING_A_BIT_LONGER) else (getString ADD_ALTERNATE_NUMBER_IN_MEANTIME)
         , color Color.black700
         , textSize FontSize.a_16
         , margin (Margin 8 0 8 16)
         , gravity CENTER
        ]
        , PrimaryButton.view (push <<< CompleteOnBoardingAction)  (primaryButtonConfig state)
      ]

popupmodal :: forall w . (Action -> Effect Unit) -> ST.ApplicationStatusScreenState -> PrestoDOM (Effect Unit) w
popupmodal push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.blackLessTrans
  ][PopUpModal.view (push <<< PopUpModalAction) (completeOnboardingConfig state )]


logoutPopupModal :: forall w . (Action -> Effect Unit) -> ST.ApplicationStatusScreenState -> PrestoDOM (Effect Unit) w
logoutPopupModal push state =
       linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , background Color.blackLessTrans
        , visibility if state.props.logoutModalView == true then VISIBLE else GONE
        ][ PopUpModal.view (push <<<PopUpModalLogoutAction) (logoutPopUp state) ]

alternateNumber :: forall w . (Action -> Effect Unit) -> ST.ApplicationStatusScreenState -> PrestoDOM (Effect Unit) w
alternateNumber push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.blackLessTrans
  ][ReferralMobileNumber.view (push <<< AlternateMobileNumberAction) (alternateMobileNumberConfig state )]

-- ----------------------------------------- uploadFailedTextView -----------------------
uploadStatus state push =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity BOTTOM
  , visibility if (state.props.isVerificationFailed || state.data.dlVerificationStatus == "PENDING" || state.data.rcVerificationStatus == "PENDING") then VISIBLE else GONE 
  ][ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    --, gravity CENTER
    , orientation HORIZONTAL
    , stroke $ "1,"<>Color.grey900
    , margin (Margin 15 0 15 30)
    , cornerRadius 8.0
    , padding (Padding 15 10 15 10 )
    ][ textView
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text if state.props.isVerificationFailed then (if state.data.dlVerificationStatus == "FAILED" then "DL" else "RC") <> " " <> ("Upload Failed.") else ("Last updated at")  -- Upload Failed.  -- Last updated at
      , textSize FontSize.a_14
      , color Color.black700
      , fontStyle $ FontStyle.regular LanguageStyle
      ]
    , textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text if state.props.isVerificationFailed then ("Please retry and upload again.") else ("02:52")         ---"Please retry and upload again."
      , color Color.black900
      , margin (MarginLeft 5)
      , fontStyle $ FontStyle.bold LanguageStyle
      , textSize FontSize.a_14
      ]

   ,linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity RIGHT
  , onClick push (const RefreshScreen)
  , visibility if (state.data.rcVerificationStatus == "PENDING" || state.data.dlVerificationStatus == "PENDING") then VISIBLE else GONE
  ][
     imageView
           [
             imageWithFallback "ny_ic_refresh,https://assets.juspay.in/nammayatri/images/driver/ny_ic_refresh.png"
            , height $ V 20
            , width $ V 20
           ]
    , textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , margin (MarginLeft 5)
      , text ("Refrsh")--getString REFRESH)
      , color Color.blue900
      , fontStyle $ FontStyle.bold LanguageStyle
      , textSize FontSize.a_14
      ] 
   ]
    ]
  ]


-- completeLottieStatusView :: forall w. ST.ApplicationStatusScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
-- completeLottieStatusView state push =
--   linearLayout
--   [ width (V 25)
--   , height (V 25)
--   , visibility if state.props.lottieStatus then VISIBLE else GONE
--   ][ lottieAnimationView
--     [ height (V 25)
--     , width (V 25)
--    -- , id (getIdForScreenIndex activeIndex)
--     , afterRender
--         ( \action -> do
--             _ <- pure $ startLottieProcess "notification_bell" (getIdForScreenIndex activeIndex) true 1.0 "default"
--             pure unit
--         )
--         (const AfterRender)
--     ]
    
--   ]