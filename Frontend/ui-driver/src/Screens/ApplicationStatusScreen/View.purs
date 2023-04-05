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
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), const, (<>), (/=), (==), (||), (&&), discard, bind, pure, unit)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, imageUrl, imageView, layoutGravity, linearLayout, margin, orientation, padding, text, textSize, textView, weight, width, onClick, visibility, afterRender, lineHeight, stroke, cornerRadius, alignParentRight, onBackPressed, imageWithFallback)
import Screens.ApplicationStatusScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Services.APITypes (DriverRegistrationStatusResp(..), DriverRegistrationStatusReq(..))
import Services.Backend (driverRegistrationStatusBT)
import Styles.Colors as Color
import Common.Types.App

screen :: ST.ApplicationStatusScreenState -> String -> Screen Action ST.ApplicationStatusScreenState ScreenOutput
screen initialState screenType =
  { initialState
  , view : view screenType
  , name : "ApplicationStatusScreen"
  , globalEvents : [
      ( \push -> do
        if screenType == "StatusScreen" then do
          launchAff_ $ EHC.flowRunner $ runExceptT $ runBackT $ do
            (DriverRegistrationStatusResp driverRegistrationStatusResp ) <- driverRegistrationStatusBT (DriverRegistrationStatusReq { })
            lift $ lift $ doAff do liftEffect $ push $ DriverRegistrationStatusAction (DriverRegistrationStatusResp driverRegistrationStatusResp)
          else pure unit
        pure $ pure unit
      )
  ]
  , eval
  }

view :: forall w . String -> (Action -> Effect Unit) -> ST.ApplicationStatusScreenState -> PrestoDOM (Effect Unit) w
view screenType push state =
  Anim.screenAnimation $ 
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , afterRender push (const AfterRender)
    , onBackPressed push (const BackPressed)
    ][ textView (
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity RIGHT
      , margin (Margin 0 30 20 0)
      , text (getString LOGOUT)
      , onClick push (const Logout)
      , alignParentRight "true,-1"
      , color Color.blueBtn
      ] <> FontStyle.body1 TypoGraphy
      )
    , if screenType == "ApprovedScreen" then applicationApprovedView state push else applicationStatusView state push
    , supportTextView state push
    ]

-- ------------------------------------------ primaryButtonView ---------------------------
primaryButtonView :: ST.ApplicationStatusScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
primaryButtonView state push =
 linearLayout
  [ height $ V 52
  , width MATCH_PARENT
  , background Color.black900
  , gravity CENTER
  , margin (Margin 16 0 20 16)
  , visibility if state.data.dlVerificationStatus == "FAILED" || state.data.rcVerificationStatus == "FAILED" then VISIBLE else GONE
  , cornerRadius 8.0
  , onClick push (const PrimaryButtonActionController)
  ][ textView
      [ text (getString TRY_AGAIN)
      , textSize FontSize.a_16
      , color Color.yellowText
      , fontStyle $ FontStyle.bold LanguageStyle
      ]
  ]

----------------------------------------- applicationStatusView -----------------------
applicationStatusView :: ST.ApplicationStatusScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
applicationStatusView state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , weight 1.0
  ][  imageView
      [ width (V 136)
      , height (V 123)
      , layoutGravity "center_horizontal"
      , imageWithFallback "ny_ic_coming_soon,https://assets.juspay.in/nammayatri/images/driver/ny_ic_coming_soon.png"
      , margin (MarginBottom 32)
      ]
    , textView 
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , padding (PaddingHorizontal 16 16)
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
  , margin (MarginTop 50)
  , height WRAP_CONTENT
  ][  textView  
      [ text (getString APPLICATION_STATUS)
      , textSize FontSize.a_14
      , visibility if state.data.dlVerificationStatus == "PENDING" && state.data.rcVerificationStatus == "PENDING" then GONE else VISIBLE
      , color Color.black800
      , fontStyle $ FontStyle.regular LanguageStyle
      ]
    , detailsView state (drivingLicenseCardDetails state) push
    , detailsView state (vehicleCardDetails state) push
    ]

detailsView :: ST.ApplicationStatusScreenState -> ST.RegCardDetails -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w 
detailsView state config push = 
  linearLayout
  [ orientation VERTICAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity BOTTOM
  , margin (Margin 0 16 0 0)
  , padding (Padding 16 16 16 16)
  , stroke $ "1,"<>Color.grey900
  , cornerRadius 4.0
  ][  linearLayout
      [ orientation HORIZONTAL
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      , height WRAP_CONTENT
      ][ textView
          [ text config.title
          , textSize FontSize.a_14
          , color Color.black800
          , lineHeight "18"
          ]
        , textView
          [ text $ " (" <> config.verificationStatus <> ")"
          , textSize FontSize.a_12
          , visibility $ if config.verificationStatus /= "" then VISIBLE else GONE
          , color if config.verificationStatus == "FAILED" then Color.red else Color.black800
          , lineHeight "18"
          ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity RIGHT
          , orientation HORIZONTAL
          ][  imageView
              [ imageWithFallback config.image 
              , height $ V 16 
              , width $ V 16 
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
    , textView 
      [ text (getString TRY_AGAIN)
      , textSize FontSize.a_14
      , color Color.blue900
      , margin (MarginTop 8)
      , fontStyle $ FontStyle.medium LanguageStyle
      , onClick push (const $ ReTry config.docType)
      , visibility if (config.status == "PENDING" || config.status == "VALID" || config.status == "INVALID") then GONE else VISIBLE -- $ if (config.verificationStatus == "PENDING") then GONE  else VISIBLE
      , lineHeight "18"
      ]
    ]
-- ----------------------------------------- supportTextView -----------------------
supportTextView state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
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

drivingLicenseCardDetails state = 
  {
    "title" : (getString DRIVING_LICENSE),
    "image" : case state.data.dlVerificationStatus of 
                "VALID" -> "ny_ic_check_mark,https://assets.juspay.in/nammayatri/images/driver/ny_ic_check_mark.png"
                "PENDING" -> "ny_ic_pending,https://assets.juspay.in/nammayatri/images/driver/ny_ic_pending.png"
                "FAILED" -> "ny_ic_api_failure_popup,https://assets.juspay.in/nammayatri/images/driver/ny_ic_api_failure_popup.png"
                "NO_DOC_AVAILABLE"  -> "ny_ic_help_circle,https://assets.juspay.in/nammayatri/images/driver/ny_ic_help_circle.png"
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
    "reason" : "",
    "visibility" : if state.data.dlVerificationStatus == "PENDING"  && state.data.rcVerificationStatus == "PENDING" then "GONE" else "VISIBLE",
    "docType" : "DL",
    "status" : state.data.dlVerificationStatus
  }

vehicleCardDetails state= 
  {
    "title" : (getString VEHICLE_DETAILS),
    "image" : case state.data.rcVerificationStatus of 
                "VALID" -> "ny_ic_check_mark,https://assets.juspay.in/nammayatri/images/driver/ny_ic_check_mark.png"
                "PENDING" -> "ny_ic_pending,https://assets.juspay.in/nammayatri/images/driver/ny_ic_pending.png"
                "FAILED" -> "ny_ic_api_failure_popup,https://assets.juspay.in/nammayatri/images/driver/ny_ic_api_failure_popup.png"
                "NO_DOC_AVAILABLE" -> "ny_ic_help_circle,https://assets.juspay.in/nammayatri/images/driver/ny_ic_help_circle.png"
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
    "reason" : "",
    "visibility" : if state.data.dlVerificationStatus == "PENDING"  && state.data.rcVerificationStatus == "PENDING" then "GONE" else "VISIBLE",
    "docType" : "RC",
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