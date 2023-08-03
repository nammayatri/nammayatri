{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.MultipleRCUploadScreen.View where

import Common.Types.App
import Data.Maybe
import Screens.MultipleRCUploadScreen.ComponentConfig

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.ReferralMobileNumber as ReferralMobileNumber
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.String (length)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((<>))
import Prelude (Unit, ($), const, (<>), (/=), (==), (<<<), (||), (&&), discard, bind, pure, unit, not, void)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alignParentBottom, alignParentRight, background, color, cornerRadius, editText, fontStyle, gradient, gravity, height, hint, hintColor, id, imageUrl, imageView, imageWithFallback, layoutGravity, lineHeight, linearLayout, margin, onBackPressed, onChange, onClick, orientation, padding, pattern, relativeLayout, scrollView, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM (Margin(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.MultipleRCUploadScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Services.API (DriverRegistrationStatusResp(..), DriverRegistrationStatusReq(..))
import Services.Backend (driverRegistrationStatusBT)
import Styles.Colors as Color
import Types.App (defaultGlobalState)

screen :: ST.MultipleRCUploadScreenState -> String -> Screen Action ST.MultipleRCUploadScreenState ScreenOutput
screen initialState screenType =
  { initialState
  , view : view screenType
  , name : "MultipleRCUploadScreen"
  , globalEvents : [
      ( \push -> do
        if screenType == "PENDING" then do
          void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
            if false then pure unit
              else do
              (DriverRegistrationStatusResp driverRegistrationStatusResp ) <- driverRegistrationStatusBT (DriverRegistrationStatusReq { })
              lift $ lift $ doAff do liftEffect $ push $ DriverRegistrationStatusAction (DriverRegistrationStatusResp driverRegistrationStatusResp)
         else pure unit
        pure $ pure unit
      )
  ]
  , eval
  }

view :: forall w . String -> (Action -> Effect Unit) -> ST.MultipleRCUploadScreenState -> PrestoDOM (Effect Unit) w
view screenType push state =
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][ if screenType == "PENDING" then rcVerificationStatusView push state "ny_ic_document_pending" "Your RC has been <b>submitted successfully</b> and is under verification" 
     else if screenType == "FAILED" then rcVerificationStatusView push state "ny_ic_document_failed" "<b>RC verification failed!</b> <br> Please try later" 
     else if screenType == "SUCCESS" then rcVerificationSuccessView push state 
     else linearLayout[][]
   ]

rcHeaderView :: forall w . (Action -> Effect Unit) -> ST.MultipleRCUploadScreenState -> String -> PrestoDOM (Effect Unit) w
rcHeaderView push state title = 
  linearLayout
  [ height $ V 56
  , width MATCH_PARENT
  , background Color.black900
  , gravity CENTER_VERTICAL
  , padding $ PaddingHorizontal 16 16
  ][ imageView
     [ width (V 24)
     , height (V 24)
     , imageWithFallback $ "ny_ic_chevron_left_white," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_left_white.png"
     , onClick push (const BackPressed)
     , margin $ MarginRight 16
     ]
   , textView
     [ text title
     , textSize FontSize.a_14
     , color Color.white900
     , fontStyle $ FontStyle.medium LanguageStyle
     ]
   ]

enterRCNumberView :: forall w . (Action -> Effect Unit) -> ST.MultipleRCUploadScreenState -> PrestoDOM (Effect Unit) w
enterRCNumberView push state =
  relativeLayout
  [ height MATCH_PARENT
  , width WRAP_CONTENT
  , orientation VERTICAL
  ][ rcHeaderView push state "Enter RC Details"
   , scrollView
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , margin $ MarginTop 56
     ][ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding $ PaddingHorizontal 16 16
        ][ textView
           [ text "Vehicle Registration Details"
           , textSize FontSize.a_22
           , color Color.black800
           , fontStyle $ FontStyle.bold LanguageStyle
           , margin $ MarginVertical 32 24
           ]
         , textFieldView push state "Vehicle Registration Number" OnRCNumberChange "KA01 20230099999" (getNewIDWithTag "RCNumberEditText")
         , linearLayout[margin $ MarginBottom 32][]
         , textFieldView push state "Re-enter Vehicle Registration Number" OnReEnterRCNumberChange "KA01 20230099999" (getNewIDWithTag "ReEnterRCNumberEditText")
         ]
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , alignParentBottom "true,-1"
      ][ linearLayout
         [ height $ V 1
         , width MATCH_PARENT
         , background Color.grey900
         ][]
       , PrimaryButton.view (push <<< GoToProfile) (primaryButtonConfig state "Proceed" state.props.isButtonEnabled)
       ]
   ]

rcImageUploadView :: forall w . (Action -> Effect Unit) -> ST.MultipleRCUploadScreenState -> PrestoDOM (Effect Unit) w
rcImageUploadView push state =
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][ rcHeaderView push state "Upload RC"
   , scrollView
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , margin $ MarginTop 56
     ][ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding $ PaddingHorizontal 16 16
        ][ textView
           [ text "Upload Registration Certificate"
           , textSize FontSize.a_22
           , color Color.black800
           , fontStyle $ FontStyle.bold LanguageStyle
           , margin $ MarginVertical 32 24
           ]
         , textView
           [ text "How to upload"
           , textSize FontSize.a_16
           , color Color.black800
           , fontStyle $ FontStyle.semiBold LanguageStyle
           , margin $ MarginBottom 12
           ]
         , textView
           [ textFromHtml "Take a clear picture of your Registration Certificate on a flat surface. <br><br> Ensure that the lighting is adequate and all the details are clearly visible <br><br> Fit the Registration Certificate in the marked area correctly as shown below"
           , textSize FontSize.a_12
           , color Color.black800
           , fontStyle $ FontStyle.regular LanguageStyle
           , margin $ MarginBottom 24
           , lineHeight "16"
           ]
         , linearLayout
           [ height WRAP_CONTENT
           , width MATCH_PARENT
           , cornerRadius 4.0
           , stroke $ "1," <> Color.grey900
           , padding $ Padding 20 20 20 20
           , orientation VERTICAL
           ][ rcImageInstructionView state "ny_ic_clear_rc" "ny_ic_check_round_green" "Clear Image" "Cropped Correctly"
            , linearLayout[margin $ MarginBottom 16][]
            , rcImageInstructionView state "ny_ic_blurry_rc" "ny_ic_cross_round_red" "Blurry Image" "Wrong Cropping"
            ]
         ]
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , alignParentBottom "true,-1"
      ][ linearLayout
         [ height $ V 1
         , width MATCH_PARENT
         , background Color.grey900
         ][]
       , PrimaryButton.view (push <<< GoToProfile) (primaryButtonConfig state "Take Photo" true)
       ]
    ]

rcImageInstructionView :: forall w . ST.MultipleRCUploadScreenState -> String -> String -> String -> String -> PrestoDOM (Effect Unit) w
rcImageInstructionView state imageURL primaryiconURL primaryText secondaryText =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  ][ imageView
      [ width (V 120)
      , height (V 76)
      , imageWithFallback $ imageURL <> "," <> (getCommonAssetStoreLink FunctionCall) <> imageURL <> ".png"
      , margin $ MarginRight 16
      ]
    , linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER_VERTICAL
      ][ linearLayout
         [ height WRAP_CONTENT
         , width WRAP_CONTENT
         , orientation VERTICAL
         , margin $ MarginBottom 4
         ][ linearLayout
           [ height WRAP_CONTENT
           , width WRAP_CONTENT
           , gravity CENTER_VERTICAL
           ][ imageView
               [ width (V 16)
               , height (V 16)
               , imageWithFallback $ primaryiconURL <> "," <> (getCommonAssetStoreLink FunctionCall) <> primaryiconURL <>".png"
               , margin $ MarginRight 8
               ]
             , textView
               [ text primaryText
               , textSize FontSize.a_16
               , color Color.black800
               , fontStyle $ FontStyle.bold LanguageStyle
               , padding $ PaddingBottom 4
               ]
             ]
         ]
      , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER_VERTICAL
        ][ imageView
          [ width (V 16)
          , height (V 16)
          , imageWithFallback $ primaryiconURL <> "," <> (getCommonAssetStoreLink FunctionCall) <> primaryiconURL <>".png"
          , margin $ MarginRight 8
          ]
        , textView
          [ text secondaryText
          , textSize FontSize.a_16
          , color Color.black800
          , fontStyle $ FontStyle.bold LanguageStyle
          , padding $ PaddingBottom 4
          ]
        ]
      ]
    ]

rcVerificationStatusView :: forall w . (Action -> Effect Unit) -> ST.MultipleRCUploadScreenState -> String -> String -> PrestoDOM (Effect Unit) w
rcVerificationStatusView push state imageURL description =
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
  ][ linearLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , gravity CENTER
     , orientation VERTICAL
     , margin $ MarginHorizontal 16 16
     ][ imageView
        [ width $ V 136
        , height $ V 123
        , margin $ MarginBottom 32
        , imageWithFallback $ imageURL <> ",https://assets.juspay.in/beckn/nammayatri/images/driver/" <> imageURL <> ".png"
        ]
      , textView
        [ textFromHtml description
        , textSize FontSize.a_16
        , color Color.black800
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , gravity CENTER
        ]
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , alignParentBottom "true,-1"
      ][ linearLayout
         [ height $ V 1
         , width MATCH_PARENT
         , background Color.grey900
         ][]
       , PrimaryButton.view (push <<< GoToProfile) (primaryButtonConfig state "Okay" true)
       , linearLayout
         [ height WRAP_CONTENT
         , width MATCH_PARENT
         , gravity CENTER_HORIZONTAL
         ][ textView
            [ textFromHtml ("For support <font color=" <> Color.blue900 <> ">contact us </font>")
            , textSize FontSize.a_12
            , color Color.black700
            , fontStyle $ FontStyle.regular LanguageStyle
            , margin $ MarginBottom 14
            , onClick push $ const $ NoAction
            ]
          ]
       ]
   ]

rcVerificationSuccessView :: forall w . (Action -> Effect Unit) -> ST.MultipleRCUploadScreenState -> PrestoDOM (Effect Unit) w
rcVerificationSuccessView push state =
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
  ][ linearLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , gravity CENTER
     , orientation VERTICAL
     ][ imageView
        [ width $ V 114
        , height $ V 114
        , margin $ MarginBottom 32
        , imageWithFallback "ny_ic_check_round_green,https://assets.juspay.in/beckn/nammayatri/images/driver/ny_ic_check_round_green.png"
        ]
      , textView
        [ text "Your RC verification is successful!"
        , textSize FontSize.a_18
        , color Color.black800
        , fontStyle $ FontStyle.bold LanguageStyle
        , margin $ MarginBottom 80
        ]
      ] 
   , linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , padding $ Padding 16 24 16 24
     , margin $ Margin 16 0 16 16
     , gravity CENTER_HORIZONTAL
     , orientation VERTICAL
     , stroke $ "1," <> Color.grey900
     , alignParentBottom "true,-1"
     , cornerRadius 8.0
     ][ textView
        [ text "Activate RC"
        , textSize FontSize.a_18
        , color Color.black800
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , margin $ MarginBottom 8
        ]
      , textView
        [ text $ "Do you want to activate RC - " <> "KA01NY2873" <> "?  This will deactivate the currently active RC."
        , textSize FontSize.a_14
        , color Color.black700
        , fontStyle $ FontStyle.regular LanguageStyle
        , margin $ MarginBottom 20
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ][ PrimaryButton.view (push <<< GoToProfile) (skipButtonConfig state)
         , PrimaryButton.view (push <<< GoToProfile) (activateButtonConfig state)
         ]
      ]
   ]

textFieldView :: forall w . (Action -> Effect Unit) -> ST.MultipleRCUploadScreenState -> String -> (String -> Action)-> String -> String -> PrestoDOM (Effect Unit) w
textFieldView push state title action hintText tag = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][ textView
     [ text title
     , textSize FontSize.a_12
     , color Color.black800
     , fontStyle $ FontStyle.regular LanguageStyle
     , margin $ MarginBottom 8
     ]
   , editText
     [ height $ V 54
     , width MATCH_PARENT
     , id tag
     , padding $ PaddingLeft 20
     , cornerRadius 8.0
     , hint $ hintText
     , hintColor Color.grey900

     , onChange push $ action
     , stroke $ "1," <> Color.grey900
     , pattern "[0-9A-Z]*,11"
     ]
   ]