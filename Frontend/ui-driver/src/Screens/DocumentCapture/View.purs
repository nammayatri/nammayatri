{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Screens.DocumentCaptureScreen.View where

import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, LoggableScreen, background, gravity, height, linearLayout, margin, onBackPressed, orientation, padding, weight, width,  textView, text, color, textSize, fontStyle, visibility, cornerRadius, stroke, imageView, imageWithFallback, frameLayout, scrollView)
import Screens.Types as ST
import Styles.Colors as Color
import Effect (Effect)
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Prelude (Unit, const, ($), (<<<), (<>), bind, discard, unit, pure, map, (==), (/=))
import Screens.DocumentCaptureScreen.Controller (Action(..), eval, ScreenOutput(..))
import Screens.DocumentCaptureScreen.ComponentConfig
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Font.Style as FontStyle
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import JBridge as JB
import PaymentPage (consumeBP)
import Effect.Uncurried (runEffectFn1)
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Components.ValidateDocumentModal as ValidateDocumentModal
import Components.PopUpModal as PopUpModal
import Screens.RegistrationScreen.ComponentConfig (logoutPopUp)
import Common.Types.App as CTA
import Resource.Constants as Constant
import Common.Animation.Config as AnimConfig
import Components.OptionsMenu as OptionsMenu
import Debug (spy)
import Screens.RegistrationScreen.ComponentConfig (changeVehicleConfig)
import Data.Array as DA
import Components.BottomDrawerList as BottomDrawerList
import Engineering.Helpers.Events as EHE
import Helpers.Utils as HU
import Data.Maybe (Maybe(..))

screen :: ST.DocumentCaptureScreenState -> LoggableScreen Action ST.DocumentCaptureScreenState ScreenOutput
screen initialState = 
  { initialState
  , view
  , name : "DocumentCaptureScreen"
  , globalEvents : [(\push -> do
    _ <- JB.storeCallBackImageUpload push CallBackImageUpload
    _ <- runEffectFn1 consumeBP unit
    let _ = EHE.addEvent (EHE.defaultEventObject $ HU.getRegisterationStepScreenLoadedEventName initialState.data.docType) { module = HU.getRegisterationStepModule initialState.data.docType, source = HU.getRegisterationStepScreenSource initialState.data.docType }
    pure $ pure unit
  )]
  , eval :
      ( \state action -> do
          let _ = spy "DocumentCaptureScreen ----- state" state
          let _ = spy "DocumentCaptureScreen --------action" action
          eval state action
        ) 
  , parent : Nothing
  , logWhitelist : initialState.data.config.logWhitelistConfig.documentCaptureScreenLogWhitelist
  }


view :: forall w. (Action -> Effect Unit) -> ST.DocumentCaptureScreenState -> PrestoDOM (Effect Unit) w 
view push state = 
  Anim.screenAnimation $
  frameLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ] $ [ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      , onBackPressed push $ const BackPressed 
      ][ AppOnboardingNavBar.view (push <<< AppOnboardingNavBarAC) (appOnboardingNavBarConfig state)
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
                  , margin $ Margin 20 20 15 0
                  ][ howToUpload push state ]
                ]
          ]
        , PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)
      ]
    , if state.props.contactSupportModal /= ST.HIDE then BottomDrawerList.view (push <<< BottomDrawerListAC) (bottomDrawerListConfig state) else linearLayout[][]
    ] <> if state.props.validateDocModal then [ValidateDocumentModal.view (push <<< ValidateDocumentModalAction) (validateDocModalState state)] else []
      <> if DA.any (_ == true) [state.props.logoutModalView, state.props.confirmChangeVehicle] then [ popupModal push state ] else []
      <> if state.props.menuOptions then [menuOptionModal push state] else []


menuOptionModal :: forall w. (Action -> Effect Unit) -> ST.DocumentCaptureScreenState -> PrestoDOM (Effect Unit) w
menuOptionModal push state = 
  linearLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , padding $ PaddingTop 55
    , background Color.blackLessTrans
    ][ OptionsMenu.view (push <<< OptionsMenuAction) (optionsMenuConfig state) ]

popupModal :: forall w . (Action -> Effect Unit) -> ST.DocumentCaptureScreenState -> PrestoDOM (Effect Unit) w
popupModal push state =
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.blackLessTrans
    ][ PopUpModal.view (push <<< action) popupConfig ] 
    where 
      action = if state.props.logoutModalView then PopUpModalLogoutAction 
                else ChangeVehicleAC
      popupConfig = if state.props.logoutModalView then (logoutPopUp CTA.Language)
                    else changeVehicleConfig FunctionCall

howToUpload :: (Action -> Effect Unit) ->  ST.DocumentCaptureScreenState -> forall w . PrestoDOM (Effect Unit) w
howToUpload push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
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
      [ text $ getVarString CAPTURE_DOC_DESC_1 [Constant.transformDocText state.data.docType]
      , color Color.black800
      , margin $ MarginBottom 18
      ] <> FontStyle.body3 TypoGraphy

      , textView $ 
      [ text $ getString ENSURE_ADEQUATE_LIGHT
      , color Color.black800
      , margin $ MarginBottom 18
      ] <> FontStyle.body3 TypoGraphy

      , textView $ 
      [ text $ getVarString CAPTURE_DOC_DESC_3 [Constant.transformDocText state.data.docType]
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
        ][ rightWrongView true state
         , rightWrongView false state
         ]  
    ]
  ]

rightWrongView :: Boolean -> ST.DocumentCaptureScreenState -> forall w . PrestoDOM (Effect Unit) w
rightWrongView isRight state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER_VERTICAL
  , margin $ MarginBottom 16
  ][ imageView
    [ width $ V 120
    , height $ V if isRight then 80 else 100
    , imageWithFallback $ fetchImage FF_ASSET $ sampleImage isRight state
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

sampleImage :: Boolean -> ST.DocumentCaptureScreenState -> String
sampleImage isRight state = 
  case state.data.docType of
    ST.VEHICLE_PERMIT -> if isRight then "ny_ic_permit_clear" else "ny_ic_permit_blur"
    ST.FITNESS_CERTIFICATE -> if isRight then "ny_ic_fitness_clear" else "ny_ic_fitness_blur"
    ST.VEHICLE_INSURANCE -> if isRight then "ny_ic_insurance_clear" else "ny_ic_insurance_blur"
    ST.VEHICLE_PUC -> if isRight then "ny_ic_puc_clear" else "ny_ic_puc_blur"
    _ -> if isRight then "ny_ic_upload_right" else "ny_ic_image_wrong"

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