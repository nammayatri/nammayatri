{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AppUpdatePopUpScreen.View where

import Animation as Anim
import Animation.Config as AnimConfig
import Prelude (Unit, bind, const, pure, unit, ($), (<>), (<<<), (==),(/=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), onBackPressed, Orientation(..), Padding(..), PrestoDOM, ScopedScreen, Visibility(..), alpha, background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, width, visibility, afterRender, imageWithFallback)
import Screens.Types as ST
import Screens.Types (UpdatePopupType(..))
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons (convertUTCtoISC, getCurrentUTC)
import Effect (Effect)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM.Animation as PrestoAnim
import Screens.AppUpdatePopUpScreen.Controller (Action(..), eval, ScreenOutput)
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Common.Types.App
import Components.PrimaryButton.View as PrimaryButton
import Components.PrimaryButton.Controller as PrimaryButtonConfig
import Language.Types(STR(..))
import JBridge(dateCallback)
import Data.Function.Uncurried(runFn2)
import Debug
import MerchantConfig.Utils (getValueFromConfig)
import Components.PopUpModal as PopUpModal
import PrestoDOM.Types.DomAttributes (Corners(..))
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))


screen :: ST.AppUpdatePopUpScreenState -> ScopedScreen Action ST.AppUpdatePopUpScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , parent : Nothing -- Just "AppUpdatePopUpScreen"
  , name : "AppUpdatePopUpScreen"
  , eval : (\state  action -> do
      let _ = spy "AppUpdatePopUpScreen state -----" state
      let _ = spy "AppUpdatePopUpScreen--------action" action
      eval state action)
  , globalEvents : [(\push -> do
      _ <- pure $ runFn2 JB.dateCallback push OnResumeCallBack
      pure $ pure $ runFn2 JB.storeOnResumeCallback push OnResumeCallBack
    )]
  }

view :: forall w . (Action  -> Effect Unit) -> ST.AppUpdatePopUpScreenState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
  [ height MATCH_PARENT
    , width MATCH_PARENT
    , onBackPressed push (const $ BackPressed)
    , clickable true
  ]
  [ if (state.updatePopup == AppVersion) then  updateRequiredView push state
    else if (state.updatePopup == AppUpdated) then appUpdatedView push state
    else inaccurateDateAndTimeView push state 
  ] 

inaccurateDateAndTimeView :: forall w. (Action  -> Effect Unit) -> ST.AppUpdatePopUpScreenState -> PrestoDOM (Effect Unit) w
inaccurateDateAndTimeView push state =
  linearLayout
  [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    , gravity CENTER_HORIZONTAL
    , orientation VERTICAL
  ]
  [ imageView
    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_app_date_and_time"
    , height $ V 250
    , width $ V 250
    , margin $ MarginTop 208
    ]
    , textView
      [ text (getString INACCURATE_DATE_AND_TIME)
      , textSize FontSize.a_18
      , fontStyle $ FontStyle.bold LanguageStyle
      , color Color.textPrimary
      , margin (MarginVertical 16 16)
      ]
    , textView
      [ text (getString ADJUST_YOUR_DEVICE_DATE_AND_TIME_AND_TRY_AGAIN)
      , color Color.black700
      , fontStyle $ FontStyle.regular LanguageStyle
      , textSize FontSize.a_14
      , gravity CENTER
      ]
    , linearLayout [
      width MATCH_PARENT
      , height MATCH_PARENT
      , orientation VERTICAL
      , gravity BOTTOM
      ]
      [ textView
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER 
        , text (getString THE_CURRENT_DATE_AND_TIME_IS <>  ((convertUTCtoISC (getCurrentUTC "") "   DD/MM/YYYY   HH:mm:ss")))
        , color Color.black700
        , fontStyle $ FontStyle.regular LanguageStyle
        , textSize FontSize.a_14
        ]
        , PrimaryButton.view (push <<< PrimaryButtonActionController ) (primaryButtonConfig (getString GO_TO_SETTING))
      ]

  ]

primaryButtonConfig :: String -> PrimaryButtonConfig.Config
primaryButtonConfig val = let
    config = PrimaryButtonConfig.config
    primaryButtonConfig' = config
      { textConfig
      { text = val
      , color = Color.primaryButtonColor}
      , background = Color.black900
      , height = (V 50)
      , margin = (Margin 16 16 16 16)
      , cornerRadius = 8.0
      , isClickable = true
      , id = "AppUpdatePopUpScreenPrimaryButton"
      }
  in primaryButtonConfig'

updateRequiredView :: forall w. (Action  -> Effect Unit) -> ST.AppUpdatePopUpScreenState -> PrestoDOM (Effect Unit) w
updateRequiredView push state =
  linearLayout [
    width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    , background "#99000000"
    , gravity CENTER_VERTICAL
    , afterRender push (const AfterRender)
  ][
   PrestoAnim.animationSet [
      Anim.translateYAnim $ AnimConfig.translateYAnimConfigUpdatePopUp
    ] $ 
      linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , cornerRadius 4.0
      , margin (MarginHorizontal 20 20)
      , orientation VERTICAL
      , background Color.greyBG
      , padding (Padding 20 20 20 20)
      , clickable true
      ][ linearLayout 
        [ height WRAP_CONTENT 
        , width MATCH_PARENT 
        , orientation HORIZONTAL 
        ,padding (PaddingBottom 20) 
        , visibility  VISIBLE 
        ] 
        [ imageView
          [ imageUrl "ic_app_update"
          , height $ V 28
          , width $ V 28
          -- , visibility GONE
          ]
        , textView
          [ text (getString UPDATE_REQUIRED)
          , textSize FontSize.a_24
          , fontStyle $ FontStyle.bold LanguageStyle
          , color Color.textPrimary
          , margin (Margin 10 0 0 0)
          ]
        ]
      , textView $ [
          width MATCH_PARENT
          ,height WRAP_CONTENT
          ,fontStyle $ FontStyle.medium LanguageStyle
          ,color "#474955"
          ,margin (MarginLeft 10)
          ,text (getString PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE)
          ,padding (PaddingBottom 30)
        ] <> FontStyle.paragraphText LanguageStyle
        , linearLayout 
          [ width MATCH_PARENT 
          , height WRAP_CONTENT
          , gravity RIGHT
          , orientation HORIZONTAL
          ][ linearLayout 
              [ width WRAP_CONTENT  
              , height WRAP_CONTENT
              ][ linearLayout
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , padding (Padding 20 11 20 11)
                , stroke ("1," <> Color.textSecondary)
                , cornerRadius 10.0
                , alpha 0.6
                -- , visibility GONE
                ][
                    textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text (getString NOT_NOW)
                    , color Color.textSecondary
                    , alpha 0.6
                    , clickable true
                    , onClick push $ const OnCloseClick
                    ] <> FontStyle.body4 LanguageStyle
                ]
            , linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , padding (Padding 28 11 28 11)
              , margin (MarginLeft 12)
              , background Color.charcoalGrey
              , cornerRadius 10.0
              ][ textView $
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text (getString UPDATE)
                  , color Color.yellowText
                  , onClick (\action -> do
                              _<- push action
                              _ <- JB.openUrlInApp $ getValueFromConfig "APP_LINK"
                              pure unit
                              ) (const OnAccept)
              ] <> FontStyle.body4 LanguageStyle
            ]
          ]
        ]
      ]
  ]

appUpdatedView :: forall w. (Action  -> Effect Unit) -> ST.AppUpdatePopUpScreenState -> PrestoDOM (Effect Unit) w
appUpdatedView push state = 
  linearLayout [
    width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    , background "#99000000"
    , gravity CENTER_VERTICAL
    , afterRender push (const AfterRender)
  ][
   PrestoAnim.animationSet [
      Anim.translateYAnim $ AnimConfig.translateYAnimConfigUpdatePopUp
    ] $ PopUpModal.view (push <<< AppUpdatedModelAction) (appUpdatedModelConfig state) 
  ]

appUpdatedModelConfig :: ST.AppUpdatePopUpScreenState -> PopUpModal.Config
appUpdatedModelConfig state =
  let
    config' = PopUpModal.config
    popUpConfig' = config'{
    gravity = CENTER
  , cornerRadius = (Corners 15.0 true true true true)
  , margin = (Margin 16 0 16 0)
  , padding = (Padding 0 5 0 0)
  , topTitle {visibility = GONE}
  , primaryText {
      text = state.appUpdatedView.primaryText
    }
  , secondaryText {
      text =state.appUpdatedView.secondaryText
    }
    , option1 {
     visibility = false
    }
  , option2 {
      text =  state.appUpdatedView.optionTwoText,
      padding = (Padding 16 0 16 0),
      margin = (Margin 12 0 12 16)
    }
  , coverImageConfig {
      imageUrl = fetchImage FF_ASSET $ state.appUpdatedView.coverImageUrl
    , visibility = if state.appUpdatedView.coverImageUrl /= "" then VISIBLE else GONE
    , height = V 178
    , width = V 204
    }
  }
  in popUpConfig'