{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AppUpdatePopUp.View where

import Animation as Anim
import Animation.Config (translateYAnimConfigUpdate)
import Prelude (Unit, bind, const, pure, unit, ($), (<>), (==),(/=),(<<<))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, ScopedScreen, Visibility(..), onBackPressed, alpha, background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, width, afterRender)
import Screens.Types (AppUpdatePopUpState)
import Components.PopUpModal as PopUpModal
import Screens.Types (UpdatePopupType(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import JBridge as JB 
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties as PP 
import Engineering.Helpers.Commons as EHC 
import Screens.AppUpdatePopUp.Controller (Action(..), eval, ScreenOutput)
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Common.Types.App
import MerchantConfig.Utils(Merchant(..), getMerchant, getValueFromConfig)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))


screen :: AppUpdatePopUpState -> ScopedScreen Action AppUpdatePopUpState ScreenOutput
screen initialState =
  { initialState
  , view
  , parent : Just "AppUpdatePopUpScreen"
  , name : "PopUpScreen"
  , globalEvents : []
  , eval
  }
  
view :: forall w .  (Action  -> Effect Unit) -> AppUpdatePopUpState -> PrestoDOM (Effect Unit) w
view push state =
   linearLayout
  [ height MATCH_PARENT
    , width MATCH_PARENT
    , clickable true
  ]
  [ if (state.updatePopup == AppVersion) then  updateRequiredView push state
    else if (state.updatePopup == AppUpdated) then appUpdatedView push state
    else linearLayout[][] 
  ]
   
updateRequiredView :: forall w .  (Action  -> Effect Unit) -> AppUpdatePopUpState -> PrestoDOM (Effect Unit) w
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
      Anim.translateYAnim $ translateYAnimConfigUpdate
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
        , PP.visibility  VISIBLE 
        ] 
        [ imageView
          [ imageUrl "ic_app_update"
          , height $ V 28
          , width $ V 28
          , PP.visibility GONE
          ]
        , textView $ 
          [ text (getString UPDATE_REQUIRED)
          , color Color.textPrimary
          , margin (Margin 10 0 0 0)
          ] <> FontStyle.h0 LanguageStyle
        ]
      , textView $ [
          width MATCH_PARENT
          ,height WRAP_CONTENT
          ,color "#474955"
          ,margin (MarginLeft 10)
          ,text (getString PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE)
          ,padding (PaddingBottom 30)
        ] <> FontStyle.body1 LanguageStyle
        , linearLayout 
          [ width MATCH_PARENT 
          , height WRAP_CONTENT
          , gravity RIGHT
          , orientation HORIZONTAL
          ][ linearLayout 
              [ width WRAP_CONTENT  
              , height WRAP_CONTENT
              ][
                  linearLayout
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , padding (Padding 20 11 20 11)
                , stroke ("1," <> Color.textSecondary)
                , cornerRadius 10.0
                , alpha 0.6
                , PP.visibility GONE
                ][
                    textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text (getString NOT_NOW)
                    , color Color.textSecondary
                    , alpha 0.6
                    , clickable true
                    , onClick (\action -> do
                                _<- push action
                                pure unit
                                ) (const OnCloseClick)
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
                              _ <- JB.openUrlInApp (getAppLink (getMerchant FunctionCall))
                              pure unit
                              ) (const OnAccept)
              ] <> FontStyle.body4 LanguageStyle
            ]
          ]
        ]
      ]
  ]


appUpdatedView :: forall w. (Action  -> Effect Unit) -> AppUpdatePopUpState -> PrestoDOM (Effect Unit) w
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
      Anim.translateYAnim $ translateYAnimConfigUpdate
    ] $ PopUpModal.view (push <<< AppUpdatedModelAction) (appUpdatedModelConfig state) 
  ]

appUpdatedModelConfig :: AppUpdatePopUpState -> PopUpModal.Config
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
  , primaryButtonLayout {
    visibility = VISIBLE
  , button1 {
    visibility = GONE
  }
  , button2{
    textConfig{
      text =  state.appUpdatedView.optionTwoText
    }
  }
  }
  , coverImageConfig {
      imageUrl = fetchImage FF_ASSET state.appUpdatedView.coverImageUrl
    , visibility = if state.appUpdatedView.coverImageUrl /= "" then VISIBLE else GONE
    , height = V 178
    , width = V 204
    }
  }
  in popUpConfig'

getAppLink :: Merchant -> String 
getAppLink merchant = 
  case merchant of 
    YATRI      -> case EHC.os of 
                    "IOS" -> "https://apps.apple.com/in/app/yatri/id1615871038"
                    _     -> "https://play.google.com/store/apps/details?id=net.openkochi.yatri"
    NAMMAYATRI -> case EHC.os of 
                    "IOS" -> "https://apps.apple.com/in/app/namma-yatri/id1637429831"
                    _ -> "https://play.google.com/store/apps/details?id=in.juspay.nammayatri"
    _          -> ""