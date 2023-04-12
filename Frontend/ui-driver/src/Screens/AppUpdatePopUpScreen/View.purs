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
import Prelude (Unit, bind, const, pure, unit, ($), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, ScopedScreen, Visibility(..), alpha, background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, width, visibility, afterRender)
import Screens.Types as ST
import Data.Maybe (Maybe(..))
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
import Constant.Test as Id
import EN

screen :: ST.AppUpdatePopUpScreenState -> ScopedScreen Action ST.AppUpdatePopUpScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , parent : Just "AppUpdatePopUpScreen"
  , name : "PopUpScreen"
  , globalEvents : []
  , eval
  }
view :: forall w .  (Action  -> Effect Unit) -> ST.AppUpdatePopUpScreenState -> PrestoDOM (Effect Unit) w
view push state =
   linearLayout [
    width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    , background "#99000000"
    , gravity CENTER_VERTICAL
    , afterRender push (const AfterRender)
    , Id.testId $ Id.Screen Id.appUpdatePopUpScreen
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
          , visibility GONE
          ]
        , textView
          [ text (getString UPDATE_REQUIRED)
          , textSize FontSize.a_24
          , fontStyle $ FontStyle.bold LanguageStyle
          , color Color.textPrimary
          , margin (Margin 10 0 0 0)
          ]
        ]
      , textView [
          width MATCH_PARENT
          ,height WRAP_CONTENT
          ,fontStyle $ FontStyle.medium LanguageStyle
          ,color "#474955"
          ,textSize FontSize.a_15
          ,margin (MarginLeft 10)
          ,text (getString PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE)
          ,padding (PaddingBottom 30)
        ]
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
                , visibility GONE
                ][
                    textView
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text (getString NOT_NOW)
                    , color Color.textSecondary
                    , alpha 0.6
                    , fontStyle $ FontStyle.bold LanguageStyle
                    , textSize FontSize.a_14
                    , clickable true
                    , onClick (\action -> do
                                _<- push action
                                pure unit
                                ) (const OnCloseClick)
                    , Id.testId $ Id.Option (getEN NOT_NOW)
                    ]
                ]
            , linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , padding (Padding 28 11 28 11)
              , margin (MarginLeft 12)
              , background Color.charcoalGrey
              , cornerRadius 10.0
              ][ textView 
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text (getString UPDATE)
                  , color Color.yellowText
                  , fontStyle $ FontStyle.bold LanguageStyle
                  , textSize FontSize.a_14
                  , onClick (\action -> do
                              _<- push action
                              _ <- JB.openUrlInApp "https://play.google.com/store/apps/details?id=in.juspay.nammayatripartner"
                              pure unit
                              ) (const OnAccept)
                  , Id.testId $ Id.Option (getEN UPDATE)
              ] 
            ]
          ]
        ]
      ]
  ]
