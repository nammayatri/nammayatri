{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RegistrationScreen.View where

import Prelude (Unit, bind, const, map, pure, unit, ($), (<>), (<<<))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textSize, textView, visibility, weight, width, imageWithFallback)
import Animation as Anim
import Effect (Effect)
import Components.PrimaryButton as PrimaryButton
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.RegistrationScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.RegistrationScreen.ScreenData (optionList, ListOptions(..))
import Screens.Types as ST
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Common.Types.App
import Screens.RegistrationScreen.ComponentConfig
import Helpers.Utils (fetchImage, FetchImageFrom(..), consumeBP)
import Effect.Uncurried (runEffectFn1)

screen :: ST.RegistrationScreenState -> Screen Action ST.RegistrationScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "RegistrationScreen"
  , globalEvents : [(\_ -> pure $ runEffectFn1 consumeBP unit)]
  , eval
  }

view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.RegistrationScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  linearLayout
  [  height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , clickable true
    , onBackPressed push (const BackPressed)
    , afterRender  (\action -> do
                      _<- push action
                      pure unit
                      ) $ const (AfterRender)
  ][ 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (Padding 16 16 16 0)
    , margin (MarginTop 20)
    , visibility VISIBLE
    , weight 1.0
    ][ imageView
        [ width ( V 20 )
        , height ( V 20)
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_back"
        , visibility GONE
        ]
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ][ registrationHeader state
          -- , tutorialView state
          , cardItemView state
        ]
    ]
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER
    ][PrimaryButton.view (push <<< PrimaryButtonAction) (primaryButtonConfig state)]
  ]

registrationHeader :: ST.RegistrationScreenState -> forall w . PrestoDOM (Effect Unit) w
registrationHeader state = 
  linearLayout
  [ orientation VERTICAL
  , width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][textView 
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , fontStyle $ FontStyle.bold LanguageStyle
    , color Color.black800
    , textSize FontSize.a_26
    , margin (MarginVertical 15 10)
    , text (getString REGISTRATION)
    ]
  , textView $
    [ text (getString FOLLOW_STEPS)
    , margin (MarginBottom 20)
    ] <> FontStyle.body5 TypoGraphy
  ]


tutorialView :: ST.RegistrationScreenState -> forall w . PrestoDOM (Effect Unit) w
tutorialView state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , cornerRadius 7.0
  , background Color.lightBlue
  , gravity LEFT
  , padding (Padding 10 10 10 10)
  ][ textView
      ([ width WRAP_CONTENT
      , height MATCH_PARENT
      , text (getString WATCH_A_TUTORIAL_FOR_EASY_REGISTRATION)
      , weight 1.0
      , gravity CENTER_VERTICAL
      , color Color.black800
      ] <> FontStyle.body1 TypoGraphy)
    , imageView
      [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_media"
      , width (V 40)
      , height (V 40)
      ]
  ]


cardItemView :: ST.RegistrationScreenState -> forall w . PrestoDOM (Effect Unit) w
cardItemView state = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , margin (MarginTop 15)
  ](map
      (\item -> 
          linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , padding (Padding 0 20 15 15)
          , cornerRadius 3.0
          ][ imageView
              [ imageWithFallback $ fetchImage FF_ASSET $ case item of
                  DRIVING_LICENSE_OPTION -> "ny_ic_dl_blue"
                  VEHICLE_DETAILS_OPTION -> "ny_ic_vehicle_onboard"
              , width (V 50)
              , height (V 50)
              , margin (MarginRight 14)
              ]
          ,   linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              , padding (PaddingRight 10)
              ][ textView
                  ([ text case item of
                            DRIVING_LICENSE_OPTION -> (getString DRIVING_LICENSE)
                            VEHICLE_DETAILS_OPTION -> (getString VEHICLE_DETAILS)
                  , color Color.black800
                  ] <> FontStyle.subHeading1 TypoGraphy)
                  , textView
                  ([ text case item of
                            DRIVING_LICENSE_OPTION -> (getString UPLOAD_FRONT_SIDE)
                            VEHICLE_DETAILS_OPTION -> (getString FILL_VEHICLE_DETAILS)
                  , color Color.black700
                  ] <> FontStyle.body1 TypoGraphy)
              ]
          ]
      ) optionList
  )