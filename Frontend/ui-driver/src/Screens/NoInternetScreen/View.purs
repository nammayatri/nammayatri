{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NoInternetScreen.View where

import Prelude (Unit, bind, const, pure, unit, (<<<), ($), (==), (<>))
import Effect (Effect)
import PrestoDOM (Length(..), Margin(..), Gravity(..), Padding(..), Orientation(..), Visibility(..), PrestoDOM, ScopedScreen, linearLayout, clickable, height, width, gravity, background, padding, orientation, imageView, textView, text, imageUrl, textSize, fontStyle, color, margin, lineHeight, relativeLayout, alignParentBottom, onClick, visibility, afterRender, imageWithFallback)
import Screens.NoInternetScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Components.PrimaryButton as PrimaryButton
import JBridge as JB
import Font.Style as FontStyle
import Font.Size as FontSize
import Data.Maybe
import Styles.Colors as Color
import Language.Strings (getString)
import Language.Types(STR(..))
import Common.Types.App
import Screens.NoInternetScreen.ComponentConfig
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))

screen :: ST.NoInternetScreenState -> String -> ScopedScreen Action ST.NoInternetScreenState ScreenOutput
screen initialState triggertype = 
  { initialState
  , view : view triggertype
  , name : "NoInternetScreen"
  , globalEvents : [(\ push -> do
    _ <- JB.storeCallBackDriverLocationPermission push LocationPermissionCallBack
    _ <- JB.storeCallBackInternetAction push InternetActionCallBack
    pure $ pure unit)]
  , eval
  , parent : Nothing -- Just "NoInternetScreen"
  }

view :: forall w . String -> (Action -> Effect Unit) -> ST.NoInternetScreenState -> PrestoDOM (Effect Unit) w 
view triggertype push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , gravity CENTER
  , afterRender push (const AfterRender)
  ][ if triggertype == "INTERNET_ACTION" then noInternetScreenView push state triggertype 
      else if triggertype == "LOCATION_DISABLED" then locationAccessPermissionView push state triggertype 
        else  textView[] 
    ]
  

locationAccessPermissionView :: forall w. (Action -> Effect Unit) -> ST.NoInternetScreenState -> String -> PrestoDOM (Effect Unit) w 
locationAccessPermissionView push state triggertype = 
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , padding (Padding 16 16 0 0)
      ][  textView $
          [ text (getString WE_NEED_ACCESS_TO_YOUR_LOCATION)
          , color Color.black800
          , gravity LEFT
          , margin (MarginVertical 22 16)
          ] <> FontStyle.h1 LanguageStyle
        , textView $
          [ text $ getString $ YOUR_LOCATION_HELPS_OUR_SYSTEM "YOUR_LOCATION_HELPS_OUR_SYSTEM"
          , color Color.black800
          ] <> FontStyle.body5 TypoGraphy
        ]
        , linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity CENTER
        ][  imageView
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_location_access"
            , height $ V 213
            , width $ V 240
            , gravity CENTER
            ]
          ]
        , buttonView push state triggertype
  ]

noInternetScreenView :: forall w. (Action -> Effect Unit) -> ST.NoInternetScreenState -> String -> PrestoDOM (Effect Unit) w 
noInternetScreenView push state triggertype = 
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , gravity CENTER
      , orientation VERTICAL
      , clickable false
      ][imageView
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_offline"
        , height $ V 213
        , width $ V 240
        , gravity CENTER
        ],
        textView $
        [ text (getString NO_INTERNET_CONNECTION)
        , color Color.black800
        , gravity LEFT
        ] <> FontStyle.h1 LanguageStyle,
        textView $
        [ text (getString PLEASE_CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN)
        , color Color.black800
        , margin (MarginTop 10)
        ] <> FontStyle.body5 TypoGraphy
      ]
      , buttonView push state triggertype
  ]

buttonView :: forall w. (Action -> Effect Unit) -> ST.NoInternetScreenState -> String -> PrestoDOM (Effect Unit) w 
buttonView push state  triggertype = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , alignParentBottom "true,-1"
  ][  PrimaryButton.view (push <<< (PrimaryButtonActionController triggertype)) (primaryButtonConfig triggertype)]
