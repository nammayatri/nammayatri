{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PermissionScreen.View where

import Common.Types.App (LazyCheck(..))
import Components.ErrorModal as ErrorModal
import Components.PrimaryButton as PrimaryButton
import Components.PopUpModal as PopUpModal
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, pure, unit, (<<<), ($), (==), (<>), (/=),(&&))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), PrestoDOM, Screen, afterRender, alignParentBottom, background, clickable, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, lineHeight, linearLayout, margin, orientation, padding, text, textSize, textView, width,visibility, id, accessibilityHint, accessibility)
import Screens.OnBoardingFlow.PermissionScreen.ComponentConfig (errorModalConfig, primaryButtonConfig, getLocationBlockerPopUpConfig)
import Screens.PermissionScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color

screen :: ST.PermissionScreenState -> Screen Action ST.PermissionScreenState ScreenOutput
screen initialState  = 
  { initialState
  , view : view 
  , name : "PermissionScreen"
  , globalEvents : [(\ push -> do
    if EHC.os == "IOS" && (JB.getLocationPermissionStatus unit) == "DENIED" then do 
      _ <- push (LocationPermissionCallBackCustomer false)
      pure $ pure unit
    else do 
      _ <- JB.storeCallBackDriverLocationPermission push LocationPermissionCallBackCustomer
      _ <- JB.storeCallBackInternetAction push InternetCallBackCustomer
      pure $ pure unit
  )]
  , eval
  }

view :: forall w . (Action -> Effect Unit) -> ST.PermissionScreenState -> PrestoDOM (Effect Unit) w 
view push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , clickable true
  , visibility if (EHC.os == "IOS" && state.stage == ST.LOCATION_DISABLED) then GONE else VISIBLE
  ][ linearLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , padding $ Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom
     , gravity CENTER
     , afterRender push (const AfterRender)
     ]([] <> (case state.stage of
                ST.INTERNET_ACTION -> [ErrorModal.view (push <<< ErrorModalActionController) (errorModalConfig state)]
                ST.LOCATION_DENIED -> [iosEnableLocationView push state]
                ST.LOCATION_DISABLED -> [locationAccessPermissionView push state]
                _ ->  [textView[]]))
   ]
  
locationAccessPermissionView :: forall w. (Action -> Effect Unit) -> ST.PermissionScreenState -> PrestoDOM (Effect Unit) w 
locationAccessPermissionView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity CENTER
  , padding (Padding 16 16 16 (if EHC.safeMarginBottom == 0 then 24 else 0))
  , background Color.blackLessTrans
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER
      , padding $ Padding 20 20 20 20
      , margin $ MarginHorizontal 20 20
      , cornerRadius 8.0
      , background Color.white900
      ][  imageView
          [ imageWithFallback $ fetchImage FF_ASSET "ic_location_permission_logo"
          , height $ V 213
          , width $ V 240
          , gravity CENTER
          ]
        , textView 
          [ text $ "Hey " <> (getValueToLocalStore USER_NAME) <> "!"
          , textSize FontSize.a_22
          , color Color.black800
          , gravity CENTER
          , lineHeight "27"
          , margin $ Margin 0 22 0 16
          , fontStyle $ FontStyle.bold LanguageStyle
        ]
        , textView
          [ text $ getString if (getValueToLocalStore PERMISSION_POPUP_TIRGGERED) /= "true" then LOCATION_PERMISSION_SUBTITLE_NEW_USER else LOCATION_PERMISSION_SUBTITLE
          , textSize FontSize.a_16
          , color Color.black800
          , fontStyle $ FontStyle.regular LanguageStyle
          , lineHeight "22"
          , gravity CENTER
          , margin $ MarginBottom 15
          ]
        , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)
      ]

      
  ]

buttonView :: forall w. (Action -> Effect Unit) -> ST.PermissionScreenState -> PrestoDOM (Effect Unit) w 
buttonView push state  = 
  linearLayout
  [ orientation VERTICAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , alignParentBottom "true,-1"
  ][  PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)
  -- ,  textView $
  --     [ text (getString DENY_ACCESS)
  --     , width MATCH_PARENT
  --     , height WRAP_CONTENT 
  --     , color Color.black800
  --     , margin (Margin 0 20 0 0)
  --     , onClick push (const $ BackPressed)
  --     , gravity CENTER
  --     ] <> FontStyle.subHeading1 TypoGraphy
  ]


iosEnableLocationView :: forall w. (Action -> Effect Unit) -> ST.PermissionScreenState -> PrestoDOM (Effect Unit) w  
iosEnableLocationView push state = 
  linearLayout
  [
    height MATCH_PARENT,
    width MATCH_PARENT
  ][PopUpModal.view (push <<< LocationBlockerPopUpAC) (getLocationBlockerPopUpConfig state)]