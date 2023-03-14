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
  , parent : Just "NoInternetScreen"
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
      ][  textView 
          [ text (getString WE_NEED_ACCESS_TO_YOUR_LOCATION)
          , textSize FontSize.a_22
          , color Color.black800
          , gravity LEFT
          , margin (MarginVertical 22 16)
          , fontStyle $ FontStyle.bold LanguageStyle
          ]
        , textView
          [ text (getString YOUR_LOCATION_HELPS_OUR_SYSTEM)
          , textSize FontSize.a_16
          , color Color.black800
          , fontStyle $ FontStyle.regular LanguageStyle
          ]
        ]
        , linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity CENTER
        ][  imageView
            [ imageWithFallback "ny_ic_location_access,https://assets.juspay.in/nammayatri/images/common/ny_ic_location_access.png"
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
        [ imageWithFallback "ny_ic_offline,https://assets.juspay.in/nammayatri/images/common/ny_ic_offline.png"
        , height $ V 213
        , width $ V 240
        , gravity CENTER
        ],
        textView 
        [ text (getString NO_INTERNET_CONNECTION)
        , textSize FontSize.a_22
        , color Color.black800
        , gravity LEFT
        , fontStyle $ FontStyle.bold LanguageStyle
        ],
        textView
        [ text (getString PLEASE_CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN)
        , textSize FontSize.a_16
        , color Color.black800
        , fontStyle $ FontStyle.regular LanguageStyle
        , margin (MarginTop 10)
        ]
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
