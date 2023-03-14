module Screens.PermissionScreen.View where

import Data.Maybe
import Components.ErrorModal as ErrorModal
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Engineering.Helpers.Commons as EHC 
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, pure, unit, (<<<), ($), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, ScopedScreen, afterRender, alignParentBottom, background, clickable, color, fontStyle, gravity, height, imageUrl, imageView, lineHeight, linearLayout, margin, orientation, padding, relativeLayout, text, textSize, textView, width, imageWithFallback)
import Screens.PermissionScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App
import Screens.OnBoardingFlow.PermissionScreen.ComponentConfig 

screen :: ST.PermissionScreenState -> String -> ScopedScreen Action ST.PermissionScreenState ScreenOutput
screen initialState triggertype = 
  { initialState
  , view : view triggertype
  , name : "PermissionScreen"
  , globalEvents : [(\ push -> do
    _ <- JB.storeCallBackDriverLocationPermission push LocationPermissionCallBackCustomer
    _ <- JB.storeCallBackInternetAction push InternetCallBackCustomer
    pure $ pure unit
  )]
  , eval
  , parent : Just "PermissionScreen"
  }

view :: forall w . String -> (Action -> Effect Unit) -> ST.PermissionScreenState -> PrestoDOM (Effect Unit) w 
view triggertype push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , clickable true
  ][ linearLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , background Color.white900
     , padding $ Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom
     , gravity CENTER
     , afterRender push (const AfterRender)
     ][ if triggertype == "INTERNET_ACTION" then ErrorModal.view (push <<< ErrorModalActionController) (errorModalConfig) else if triggertype == "LOCATION_DISABLED" then locationAccessPermissionView push state else  textView[]]  
   ]
  
locationAccessPermissionView :: forall w. (Action -> Effect Unit) -> ST.PermissionScreenState -> PrestoDOM (Effect Unit) w 
locationAccessPermissionView push state = 
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding (Padding 16 16 16 (if EHC.safeMarginBottom == 0 then 24 else 0))
  ][  linearLayout[
    height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][  
    -- linearLayout
        -- [ height $ V 25
        -- , width MATCH_PARENT
        -- , gravity RIGHT  
        -- , onClick push $ const BackPressed
        -- ][imageView
        --   [ height $ V 25
        --   , width $ V 25
        --   , imageUrl "ic_close"
        --   ]
        -- ]
      textView 
      [ text (getString WE_NEED_ACCESS_TO_YOUR_LOCATION)
      , textSize FontSize.a_22
      , color Color.black800
      , gravity LEFT
      , lineHeight "27"
      , margin (Margin 0 22 0 16)
      , fontStyle $ FontStyle.bold LanguageStyle
      ]
    , textView
      [ text (getString YOUR_LOCATION_HELPS_OUR_SYSTEM)
      , textSize FontSize.a_16
      , color Color.black800
      , fontStyle $ FontStyle.regular LanguageStyle
      , lineHeight "22"
      ]
    ]
    , linearLayout[
      height MATCH_PARENT
    , width MATCH_PARENT
    , gravity CENTER
    ][imageView
      [ imageWithFallback "ny_ic_location_access,https://assets.juspay.in/nammayatri/images/common/ny_ic_location_access.png"
      , height $ V 213
      , width $ V 240
      , gravity CENTER
      ]]
    , buttonView push state 
    
  ]

buttonView :: forall w. (Action -> Effect Unit) -> ST.PermissionScreenState -> PrestoDOM (Effect Unit) w 
buttonView push state  = 
  linearLayout
  [ orientation VERTICAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , alignParentBottom "true,-1"
  ][  PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig)
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
