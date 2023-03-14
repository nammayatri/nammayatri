module Screens.AboutUsScreen.View where

import Animation as Anim 
import Components.GenericHeader as GenericHeader
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB 
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, pure, unit, ($), (<<<), (==), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, lineHeight, linearLayout, margin, onBackPressed, orientation, padding, text, textSize, textView, weight, width, imageView, imageUrl, cornerRadius, onClick, afterRender, visibility, imageWithFallback)
import Screens.AboutUsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Common.Types.App
import Screens.CustomerUtils.AboutUsScreen.ComponentConfig 

screen :: ST.AboutUsScreenState -> Screen Action ST.AboutUsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "AboutUsScreen"
  , globalEvents : []
  , eval
  }

view :: forall w . (Action -> Effect Unit) -> ST.AboutUsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
    linearLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push (const BackPressed) 
    , background Color.white900
    , padding if EHC.os == "IOS" then (Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom) else (Padding 0 0 0 10)
    , gravity CENTER
    , afterRender push (const AfterRender)
    ][  GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
      , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.greySmoke
        ][]
      , topTextView state
      , linearLayout
        [ orientation VERTICAL
        , weight 1.0
        ][]
      
      , bottomLinksView state
      ]

--------------------------------------------------- topTextView -----------------------------------------------------
topTextView :: ST.AboutUsScreenState -> forall w . PrestoDOM (Effect Unit) w
topTextView state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (Padding 20 0 20 10)
    ][  logoView 
      , textView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , textSize FontSize.a_16
        , text (getString ABOUT_APP_DESCRIPTION)
        , color Color.black800
        , fontStyle $ FontStyle.regular LanguageStyle
        , gravity LEFT
        , lineHeight "22"
        , margin (Margin 0 40 0 32)
        ]
      , linearLayout
        [ gravity LEFT
        , width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ][  softwareLicenseView
          , termsAndConditionsView state
          , privacyPolicyView 
          ]
      ]

--------------------------------------------------- logoView -----------------------------------------------------
logoView :: forall w . PrestoDOM (Effect Unit) w
logoView = 
  linearLayout
        [ height $ V 48
        , width MATCH_PARENT
        , margin (MarginTop 48)
        , gravity CENTER
        , cornerRadius 10.0
        ][  imageView
              [ height $ V 48
              , width $ V 48
              , imageWithFallback "ny_ic_launcher,https://assets.juspay.in/nammayatri/images/common/ny_ic_launcher.png"
              ]
          ]

--------------------------------------------------- bottomLinksView -----------------------------------------------------
bottomLinksView :: ST.AboutUsScreenState -> forall w . PrestoDOM (Effect Unit) w
bottomLinksView state = 
   linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , orientation VERTICAL
        , margin (Margin 0 10 0 10)
        ][  textView
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $ "v" <> (getValueToLocalStore VERSION_NAME) <> " [ " <> (getValueToLocalStore BUNDLE_VERSION) <> " ]"
            , textSize FontSize.a_14
            , fontStyle $ FontStyle.semiBold LanguageStyle
            , color "#354052"
            ]
          ]
  
--------------------------------------------------- softwareLicenseView -----------------------------------------------------
softwareLicenseView :: forall w . PrestoDOM (Effect Unit) w
softwareLicenseView = 
  linearLayout
    [ height WRAP_CONTENT
    , orientation VERTICAL
    , width WRAP_CONTENT
    ][  linearLayout
        [ height WRAP_CONTENT
        , orientation VERTICAL
        , width WRAP_CONTENT
        , visibility GONE
        ][  textView
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text (getString SOFTWARE_LICENSE)
            , textSize FontSize.a_14
            , fontStyle $ FontStyle.regular LanguageStyle
            , color Color.blue900
            , margin (Margin 0 0 0 0)
            ]
          , linearLayout
            [ width MATCH_PARENT
            , height (V 1)
            , background Color.blue900
            ][]
          ]
        ]
      
--------------------------------------------------- termsAndConditionsView -----------------------------------------------------      
termsAndConditionsView :: ST.AboutUsScreenState -> forall w . PrestoDOM (Effect Unit) w
termsAndConditionsView state =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    ][  textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text (getString TERMS_AND_CONDITIONS)
        , textSize FontSize.a_14
        , fontStyle $ FontStyle.regular LanguageStyle
        , color Color.blue900
        , onClick (\action -> do
            _ <- JB.openUrlInApp "https://drive.google.com/file/d/1xKyhxGyK_xLusd8nX7s4AuXnTW9YrkO_/view?usp=sharing"
            pure unit
          ) (const TermsAndConditions)
        , margin (Margin 0 20 0 0)
        ]
      , linearLayout
              [ width MATCH_PARENT
              , height (V 1)
              , background Color.blue900
              ][]
        ]

--------------------------------------------------- privacyPolicyView -----------------------------------------------------
privacyPolicyView :: forall w . PrestoDOM (Effect Unit) w
privacyPolicyView =
  linearLayout
    [ height WRAP_CONTENT
    , orientation VERTICAL
    , width WRAP_CONTENT
    , visibility GONE
    ][  textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text (getString PRIVACY_POLICY)
        , textSize FontSize.a_14
        , fontStyle $ FontStyle.regular LanguageStyle
        , color Color.blue900
        , margin (Margin 0 20 0 0)
        ]
      , linearLayout
        [ width MATCH_PARENT
        , height (V 1)
        , background Color.blue900
        ][]
      ]
