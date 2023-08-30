module Screens.OnBoardingFlow.WelcomeScreen.View where

import Animation as Anim
import Components.PrimaryButton as PrimaryButton
import Debug (spy)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Prelude (Unit, const, map, ($), (<<<), (<>), bind, pure, unit, (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Accessiblity(..), PrestoDOM, Prop, Screen, afterRender, alpha, background, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, id, imageUrl, accessibilityHint, accessibilityImportance)
import Screens.OnBoardingFlow.WelcomeScreen.Controller (Action(..), ScreenOutput, eval)
import Styles.Colors as Color
import Screens.Types (WelcomeScreenState)
import Helpers.Utils (addCarousel)
import Engineering.Helpers.Commons (getNewIDWithTag, os)
import Components.PrimaryButton as PrimaryButton


screen :: WelcomeScreenState -> Screen Action WelcomeScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "WelcomeScreen"
  , globalEvents: []
  , eval:
      ( \state action -> do
          let _ = spy "WelcomeScreen ----- state" state
          let _ = spy "WelcomeScreen --------action" action
          eval state action
      )
  }


view :: forall w. (Action -> Effect Unit) -> WelcomeScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , accessibilityImportance DISABLE
        , gravity CENTER
        , onBackPressed push $ const BackPressed
        , afterRender push $ const AfterRender
        , background "#FFFAED"
        , padding $ PaddingBottom 24
        ][  imageView
            [ height $ V 50
            , width $ V 147
            , accessibilityHint "Namma Yatri : Logo"
            , margin $ MarginTop if os == "IOS" then 80 else 50
            , imageWithFallback "ic_namma_yatri_logo,https://assets.juspay.in/nammayatri/images/user/ic_namma_yatri_logo.png"   -- "ic_namma_yatri_logo"
            ]
            , carouselView state push
            , PrimaryButton.view (push <<< PrimaryButtonAC ) (primaryButtonConfig state)
        ]

carouselView:: WelcomeScreenState -> (Action -> Effect Unit)  -> forall w . PrestoDOM (Effect Unit) w
carouselView state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , id $ getNewIDWithTag "CarouselView"
    , accessibilityImportance ENABLE
    , gravity CENTER
    , weight 1.0
    , margin $ MarginBottom 20
    , afterRender (\action -> do
        _ <- push action
        _ <- addCarousel state.data.carouselModel (getNewIDWithTag "CarouselView")
        pure unit
        ) (const AfterRender)
    ][]

primaryButtonConfig :: WelcomeScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig { text = "Get Started"
      , accessibilityHint = "Get Started : Button" }
      , id = "PrimaryButtonWelcomeScreen"
      }
  in primaryButtonConfig'