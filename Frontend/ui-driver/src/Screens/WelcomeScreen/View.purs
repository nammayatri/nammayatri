module Screens.WelcomeScreen.View where

import Animation as Anim
import Components.PrimaryButton as PrimaryButton
import Debug (spy)
import Effect (Effect)
import Data.Tuple.Nested((/\))
import Prelude --(Unit, bind, const, pure, unit, ($), (<<<), show, (+))
import Data.Maybe
import React.Render.CustomBase as ReactElement
import React.Basic.Hooks as React
import PrestoDOM (Accessiblity(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, accessibility, afterRender, background, gravity, height, id, imageView, imageWithFallback, linearLayout, margin, onBackPressed, textView, text, orientation, padding, weight, width, ScopedScreen)
import Screens.WelcomeScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (WelcomeScreenState)
import JBridge (addCarousel)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Data.Function.Uncurried (runFn2)
import Screens.WelcomeScreen.ComponentConfig
import Halogen.VDom.Types (VDom(..))

screen :: WelcomeScreenState -> ScopedScreen Action WelcomeScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , parent : Nothing
  , name: "WelcomeScreen"
  , globalEvents: []
  , eval:
      ( \state action -> do
          let _ = spy "WelcomeScreen ----- state" state
          let _ = spy "WelcomeScreen --------action" action
          eval state action
      )
  }


app :: React.Component {push :: (Action -> Effect Unit)}
app = do
  React.component "App" \{push} -> React.do  
    count /\ setCount <- React.useState 0
    let handleCount = do
          setCount (\prevCount -> prevCount + 1)
    
    React.useEffect count (\_ -> do
                            when ((count `mod` 5 )== 0 && count > 0) do push BackPressed
                            pure $ pure unit
                            )

    pure $ 
      ReactElement.linearLayout {
        width: "match_parent",
        height: "wrap_content"
      } [
        ReactElement.textView {text: (show count)},
        ReactElement.linearLayout {
          onClick: handleCount
        } [
          ReactElement.textView {text: "Increment"}
        ]
      ]
      

view :: forall w. (Action -> Effect Unit) -> WelcomeScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , accessibility DISABLE
        , gravity CENTER
        , onBackPressed push $ const BackPressed
        , afterRender push $ const AfterRender
        , background "#FFFAED"
        , padding $ PaddingBottom 24
        ][  imageView
            [ height $ V 50
            , width $ V 147
            , margin $ MarginTop 50
            , imageWithFallback "ic_namma_yatri_logo,https://assets.juspay.in/nammayatri/images/user/ic_namma_yatri_logo.png"   -- "ic_namma_yatri_logo"
            ]
            , carouselView state push 
            -- , textView [
            --   text "Hello"
            -- ] 
            -- , ReactElem $ app {push}
            , PrimaryButton.view (push <<< PrimaryButtonAC ) (primaryButtonConfig state)
        ]


carouselView:: WelcomeScreenState -> (Action -> Effect Unit)  -> forall w . PrestoDOM (Effect Unit) w
carouselView state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , id $ getNewIDWithTag "CarouselView"
    , accessibility DISABLE
    , gravity CENTER
    , weight 1.0
    , margin $ MarginBottom 20
    , afterRender (\action -> do
        _ <- push action
        _ <- runFn2 addCarousel (carouselData state) (getNewIDWithTag "CarouselView")
        pure unit
        ) (const AfterRender)
    ][]