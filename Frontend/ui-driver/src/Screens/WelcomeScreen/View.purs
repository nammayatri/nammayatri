module Screens.WelcomeScreen.View where

import Animation as Anim
import Components.PrimaryButton as PrimaryButton
import Debug (spy)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Prelude (Unit, const, map, ($), (<<<), (<>), bind, pure, unit)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, afterRender, alpha, background, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, id, imageUrl)
import Screens.WelcomeScreen.Controller (Action(..), ScreenOutput, eval)
import Styles.Colors as Color
import Screens.Types (WelcomeScreenState, CarouselModel)
import Helpers.Utils (addCarousel)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Components.PrimaryButton as PrimaryButton
import Language.Strings (getString)
import Language.Types (STR(..))


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
        , gravity CENTER
        , onBackPressed push $ const BackPressed
        , afterRender push $ const AfterRender
        , background "#FFFAED"
        , padding $ PaddingBottom 24
        ][  imageView
            [ height $ V 50
            , width $ V 147
            , margin $ MarginTop 50
            , imageWithFallback "ny_namma_yatri,https://assets.juspay.in/nammayatri/images/user/ny_namma_yatri"   -- "ic_namma_yatri_logo"
            ]
            , carouselView state push
            , PrimaryButton.view (push <<< PrimaryButtonAC ) (primaryButtonConfig state)
        ]

carouselView :: WelcomeScreenState -> (Action -> Effect Unit)  -> forall w . PrestoDOM (Effect Unit) w
carouselView state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , id $ getNewIDWithTag "CarouselView"
    , gravity CENTER
    , weight 1.0
    , margin $ MarginBottom 20
    , afterRender (\action -> do
        _ <- push action
        _ <- addCarousel (getCarouselViewData state) (getNewIDWithTag "CarouselView")
        pure unit
        ) (const AfterRender)
    ][]

primaryButtonConfig :: WelcomeScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig { text = "Get Started" }
      , id = "PrimaryButtonWelcomeScreen"
      }
  in primaryButtonConfig'

getCarouselViewData :: WelcomeScreenState -> Array CarouselModel
getCarouselViewData state = [
      {image : "ny_ic_welcome_screen_1", title : getString DIRECT_PAYMENT_NO_COMMISSIONS, description : getString CUSTOMER_PAYS_DIRECTLY},
      {image : "ny_ic_welcome_screen_2", title : getString HUNDRED_PERCENT_FARE_GOES_TO_YOU, description : getString FARE_SHOWN_IS_FARE_YOU_GET},
      {image : "ny_ic_welcome_screen_3", title : getString BE_A_PART_OF_OPEN_MOBILITY_REVOLUTION, description : getString OUR_DATA_AND_PRODUCT_ARE_TRANSPARENT}
    ]