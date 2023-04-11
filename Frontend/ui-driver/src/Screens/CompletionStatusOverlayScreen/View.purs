 module Screens.CompletionStatusOverlayScreen.View where

import Prelude(Unit,(/),unit,(<>),($),bind,pure,const,discard,void)
import Effect (Effect)
import Components.ReferralMobileNumber.Controller (Config(..))
import PrestoDOM (Gravity(..), Length(..), PrestoDOM(..), Margin(..), Orientation(..), Padding(..), Visibility(..), ScopedScreen, linearLayout, textView, editText, onBackPressed, onClick, imageView,afterRender)
import PrestoDOM.Properties(background,color,gravity, height, id, imageUrl, margin, orientation, padding,  text, textSize, width)
import Styles.Colors as Color
import Language.Strings (getString)
import Language.Types (STR(..))
import Engineering.Helpers.Commons (getNewIDWithTag, screenHeight)
import Font.Style as FontStyle
import Font.Size as FontSize
import Helpers.Utils (countDown)
import Data.Maybe
import Common.Types.App
import Screens.Types(CompletionStatusOverlayState)
import Screens.CompletionStatusOverlayScreen.Controller (Action(..), ScreenOutput(..),eval)
import Presto.Core.Types.Language.Flow (Flow, delay, doAff)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner)
import Control.Monad.Except.Trans (lift, runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import PrestoDOM (lottieAnimationView)
import JBridge (startLottieProcess)
import Animation (scaleAnim, fadeIn)

screen :: CompletionStatusOverlayState -> ScopedScreen Action CompletionStatusOverlayState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "CompletionStatusOverlay"
  , parent : Just "CompletionStatus"
  , globalEvents : [(\push -> do
      launchAff_ $ flowRunner $ runExceptT $ runBackT $ do
        lift $ lift $ void $ delay $ Milliseconds 4500.0
        lift $ lift $ doAff do liftEffect $ push CountDown
        pure unit
      pure $ pure unit
  
  )]
  , eval
  }


view :: forall w . (Action -> Effect Unit) -> CompletionStatusOverlayState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , background Color.white900
    , padding (PaddingHorizontal 40 40)
    ][ linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    , gravity CENTER_HORIZONTAL
    , background Color.white900
    , padding (PaddingBottom ((screenHeight unit) / 6 ))
    ][lottieLoaderView state push
     , textView( 
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , margin $ MarginVertical 0 4
        , text (getString MOBILE_NUMBER_VERIFIED)
        , gravity CENTER
        , color Color.black800
        ]<> FontStyle.h2 TypoGraphy)
        , textView 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER
        , text (getString YOU_ARE_ONE_STEP_CLOSER)
        , color Color.black700
        , textSize  FontSize.a_18
        ]
      ]
    ]
lottieLoaderView :: forall w.CompletionStatusOverlayState-> (Action -> Effect Unit) -> PrestoDOM ( Effect Unit) w 
lottieLoaderView state push = 
  lottieAnimationView 
  [ id (getNewIDWithTag "verified_lottie")
  , afterRender (\action-> do
                _ <- pure $ startLottieProcess state.data.lottieUrl (getNewIDWithTag "verified_lottie") true 0.6 "default"
                pure unit)(const CountDown)
  , height MATCH_PARENT
  , width WRAP_CONTENT
  ]
