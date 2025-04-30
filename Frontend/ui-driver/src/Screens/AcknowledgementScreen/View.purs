module Screens.AcknowledgementScreen.View where

import Animation as Anim
import Common.Types.App (LazyCheck(..)) as LazyCheck
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryButton as PrimaryButton
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Debug (spy)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import Halogen.VDom.DOM.Prop (Prop)
import JBridge as JB
import Prelude (Unit, const, map, ($), (<<<), (<>), bind, pure, unit)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), afterRender, alignParentBottom, alpha, background, color, cornerRadius, fontStyle, gravity, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, lottieAnimationView, margin, onBackPressed, onClick, orientation, padding, relativeLayout, stroke, text, textSize, textView, visibility, weight, width)
import Screens.AcknowledgementScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Mobility.Prelude (boolToVisibility)


screen :: ST.AcknowledgementScreenState -> Screen Action ST.AcknowledgementScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "AcknowledgementScreen"
  , globalEvents: []
  , eval:
      ( \state action -> do
          let _ = spy "AcknowledgementScreen ----- state" state
          let _ = spy "AcknowledgementScreen --------action" action
          eval state action
      )
  }


view :: forall w. (Action -> Effect Unit) -> ST.AcknowledgementScreenState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity CENTER
        , onBackPressed push $ const BackPressed
        , afterRender push $ const AfterRender
        , background Color.white900
        , padding $ PaddingBottom 24
        ][  illustrationView state push
          , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , alignParentBottom "true,-1"
            , visibility $ boolToVisibility state.data.primaryButtonVisibility
            ][ case state.data.primaryButtonText of
                Maybe.Nothing -> linearLayout[][]
                Maybe.Just text -> PrimaryButton.view (push <<< PrimaryButtonAC ) (primaryButtonConfig text)
                ]
        ]

illustrationView:: ST.AcknowledgementScreenState -> (Action -> Effect Unit)  -> forall w . PrestoDOM (Effect Unit) w
illustrationView state push = 
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , margin $ MarginHorizontal 20 20
    ][ case state.props.illustrationType of
        ST.Lottie ->  lottieAnimationView
                      [ width $ V 150
                      , height $ V 150
                      , id $ getNewIDWithTag "successAnim"
                      , afterRender (\action -> do
                            _ <- pure $ JB.startLottieProcess JB.lottieAnimationConfig{ rawJson = state.data.illustrationAsset , lottieId = (getNewIDWithTag "successAnim"),scaleType = "CENTER_CROP", repeat = false}
                            pure unit) (const NoAction)
                      ]
        ST.Image ->   imageView
                      [ width MATCH_PARENT
                      , height $ V 250
                      , imageWithFallback  state.data.illustrationAsset
                      ]
      , commonTV push state.data.title Color.black900 FontStyle.h2 CENTER 10 NoAction
      , commonTV push state.data.description Color.black800 FontStyle.subHeading2 CENTER 10 NoAction
      , case state.data.orderId of
          Maybe.Just id -> commonTV push (Just ("Order Id: " <> id)) Color.black800 FontStyle.subHeading2 CENTER 10 NoAction
          Maybe.Nothing -> linearLayout[][]
    ]

commonTV :: forall w .  (Action -> Effect Unit) -> Maybe.Maybe String -> String -> (LazyCheck.LazyCheck -> forall properties. (Array (Prop properties))) -> Gravity -> Int -> Action -> PrestoDOM (Effect Unit) w
commonTV push text' color' theme gravity' marginTop action = 
  textView $
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , color color'
  , gravity gravity'
  , margin $ MarginTop marginTop
  , onClick push $ const action
  ] <> case text' of
        Maybe.Nothing -> [visibility GONE]
        Maybe.Just txt -> [text txt]
    <> theme LazyCheck.TypoGraphy

primaryButtonConfig :: String -> PrimaryButton.Config
primaryButtonConfig text = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig { text = text }
      , id = "PrimaryButtonAckScreen"
      }
  in primaryButtonConfig'