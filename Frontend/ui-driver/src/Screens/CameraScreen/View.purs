{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.CameraScreen.View where 

import Animation (translateInYAnim, translateOutYAnim)
import Animation.Config (translateYAnimConfig)
import Data.Maybe
import Data.Tuple (Tuple(..))
import Debug
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried(runEffectFn1)
import JBridge (setupCamera, takePhoto, displayBase64Image, displayBase64ImageConfig)
import Engineering.Helpers.Commons as EHC
import Prelude
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM (Gravity(..), ScopedScreen, Length(..), PrestoDOM, Orientation(..), Visibility(..), Padding(..), Margin(..), linearLayout, height, width, orientation, onBackPressed, background, afterRender, margin, weight, frameLayout, layoutGravity, id, imageView, imageWithFallback, alpha, gravity, textView, text, color, relativeLayout, cornerRadius, onClick, progressBar, stroke, clickable, visibility, onAnimationEnd, rippleColor, padding)
import Screens.CameraScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color

screen :: ST.CameraScreenState -> ScopedScreen Action ST.CameraScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "CameraScreen"
  , globalEvents : []
  , parent : Just "CameraScreen"
   , eval :
      ( \action state -> do
          let _ = spy "CameraScreen ----- state" state
          let _ = spy "CameraScreen --------action" action
          eval action state
        ) 
  }


view :: forall w. (Action -> Effect Unit) -> ST.CameraScreenState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push (const BackPressed)
    , background Color.black900
    , clickable true
    ][
        cameraLayout push state
    ]

cameraLayout :: forall w. (Action -> Effect Unit) -> ST.CameraScreenState -> PrestoDOM (Effect Unit) w
cameraLayout push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginVertical 0 38
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , weight 1.0
        ]
        [ frameLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            ]
            [ Keyed.linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                ]
                [ if not state.props.imageClicked then Tuple "CameraX" $ cameraScreenView push state else Tuple "ImageView" $ photoClickedView push state
                ]
              , circularFrameLayout push state
            ]
        ]
    , if state.props.imageClicked then confirmOrRetakeImageView push state else footerView push state 
    ]

footerView :: forall w. (Action -> Effect Unit) -> ST.CameraScreenState -> PrestoDOM (Effect Unit) w
footerView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , padding $ PaddingTop 38
    , orientation VERTICAL
    , onClick 
        (\action -> do
            takePhoto push action
            ) (const $ PictureClick) 
    , clickable $ not state.props.imageClicked
    ]
    [ imageView 
        [ height $ V 40 
        , width $ V 40
        , imageWithFallback "ny_ic_camera_icon"
        , background $ Color.transparent
        ]
    ]

circularFrameLayout :: forall w. (Action -> Effect Unit) -> ST.CameraScreenState -> PrestoDOM (Effect Unit) w
circularFrameLayout push state = 
  linearLayout 
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout 
        [ width MATCH_PARENT
        , weight 1.0
        , background Color.black
        , alpha 0.7
        ][]
    , imageView
        [ width $ V $ EHC.screenWidth unit
        , height $ V $ EHC.screenWidth unit
        , layoutGravity "center"
        , imageWithFallback "ny_ic_camera_overlay,"
        , alpha 0.7 
        ]
        , linearLayout 
        [ width MATCH_PARENT
        , weight 1.0
        , background Color.black
        , alpha 0.7
        ][]
    ]

cameraScreenView :: forall w. (Action -> Effect Unit) -> ST.CameraScreenState -> PrestoDOM (Effect Unit) w
cameraScreenView push state = 
  linearLayout
    [ width $ MATCH_PARENT
    , height $ MATCH_PARENT
    , layoutGravity "center"
    , id $ EHC.getNewIDWithTag "CameraX"
    , afterRender
        (\action -> do
            void $ pure $ setupCamera (EHC.getNewIDWithTag "CameraX") false
            ) (const NoAction)
    ]
    []

photoClickedView :: forall w. (Action -> Effect Unit) -> ST.CameraScreenState -> PrestoDOM (Effect Unit) w
photoClickedView push state = 
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , layoutGravity "center"
    , id $ EHC.getNewIDWithTag "ImageView2"
    , background Color.black
    , alpha 0.7
    , afterRender
        (\action -> do 
            runEffectFn1 displayBase64Image displayBase64ImageConfig {source = state.data.clickedImageUrl, id = (EHC.getNewIDWithTag "ImageView2"), scaleType =  "FIT_CENTER", inSampleSize = 1} 
            pure unit
            ) (const NoAction)
    ]
    []

confirmOrRetakeImageView :: forall w. (Action -> Effect Unit) -> ST.CameraScreenState -> PrestoDOM (Effect Unit) w
confirmOrRetakeImageView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER
  , margin $ MarginTop 38
  , orientation HORIZONTAL
  ]
  [ linearLayout 
      [ height WRAP_CONTENT
      , weight 1.0
      , gravity CENTER
      , onClick push $ const RetakeButtonPressed
      ]
      [ imageView
          [ height $ V 30
          , width $ V 30
          , imageWithFallback "ny_ic_retake_icon"
          ]
      ]
    , linearLayout 
      [ height WRAP_CONTENT
      , weight 1.0
      , gravity CENTER
      ]
      [ imageView
          [ height $ V 50
          , width $ V 50
          , imageWithFallback "ny_ic_tick_white"
          , onClick push $ const ConfirmImage
          ]
      ]
    , linearLayout 
      [ height WRAP_CONTENT
      , weight 1.0
      , gravity CENTER
      , onClick push $ const BackPressed
      ]
      [ imageView
          [ height $ V 30
          , width $ V 30
          , imageWithFallback "ic_close"
          ]
      ]  
  ]