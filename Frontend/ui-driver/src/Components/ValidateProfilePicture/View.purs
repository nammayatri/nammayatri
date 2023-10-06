{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.ValidateProfilePicture.View where

import Common.Types.App

import Animation as Anim
import Components.PrimaryButton as PrimaryButton
import Components.ValidateProfilePicture.Controller (Action(..), IssueListFlowState)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Show (show)
import Data.String (length)
import Debug (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, map, not, pure, unit, ($), (&&), (/), (<), (<<<), (<>), (==), (||))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alpha, background, clickable, color, cornerRadius, fontStyle, frameLayout, gravity, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, progressBar, relativeLayout, scrollView, stroke, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> IssueListFlowState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background state.background
        , afterRender push $ const AfterRender
        ]
        [ headerLayout state push
        , cameraLayout push state
        , if not state.cameraView then picturePreview state push else textView [ visibility GONE ]
        ]

cameraLayout :: forall w. (Action -> Effect Unit) -> IssueListFlowState -> PrestoDOM (Effect Unit) w
cameraLayout push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginVertical 15 28
    , visibility if state.cameraView then VISIBLE else GONE
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
            [ linearLayout
                [ width $ V 420
                , height $ V 420
                , layoutGravity "center"
                , id $ EHC.getNewIDWithTag "CameraX"
                ]
                []
            , imageView
                [ width $ V 420
                , height $ V 420
                , layoutGravity "center"
                , imageWithFallback "ny_ic_camera_overlay,"
                , alpha 0.7
                ]
            , imageView
                [ width $ V 240
                , height $ V 240
                , layoutGravity "center"
                , imageWithFallback "ny_ic_human_face_shape,"
                , alpha 0.7
                ]
            ]
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , margin $ MarginTop 15
        , orientation VERTICAL
        ]
        [ textView
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER_HORIZONTAL
            , text "Take a clear selfie with your face inside the \nmarked area"
            , color Color.white900
            ]
        , relativeLayout
          [ width $ V 56
          , height $ V 56
          , margin $ MarginTop 20
          , cornerRadius 180.0
          ][ linearLayout
              [ width $ V 55
              , height $ V 55
              , cornerRadius 180.0
              , background Color.white900
              , onClick push $ const PictureClick
              , clickable $ not state.buttonLoader
              ][]
          , progressBar
              [ width $ V 56
              , height $ V 56
              , stroke Color.black900
              , visibility if state.buttonLoader then VISIBLE else GONE
              ]
          ]
        ]
    ]

headerLayout :: IssueListFlowState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
headerLayout state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , layoutGravity "center_vertical"
    , margin $ MarginTop 5
    , padding state.headerConfig.padding
    ]
    [ imageView
        [ width state.headerConfig.imageConfig.width
        , height state.headerConfig.imageConfig.height
        , imageWithFallback state.headerImage
        , onClick push $ const BackPressed
        , padding state.headerConfig.imageConfig.padding
        , margin state.headerConfig.imageConfig.margin
        , color state.headerConfig.imageConfig.color
        ]
    , textView
        $ [ width state.headerConfig.headTextConfig.width
          , height state.headerConfig.headTextConfig.height
          , text state.headerConfig.headTextConfig.text
          , textSize state.headerConfig.headTextConfig.fontSize
          , margin state.headerConfig.headTextConfig.margin
          , weight state.headerConfig.headTextConfig.weight
          , color state.headerConfig.headTextConfig.color
          ]
    ]

picturePreview :: IssueListFlowState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
picturePreview state push =
  linearLayout
    [ width MATCH_PARENT
    , weight 1.0
    , orientation VERTICAL
    , background Color.black
    , margin $ MarginVertical 5 26
    ][ linearLayout
        [ weight 1.0
        , width MATCH_PARENT
        , gravity CENTER
        ][  linearLayout
            [ width $ V 277
            , height $ V 277
            , cornerRadius 180.0
            , afterRender push $ const AfterRender
            , id $ EHC.getNewIDWithTag "ValidateProfileImage"
            , stroke $ if (state.verificationStatus) then ("3," <> Color.darkGreen) else ("3," <> Color.red)
            ][]
        ]
      , textView
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , text if (state.verificationStatus) then (getString SELFIE_VERIFIED) else ((getString VERIFICATION_FAILED) <> " - " <> (state.failureReason) <> "\n" <> (getString PLEASE_RETAKE_SELFIE))
        , gravity CENTER_HORIZONTAL
        , color Color.white900
        , margin $ MarginBottom 34
        ]
      , PrimaryButton.view (push <<< (if state.verificationStatus then PrimaryButtonActionController else Retake)) (primaryButtonConfig state)
    ]

primaryButtonConfig :: IssueListFlowState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config

    primaryButtonConfig' =
      config
        { textConfig
          { text = if (state.verificationStatus) then (getString CONFIRM_AND_UPLOAD) else (getString RETAKE_SELFIE)
          , color = Color.yellow900
          }
        , margin = (Margin 17 0 25 10)
        , cornerRadius = 8.0
        , background = Color.black900
        , height = (V 60)
        }
  in
    primaryButtonConfig'