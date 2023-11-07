{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.ValidateDocumentModal.View where

import Common.Types.App

import Animation as Anim
import Components.PrimaryButton as PrimaryButton
import Components.ValidateDocumentModal.Controller (Action(..), ValidateDocumentModalState)
import Data.Array as DA
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
import Prelude (Unit, bind, const, map, pure, unit, ($), (/), (==), (<>), (<<<), (&&), (||), (<), (/=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alpha, background, clickable, color, cornerRadius, fontStyle, frameLayout, gravity, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, progressBar, relativeLayout, scrollView, stroke, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (StageStatus(..), ValidationStatus(..))
import Styles.Colors as Color

view :: forall w . (Action -> Effect Unit) -> ValidateDocumentModalState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  relativeLayout
  [height MATCH_PARENT
    , width MATCH_PARENT
    , background state.background
    , orientation VERTICAL]
  ([ linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , afterRender push (const AfterRender)
    ][
      headerLayout state push
      ,linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , visibility if (state.profilePictureCapture == true) then VISIBLE else GONE
        , id (EHC.getNewIDWithTag "ProfilePictureCaptureLayout")
        ][]
      , if (state.profilePictureCapture == false) then (profilePictureLayout state push) else textView[]
      , if DA.any (_ == state.verificationStatus)[None, Failure] then PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state) else textView[]
    ]]
  )

headerLayout :: ValidateDocumentModalState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerLayout state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        , layoutGravity "center_vertical"
        , padding state.headerConfig.padding                      
        ]
        [ imageView
            [ width  state.headerConfig.imageConfig.width                        
            , height state.headerConfig.imageConfig.height                       
            , imageWithFallback state.headerImage
            , onClick push $ const BackPressed
            , padding state.headerConfig.imageConfig.padding    
            , margin  state.headerConfig.imageConfig.margin  
            , color   state.headerConfig.imageConfig.color                             
            , onClick push (const BackPressed)
            ]
        , textView
            $ [ width  state.headerConfig.headTextConfig.width                                 
              , height  state.headerConfig.headTextConfig.height                                
              , text  state.headerConfig.headTextConfig.text                              
              , textSize state.headerConfig.headTextConfig.fontSize        
              , margin  state.headerConfig.headTextConfig.margin   
              , weight  state.headerConfig.headTextConfig.weight                                             
              , color  state.headerConfig.headTextConfig.color                                                          
              ]
        ]
    , linearLayout
        [ width MATCH_PARENT
        , margin if (state.profilePictureCapture == false) then (MarginBottom 45 ) else (MarginBottom 15 )
        , background Color.black900
       
        ]
        []
    ]

profilePictureLayout :: ValidateDocumentModalState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
profilePictureLayout state push =
    frameLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    ][ linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , layoutGravity "center"
        ][ frameLayout
            [width WRAP_CONTENT
            , height MATCH_PARENT
            , margin (MarginTop 70)
            , layoutGravity "center"
            ][ linearLayout
                [ width $ V 350
                , afterRender push (const AfterRender)
                , height $ V 250
                , margin (MarginTop 2)
                , layoutGravity "center"
                , gravity CENTER_HORIZONTAL
                , id (EHC.getNewIDWithTag "ValidateProfileImage")
                ][]
                ,imageView
                [ width $ V 354
                , height $ V 255
                , layoutGravity "center"
                , cornerRadius 4.0
                , stroke $ (if state.verificationStatus /= Failure then ("4,"<> Color.darkGreen) else ("4,"<> Color.red))
                ]
            ],
        linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , margin (MarginVertical 50 60)
        ][
           progressBar
            [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , stroke Color.grey900
              , visibility if state.verificationStatus == InProgress then VISIBLE else GONE
            ]
        ,textView
        [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , padding (PaddingHorizontal 20 20)
        , text case state.verificationStatus of 
                Success -> (getString $ if state.verificationType == "DL" then DL_UPLOADED else RC_UPLOADED) <> "✅"
                Failure -> ((getString VERIFICATION_FAILED)<>"  -  "<>(state.failureReason)<>" \n"<>(getString if state.verificationType == "DL" then RETAKE_DL else RETAKE_RC))
                InProgress -> getString if state.verificationType == "DL" then DL_UPLOADING else RC_UPLOADING
                _ -> ""
        , textSize FontSize.a_16
        , fontStyle $ FontStyle.medium LanguageStyle
        , color Color.white900
        ]
        ]
        ]
    ]

primaryButtonConfig :: ValidateDocumentModalState -> PrimaryButton.Config 
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = getString if state.verificationStatus == None then CONFIRM_AND_UPLOAD else RETAKE_PHOTO
      , color = Color.yellow900
      }
      , margin = (Margin 18 0 25 10)
      , cornerRadius = 8.0
      , background = Color.black900
      , height = (V 60)
      }
  in primaryButtonConfig'
------------------------------------------ driverDetailsView ---------------

