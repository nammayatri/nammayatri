
{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.ValidateProfilePicture.View where

import Prelude (Unit, bind, const, map, pure, unit, ($), (/), (==), (<>),(<<<),(&&),(||),(<))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, orientation, padding, text, textSize, textView, weight, width, onClick, frameLayout, layoutGravity, alpha, scrollView, cornerRadius, onBackPressed, afterRender, id, visibility, imageWithFallback, clickable, relativeLayout, stroke)
import Effect (Effect)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Style as FontStyle
import Font.Size as FontSize
import Engineering.Helpers.Commons as EHC
import Animation as Anim
import Language.Strings (getString)
import Language.Types(STR(..))
import JBridge as JB
import Effect.Class (liftEffect)
import Common.Types.App
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Debug (spy)
import Components.PrimaryButton as PrimaryButton
import Data.String (length)
import Components.ValidateProfilePicture.Controller (Action(..), IssueListFlowState)
import Data.Show (show)
import PrestoDOM.Types.DomAttributes (Corners(..))

view :: forall w . (Action -> Effect Unit) -> IssueListFlowState -> PrestoDOM (Effect Unit) w
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
      , if (state.profilePictureCapture == false) then PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state) else textView[]
    ]]
  )

headerLayout :: IssueListFlowState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
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

profilePictureLayout :: IssueListFlowState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
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
            , cornerRadius 250.0
            , layoutGravity "center"
            ][ linearLayout
                [ width $ V 365
                , afterRender push (const AfterRender)
                , height $ V 365
                , margin (MarginTop 45)
                , cornerRadius 250.0
                , layoutGravity "center"
                , id (EHC.getNewIDWithTag "ValidateProfileImage")
                , layoutGravity "center"
                ][]
                ,imageView
                [ width $ V 277
                , height $ V 277
                , layoutGravity "center"
                , cornerRadius 180.0
                , stroke $ (if (state.verificationStatus) then ("3,"<> Color.darkGreen) else ("3,"<> Color.red))
                ]
            ],
        linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , layoutGravity "center"
        , margin (MarginBottom 60)
        ][
        textView
        [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text if (state.verificationStatus) then (getString SELFIE_VERIFIED) else ((getString VERIFICATION_FAILED)<>" - < "<>(state.failureReason)<>" > \n"<>(getString PLEASE_RETAKE_SELFIE))
        , textSize FontSize.a_16
        , gravity CENTER_HORIZONTAL
        , fontStyle $ FontStyle.medium LanguageStyle
        , color Color.white900
        ]
        ,
        imageView
                [ width $ V 14
                , height $ V 14
                , margin (Margin 5 5 0 0)
                , visibility if state.verificationStatus then VISIBLE else GONE
                , imageWithFallback "ny_ic_green_box_tick,https://assets.juspay.in/nammayatri/images/driver/ny_ic_green_box_tick"
                ]
        ]
        ]
    ]

primaryButtonConfig :: IssueListFlowState -> PrimaryButton.Config 
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = if (state.verificationStatus) then (getString CONFIRM_AND_UPLOAD) else (getString RETAKE_SELFIE)
      , color = Color.yellow900
      }
      , margin = (Margin 17 0 25 10)
      , cornerRadius = 8.0
      , background = Color.black900
      , height = (V 60)
      }
  in primaryButtonConfig'
------------------------------------------ driverDetailsView ---------------

