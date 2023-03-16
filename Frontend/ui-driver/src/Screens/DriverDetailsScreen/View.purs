{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverDetailsScreen.View where

import Prelude (Unit, bind, const, map, pure, unit, ($), (/), (==), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, orientation, padding, text, textSize, textView, weight, width, onClick, frameLayout, layoutGravity, alpha, scrollView, cornerRadius, onBackPressed, afterRender, id, visibility, imageWithFallback)
import Effect (Effect)
import Screens.DriverDetailsScreen.Controller (Action(..), ScreenOutput, eval, getTitle, getValue)
import Screens.DriverDetailsScreen.ScreenData (ListOptions(..), optionList)
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

screen :: ST.DriverDetailsScreenState -> Screen Action ST.DriverDetailsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "DriverDetailsScreen"
  , globalEvents : [(\push -> do
    _ <- JB.storeCallBackImageUpload push CallBackImageUpload
    pure $ pure unit)]
  , eval
  }

view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.DriverDetailsScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push (const BackPressed)
    , afterRender push (const AfterRender)
    ][ headerLayout state push
     , profilePictureLayout state push
     , driverDetailsView state
    ]



---------------------------------------------- profilePictureLayout ------------------------------------
profilePictureLayout :: ST.DriverDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
profilePictureLayout state push =
    frameLayout
    [ width MATCH_PARENT
    , height $ V ((EHC.screenHeight unit)/5)
    , margin (MarginTop 10)
    ][ linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , layoutGravity "center"
        ][ frameLayout
            [ width $ V 90
            , height $ V 90
            ][ imageView
                [ width $ V 90
                , height $ V 90
                , layoutGravity "center"
                , cornerRadius 45.0
                , id (EHC.getNewIDWithTag "EditProfileImage")
                , imageWithFallback "ny_ic_profile_image,https://assets.juspay.in/nammayatri/images/common/ny_ic_profile_image.png"
                -- TODO : after 15 aug
                -- , afterRender push (const RenderBase64Image)
                ]
              , linearLayout
                [ width MATCH_PARENT
                , height $ V 30
                , layoutGravity "bottom"
                ][ linearLayout
                    [ width MATCH_PARENT
                    , height $ V 30
                    , gravity RIGHT
                    ][ imageView
                        [ width $ V 30
                        , height $ V 30
                        , gravity RIGHT
                        , cornerRadius 45.0
                        , imageWithFallback "ny_ic_camera_white,https://assets.juspay.in/nammayatri/images/driver/ny_ic_camera_white.png"
                        , visibility GONE 
                        -- To be added after 15 aug
                        -- , onClick (\action-> do
                        --       _ <- liftEffect $ JB.uploadFile unit
                        --       pure unit)(const UploadFileAction)
                        ]
                    ]
                ]

            ]
        ]
    ]


-------------------------------------------------- headerLayout --------------------------
headerLayout :: ST.DriverDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerLayout state push = 
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , orientation VERTICAL
 ][ linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation HORIZONTAL
    , layoutGravity "center_vertical"
    , padding (Padding 5 0 5 0)
    ][ imageView
        [ width $ V 25
        , height MATCH_PARENT
        , imageWithFallback "ny_ic_back,https://assets.juspay.in/nammayatri/images/driver/ny_ic_back.png"
        , gravity CENTER_VERTICAL
        , onClick push (const BackPressed)
        , padding (Padding 2 2 2 2)
        , margin (MarginLeft 5)
        ]
      , textView
        [ width WRAP_CONTENT
        , height MATCH_PARENT
        , text (getString PERSONAL_DETAILS)
        , textSize FontSize.a_19
        , margin (MarginLeft 20)
        , color Color.black
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , weight 1.0
        , gravity CENTER_VERTICAL
        , alpha 0.8
        ]
    ]
  , horizontalLineView 0 0
 ]


------------------------------------------ driverDetailsView ---------------

driverDetailsView :: ST.DriverDetailsScreenState -> forall w . PrestoDOM (Effect Unit) w
driverDetailsView state =
 scrollView
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 ][ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding (PaddingBottom 5)
    ] (map(\optionItem ->
            linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , gravity CENTER_VERTICAL
            , visibility if (optionItem.title == DRIVER_LICENCE_INFO) then GONE else VISIBLE
            , margin (MarginTop 20)
            ][ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              , gravity CENTER_VERTICAL
              , padding (Padding 15 0 15 0)
              , margin (MarginBottom 20)
              ][ textView
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text (getTitle optionItem.title)
                  , color Color.black800
                  , fontStyle $ FontStyle.medium LanguageStyle
                  , textSize FontSize.a_14
                  , alpha 0.9
                  ]
                  , textView (
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , margin (MarginTop 10)
                  , color Color.black900
                  , text (getValue optionItem.title state)
                  ] <> FontStyle.subHeading2 TypoGraphy
                  )
              ]
              , if(optionItem.title == DRIVER_LICENCE_INFO) then dummyTextView else horizontalLineView 15 15
            ]
          ) optionList
    )
 ]



--------------------------------- horizontalLineView and dummyTextView -------------------

horizontalLineView :: Int -> Int -> forall w . PrestoDOM (Effect Unit) w
horizontalLineView marginLeft marginRight = 
 linearLayout
  [ width MATCH_PARENT
  , height $ V 1
  , background Color.greyLight
  , margin (Margin marginLeft 0 marginRight 0)
  ][]

dummyTextView :: forall w . PrestoDOM (Effect Unit) w
dummyTextView = 
 textView
 [ width WRAP_CONTENT
 , height WRAP_CONTENT
 ]