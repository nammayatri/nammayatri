{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.UploadAdhaarScreen.View where

import Prelude (Unit, bind, const, pure, unit, ($), (<<<), (<>), (/=), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, afterRender, background, clickable, color, cornerRadius, fontStyle, frameLayout, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, weight, width, imageWithFallback)
import Animation as Anim
import Effect (Effect)
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.UploadAdhaarScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Style as FontStyle
import Components.RegistrationModal as RegistrationModal
import Components.OnboardingHeader as OnboardingHeader
import Components.PrimaryButton as PrimaryButton
import JBridge as JB
import Effect.Class (liftEffect)
import Common.Types.App
import Screens.UploadAdhaarScreen.ComponentConfig
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))
import Engineering.Helpers.Utils (isEmpty)

screen :: ST.UploadAdhaarScreenState -> Screen Action ST.UploadAdhaarScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "UploadAdhaarScreen"
  , globalEvents : [(\push -> do
    _ <- JB.storeCallBackImageUpload push CallBackImageUpload
    pure $ pure unit)]
  , eval
  }

view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.UploadAdhaarScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  frameLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][
linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , clickable true
    , onBackPressed push (const BackPressed)
    , afterRender  (\action -> do
                      _<- push action
                      pure unit
                      ) $ const (AfterRender)
    ][  onboardingHeaderView state push
      , linearLayout
        [ width MATCH_PARENT
        , weight 1.0
        , orientation VERTICAL
        ][ scrollView
            [ width MATCH_PARENT
            , height MATCH_PARENT
            ][ linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , padding (Padding 20 25 20 0)
                ][
                  instructionSectionView state
                , frontUploadSection state push
                , backUploadSection state push
                    
                ]
              ]
          ]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        ][PrimaryButton.view (push <<< PrimaryButtonAction) (primaryButtonConfig state)]

    ]   
    , if state.props.openRegistrationModal then 
    linearLayout[
      width MATCH_PARENT
    , height MATCH_PARENT
      ] [registrationModalView state push] else linearLayout [][]

  ] 
  

registrationModalView :: ST.UploadAdhaarScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
registrationModalView state push = 
  RegistrationModal.view (push <<< RegistrationModalAction) ({
    openRegistrationModal: state.props.openRegistrationModal
  })

onboardingHeaderView :: ST.UploadAdhaarScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
onboardingHeaderView state push =
  OnboardingHeader.view (push <<< OnboardingHeaderAction) ({
    stepNumber: "2",
    barNumber: 2
  })

frontUploadSection :: ST.UploadAdhaarScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
frontUploadSection state push =
  linearLayout
  [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin (MarginTop 20)
    , onClick push (const( UploadFileAction "front"))
  ][
    textView
    ([ text (getString FRONT_SIDE)
    , color Color.black800
    ]<>FontStyle.body3 TypoGraphy)
  , linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , margin (Margin 0 10 0 10)
    , padding (Padding 16 16 16 16)
    , cornerRadius 4.0
    , stroke ("1," <> Color.borderGreyColor)
    ][ 
      textView
      ([ text if (isEmpty state.data.imageFront ) then (getString UPLOAD_FRONT_SIDE) else state.data.imageName
      , color if (isEmpty state.data.imageFront ) then Color.darkGrey else Color.greyTextColor
      , weight 1.0
      ]<> FontStyle.subHeading1 TypoGraphy)
    , if (state.data.imageFront /= "") then previewIcon state push "front" else
      imageView
      [ width ( V 20 )
      , height ( V 20 )
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_upload"
      ]
    ]
  ]

backUploadSection :: ST.UploadAdhaarScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
backUploadSection state push = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin (MarginTop 20)
  , orientation VERTICAL
  , onClick push (const (UploadFileAction "back"))
  ][
    textView
    ([ text (getString BACK_SIDE)
    , color Color.black800
    ]<>FontStyle.body3 TypoGraphy)
  , linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , margin (Margin 0 10 0 10)
    , padding (Padding 16 16 16 16)
    , cornerRadius 4.0
    , stroke ("1," <> Color.borderGreyColor)
    ][
      textView
      ([ text if (isEmpty state.data.imageBack) then (getString UPLOAD_BACK_SIDE) else state.data.imageName
      , color if (isEmpty state.data.imageBack) then Color.darkGrey else Color.greyTextColor
      , weight 1.0
      ]<>FontStyle.subHeading1 TypoGraphy)
    , if (state.data.imageBack /= "") then previewIcon state push "back" else
      imageView
      [ width ( V 20 )
      , height ( V 20 )
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_upload"
      ]
    ]
  ]
instructionSectionView :: ST.UploadAdhaarScreenState -> forall w . PrestoDOM (Effect Unit) w
instructionSectionView state = 
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin (MarginBottom 10)
    ][
      imageView
      [ width ( V 100 )
      , height ( V 100 ) 
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_aadhaar"
      ]
    , textView
      ([ width WRAP_CONTENT
      , text (getString UPLOAD_ADHAAR_CARD)
      , color Color.black800
      , margin (Margin 0 20 0 15)
      ] <> FontStyle.h1 TypoGraphy)
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , margin (Margin 0 11 0 11)
      ][
        imageView
        [ width ( V 20 )
        , height ( V 20 ) 
        , margin (MarginRight 9)
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_text_pointer_right"
        ]
      , textView
        ([ text (getString ADHAAR_INTRUCTION_PICTURE)
        , color Color.black800
        ]<>FontStyle.body3 TypoGraphy)
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , margin (Margin 0 11 0 11)
      ][
        imageView
        [ width ( V 20 )
        , height ( V 20 ) 
        , margin (MarginRight 9)
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_text_pointer_right"
        ]
      , textView
        ([ text (getString LICENSE_INSTRUCTION_CLARITY)
          , color Color.black800
        ]<>FontStyle.body3 TypoGraphy)
      ]
    ]


previewIcon :: ST.UploadAdhaarScreenState -> (Action -> Effect Unit) -> String -> forall w . PrestoDOM (Effect Unit) w
previewIcon state push previewType = 
  linearLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , gravity CENTER
    ][ textView
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text (getString PREVIEW)
        , color Color.blueTextColor
        , onClick (\action-> do
                      _ <- liftEffect $ JB.previewImage $ if(previewType == "front") then state.data.imageFront else state.data.imageBack
                      pure unit)(const PreviewImageAction)
        ] 
      , imageView
          [ height (V 20)
          , width (V 20)
          , margin (Margin 10 0 0 0)
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_cancel"
          , onClick push (const(RemoveUploadedFile previewType))
          ]
    ]