{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReportIssueChatScreen.View where

import Animation (screenAnimation, screenAnimationFadeInOut)
import Common.Types.App (LazyCheck(..))
import Components.ChatView.Controller (makeChatComponent')
import Data.Array (length)
import Data.Maybe (fromMaybe, isJust, Maybe(..))
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth, convertUTCtoISC)
import Font.Style (bold)
import Helpers.Utils (getCurrentUTC, fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, not, pure, show, unit, ($), (&&), (-), (<<<), (<>), (>), (||), discard, void)
import PrestoDOM (linearLayout)
import PrestoDOM.Elements.Elements (frameLayout, imageView, relativeLayout, textView)
import PrestoDOM.Events (afterRender, onBackPressed, onClick)
import PrestoDOM.Properties (adjustViewWithKeyboard, alignParentBottom, alpha, background, color, cornerRadii, cornerRadius, fontStyle, gravity, height, imageUrl, imageWithFallback, layoutGravity, lineHeight, margin, maxWidth, orientation, padding, position, stroke, text, textSize, visibility, weight, width, clickable, rippleColor)
import PrestoDOM.Types.Core (Corners(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Position(..), PrestoDOM, Screen, Visibility(..))
import Screens.ReportIssueChatScreen.ComponentConfig (cancelButtonConfig, doneButtonConfig, primaryEditTextConfig, viewImageModelConfig, addImageModelConfig, recordAudioModelConfig, addAudioModelConfig)
import Screens.ReportIssueChatScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (ReportIssueChatScreenState)
import Components.AddAudioModel (view) as AddAudioModel
import Components.AddImagesModel (view) as AddImagesModel
import Components.ChatView as ChatView
import Styles.Colors (black800, black900, black9000, blue900, brightBlue, blueTextColor, grey900, greyLight, lightGreyBlue, white900, rippleShade) as Color
import Font.Size (a_16, a_20, a_14, a_17, a_18) as FontSize
import Font.Style (h3, semiBold) as FontStyle
import JBridge (storeCallBackImageUpload, storeCallBackUploadMultiPartData) as JB
import Components.PrimaryButton.View (view) as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.RecordAudioModel.View (view) as RecordAudioModel
import Data.String (length, trim) as STR
import Components.ViewImageModel.View (view) as ViewImageModel
import Effect.Uncurried (runEffectFn2)

screen :: ReportIssueChatScreenState -> Screen Action ReportIssueChatScreenState ScreenOutput
screen initialState =
    { initialState
    , view
    , name : "ReportIssueChatScreen"
    , globalEvents : [(\push -> do
        void $ JB.storeCallBackImageUpload push ImageUploadCallback
        void $ runEffectFn2 JB.storeCallBackUploadMultiPartData push UploadMultiPartDataCallback
        pure $ pure unit)
    ]
    , eval
    }

view :: forall w . (Action -> Effect Unit) -> ReportIssueChatScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimation
    $ frameLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , onBackPressed push (const BackPressed)
      , afterRender push (const AfterRender)
      , background Color.white900
      ] ([ relativeLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          ][ linearLayout
           [ height MATCH_PARENT
           , width MATCH_PARENT
           , orientation VERTICAL
           ][ headerLayout state push
            , issueIdHeader state push
            , chatView push state
           ]
           , submitView push state
           , helperView push state
           ]
           , linearLayout
           [ width MATCH_PARENT
           , height MATCH_PARENT
           , background Color.black9000
           , onClick push (const NoAction)
           , gravity CENTER
           , visibility if state.props.isPopupModelOpen then VISIBLE else GONE
           ] (
              if state.props.showAudioModel then [ addAudioModel state push ] else []
           <> if state.props.showImageModel then [ addImageModel state push ] else []
           <> if state.props.showRecordModel then [ recordAudioModel state push ] else []
           <> if state.props.showCallCustomerModel then [ callCustomerModel state push ] else []
           )
     ]
     <>  if state.props.showViewImageModel then [viewImageModel state push] else []
     )


-------------------------------------------------- headerLayout --------------------------
headerLayout :: ReportIssueChatScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
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
        , padding $ Padding 5 16 5 16
        ]
        [ imageView
            [ width $ V 40
            , height $ V 40
            , imageUrl "ny_ic_chevron_left"
            , onClick push $ const BackPressed
            , padding $ Padding 7 7 7 7
            , margin $ MarginLeft 5
            , rippleColor Color.rippleShade
            , cornerRadius 20.0
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text state.data.categoryName
              , textSize FontSize.a_18
              , margin $ MarginLeft 10
              , weight 1.0
              , color Color.black900
              ]
            <> FontStyle.h3 TypoGraphy
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.greyLight
        ]
        []
    ]

issueIdHeader :: ReportIssueChatScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
issueIdHeader state push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , padding (Padding 15 10 10 10)
  , background Color.lightGreyBlue
  , visibility if isJust state.data.issueId then VISIBLE else GONE
  ][ textView
     [ width WRAP_CONTENT
     , height MATCH_PARENT
     , text ((getString ISSUE_NO) <> ": " <> fromMaybe "" state.data.issueId)
     , gravity CENTER_VERTICAL
     , textSize FontSize.a_14
     , color Color.black800
     , lineHeight "25"
     ]
   , linearLayout
     [ width MATCH_PARENT
     , height MATCH_PARENT
     ][ textView
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , textSize FontSize.a_17
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , gravity RIGHT
        , color Color.blueTextColor
        ]
      ]
   ]

chatView :: forall w. (Action -> Effect Unit) -> ReportIssueChatScreenState -> PrestoDOM (Effect Unit) w
chatView push state =
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , padding if state.props.showSubmitComp then (PaddingBottom 274) else (PaddingBottom 52)
  , adjustViewWithKeyboard "true"
  , afterRender push (do
      if state.props.isReversedFlow
      then
        pure $ SendMessage (makeChatComponent' (getString ASK_DETAILS_MESSAGE_REVERSED) Nothing Nothing Nothing "Bot" (getCurrentUTC "") "Text" 500) true
      else
        pure ShowOptions
    )
  , background Color.white900
  , orientation VERTICAL
  ][ ChatView.view (push <<< ChatViewActionController) state.data.chatConfig
  ]

submitView :: (Action -> Effect Unit) -> ReportIssueChatScreenState -> forall w. PrestoDOM (Effect Unit) w
submitView push state =
  linearLayout
  [ height WRAP_CONTENT
  , position FIXED
  , width MATCH_PARENT
  , adjustViewWithKeyboard "true"
  , stroke ("1,"<>Color.grey900)
  , visibility if state.props.showSubmitComp then VISIBLE else GONE
  , padding (Padding 20 0 20 20)
  , background Color.white900
  , cornerRadii (Corners 20.0 true true false false)
  , orientation VERTICAL
  , alignParentBottom "true,-1"
  ][ PrimaryEditText.view (push <<< PrimaryEditTextActionController) (primaryEditTextConfig "")
   , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER
    , orientation HORIZONTAL
    , margin (MarginVertical 16 16)
    ][ linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , orientation HORIZONTAL
     ][ linearLayout
      [ weight 1.0
      , width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER
      , stroke ("1,"<>Color.grey900)
      , cornerRadius 18.0
      , onClick push (const AddImage)
      , margin (MarginRight 12)
      ][ imageView
       [ width $ V 36
       , height $ V 36
       , margin (MarginRight 8)
       , imageWithFallback $ fetchImage FF_ASSET "ny_ic_add_image"
       ]
       , textView
       [ weight 1.0
       , height WRAP_CONTENT
       , text if (length state.data.addedImages > 0) then (show $ length state.data.addedImages) <> " " <> (if length state.data.addedImages > 1 then (getString IMAGES_ADDED) else (getString IMAGE_ADDED)) else (getString ADD_IMAGE)
       , color if (length state.data.addedImages > 0) then Color.blue900 else Color.black900
       , textSize FontSize.a_14
       ]
       , imageView
       [ width $ V 24
       , height $ V 24
       , margin (MarginRight 6)
       , imageWithFallback $ fetchImage FF_ASSET "ny_ic_close_bold"
       , visibility if (length state.data.addedImages > 0) then VISIBLE else GONE
       , onClick push (const DeleteSelectedImages)   
       ]
      ]
      , linearLayout
      [ weight 1.0
      , width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER
      , stroke ("1,"<>Color.grey900)
      , cornerRadius 18.0
      , onClick push (const $ AddAudio false)
      ][ imageView
       [ width $ V 36
       , height $ V 36
       , margin (MarginRight 8)
       , imageWithFallback $ fetchImage FF_COMMON_ASSET  "ny_ic_add_audio"
       ]
       , textView
       [ weight 1.0
       , height WRAP_CONTENT
       , text if (isJust state.data.recordedAudioUrl) then (getString VOICE_NOTE_ADDED) else (getString ADD_VOICE_NOTE)
       , color if (isJust state.data.recordedAudioUrl) then Color.blue900 else Color.black900
       , textSize FontSize.a_14
       ]
       , imageView
       [ width $ V 24
       , height $ V 24
       , margin (MarginRight 6)
       , imageWithFallback $ fetchImage FF_ASSET "ny_ic_close_bold"
       , visibility if (isJust state.data.recordedAudioUrl) then VISIBLE else GONE
       , onClick push (const DeleteRecordedAudio)
       ]
      ]
     ]
    ]
    , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.brightBlue
    , cornerRadius 32.0
    , padding (PaddingVertical 12 12)
    , orientation HORIZONTAL
    , alpha if shouldEnableSubmitBtn state then 1.0 else 0.5
    , onClick push $ const SubmitIssue
    , gravity CENTER
    , clickable (shouldEnableSubmitBtn state)
    ][ textView
     [ color Color.white900
     , textSize FontSize.a_16
     , text (getString SUBMIT_ISSUE_DETAILS)
     , maxWidth ((screenWidth unit) - 150)
     , gravity CENTER
     ]
     , imageView
     [ width $ V 18
     , height $ V 18
     , margin (MarginLeft 12)
     , height WRAP_CONTENT
     , imageWithFallback $ fetchImage FF_ASSET "ny_ic_submit_issue_arrow"
     ]
    ]
  ]
shouldEnableSubmitBtn :: ReportIssueChatScreenState -> Boolean
shouldEnableSubmitBtn state = (not state.props.submitIsInProgress) && (STR.length (STR.trim state.data.messageToBeSent) > 0) || (isJust state.data.uploadedAudioId) || (length state.data.uploadedImagesIds > 0)

helperView :: (Action -> Effect Unit) -> ReportIssueChatScreenState -> forall w. PrestoDOM (Effect Unit) w
helperView push state =
  linearLayout
  [ height WRAP_CONTENT
  , position FIXED
  , width MATCH_PARENT
  , background Color.white900
  , visibility if state.props.showSubmitComp then GONE else VISIBLE
  , orientation VERTICAL
  , alignParentBottom "true,-1"
  ][ linearLayout
     [ width MATCH_PARENT
     , height $ V 1
     , background Color.greyLight
     ]
     []
     , textView
     [ width WRAP_CONTENT
     , layoutGravity "center"
     , text if (isJust state.data.issueId) && (isJust state.data.selectedOptionId) then (getString ISSUE_SUBMITTED_TEXT) else (getString CHOOSE_AN_OPTION)
     , textSize FontSize.a_16
     , margin (MarginVertical 18 24)
     , gravity CENTER
     ]
   ]

addAudioModel :: ReportIssueChatScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
addAudioModel state push = AddAudioModel.view (push <<< AddAudioModelAction) (addAudioModelConfig state)

addImageModel :: ReportIssueChatScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
addImageModel state push = AddImagesModel.view (push <<< AddImagesModelAction) (addImageModelConfig state)

recordAudioModel :: ReportIssueChatScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
recordAudioModel state push = RecordAudioModel.view (push <<< RecordAudioModelAction) (recordAudioModelConfig state)

viewImageModel :: ReportIssueChatScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
viewImageModel state push = ViewImageModel.view (push <<< ViewImageModelAction) (viewImageModelConfig state)

callCustomerModel :: ReportIssueChatScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
callCustomerModel state push = 
  screenAnimationFadeInOut
  $ linearLayout
   [ width MATCH_PARENT
   , height WRAP_CONTENT
   , orientation VERTICAL
   , onBackPressed push (const BackPressed)
   , cornerRadius 16.0
   , background Color.white900
   , padding (Padding 16 24 16 24)
   , margin (MarginHorizontal 16 16)
   , gravity CENTER
   ][ textView
    [ text $ getString CALL_CUSTOMER_TITLE <> "?"
    , textSize FontSize.a_20
    , fontStyle $ bold LanguageStyle
    , color Color.black800
    ]
    , textView
    [ text $ getString CALL_CUSTOMER_DESCRIPTION
    , textSize FontSize.a_16
    , margin (Margin 16 16 16 16)
    , color Color.black800
    , gravity CENTER
    ]
    , linearLayout
    [ orientation HORIZONTAL
    , width MATCH_PARENT
    , height WRAP_CONTENT
    , margin (MarginTop 16)
    ][ linearLayout
     [ weight 1.0
     , margin (MarginRight 4)
     ][ PrimaryButton.view (push <<< CancelCall) (cancelButtonConfig state)
     ]
     , linearLayout
     [ weight 1.0
     , margin (MarginLeft 4)
     ][ PrimaryButton.view (push <<< ConfirmCall) (doneButtonConfig state)
     ]
    ]
   ]
