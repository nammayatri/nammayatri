{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReportIssueChatScreen.View where

import Data.Maybe

import Animation (screenAnimation, screenAnimationFadeInOut)
import Common.Types.App (LazyCheck(..))
import Components.AddAudioModel (view) as AddAudioModel
import Components.AddImagesModel (view) as AddImagesModel
import Components.ChatView as ChatView
import Components.PrimaryButton.View (view) as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.RecordAudioModel.View (view) as RecordAudioModel
import Components.ViewImageModel.View (view) as ViewImageModel
import Data.Array (length, null)
import Data.String (length, trim) as STR
import Effect (Effect)
import Effect.Uncurried (runEffectFn2)
import Engineering.Helpers.Commons as EHC
import Font.Size (a_16, a_20, a_14, a_17, a_18) as FontSize
import Font.Style (bold)
import Font.Style (h3, semiBold) as FontStyle
import JBridge (storeCallBackImageUpload, storeCallBackUploadMultiPartData, storeKeyBoardCallback) as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, pure, show, unit, ($), (-), (<<<), (<>), (>), (||), (==), not)
import PrestoDOM (linearLayout, frameLayout, imageView, relativeLayout, textView)
import PrestoDOM.Events (afterRender, onBackPressed, onClick)
import PrestoDOM.Properties (adjustViewWithKeyboard, alignParentBottom, alpha, background, color, cornerRadii, cornerRadius, fontStyle, gravity, height, imageUrl, imageWithFallback, layoutGravity, lineHeight, margin, maxWidth, orientation, padding, position, stroke, text, textSize, visibility, weight, width)
import PrestoDOM.Types.Core (Corners(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Position(..), PrestoDOM, Screen, Visibility(..))
import Screens.ReportIssueChatScreen.ComponentConfig (cancelButtonConfig, doneButtonConfig, primaryEditTextConfig, viewImageModelConfig, addImageModelConfig, recordAudioModelConfig, addAudioModelConfig, chatConfig)
import Screens.ReportIssueChatScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (ReportIssueChatScreenState)
import Styles.Colors (grey700, black700, black800, black900, black9000, blue900, blueBtn, blueTextColor, grey900, greyLight, lightGreyBlue, white900) as Color

screen :: ReportIssueChatScreenState -> Screen Action ReportIssueChatScreenState ScreenOutput
screen initialState =
    { initialState
    , view
    , name : "ReportIssueChatScreen"
    , globalEvents : [(\push -> do
        _ <- JB.storeCallBackImageUpload push ImageUploadCallback
        _ <- JB.storeCallBackUploadMultiPartData push UploadMultiPartDataCallback
        _ <- runEffectFn2 JB.storeKeyBoardCallback push KeyboardCallback
        _ <- push ShowOptions
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
      , padding $ Padding 0 EHC.safeMarginTop 0 0
      ] ([ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , adjustViewWithKeyboard "true"
          ][ linearLayout
           [ 
           width MATCH_PARENT
           , orientation VERTICAL
          --  , adjustViewWithKeyboard "true"
           , weight 1.0
           ][ headerLayout state push
            , issueIdHeader state push
            , chatView push state
           ]
          , linearLayout
              [ height WRAP_CONTENT
              -- , position FIXED
              , width MATCH_PARENT
              , background Color.grey700
              , orientation VERTICAL
              -- , alignParentBottom "true,-1"
              -- , adjustViewWithKeyboard "false"
              -- , gravity BOTTOM
              ][ submitView push state
                , helperView push state
              ]
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
           <> if state.props.showCallDriverModel || state.props.showCallSupportModel then [ callModel state push ] else []
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
            [ width $ V 30
            , height $ V 30
            , imageUrl "ny_ic_chevron_left"
            , onClick push $ const BackPressed
            , padding $ Padding 2 2 2 2
            , margin $ MarginLeft 5
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text state.data.categoryName
              , textSize FontSize.a_18
              , margin $ MarginLeft 20
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
     , text ((getString ISSUE_NO)<> ": " <> fromMaybe "" state.data.issueId)
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
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , afterRender push (
        pure ShowOptions
    )
  , background Color.white900
  , orientation VERTICAL
  ][ ChatView.view (push <<< ChatViewActionController) (chatConfig state)
  ]

submitView :: (Action -> Effect Unit) -> ReportIssueChatScreenState -> forall w. PrestoDOM (Effect Unit) w
submitView push state =
  linearLayout
  [ height WRAP_CONTENT
  -- , position FIXED
  , width MATCH_PARENT
  -- , adjustViewWithKeyboard "true"
  , stroke ("1,"<>Color.grey900)
  , visibility if state.props.showSubmitComp then VISIBLE else GONE
  , padding (Padding 20 (if EHC.os == "IOS" then 20 else 0 ) 20 20)
  , background Color.grey700
  , cornerRadii (Corners 20.0 true true false false)
  , orientation VERTICAL
  -- , alignParentBottom "true,-1"
  ][ PrimaryEditText.view (push <<< PrimaryEditTextActionController) (primaryEditTextConfig state)
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
     , visibility if state.props.isKeyboardOpen then GONE else VISIBLE
     ][ linearLayout
      [ weight 1.0
      , width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER
      , stroke ("1,"<>Color.grey900)
      , cornerRadius 25.0
      , onClick push (const AddImage)
      , margin (MarginRight 12)
      , background Color.white900
      , padding (Padding 8 8 8 8)
      ][ imageView
       [ width $ V 36
       , height $ V 36
       , margin (MarginRight 8)
       , imageWithFallback "ny_ic_add_image,https://assets.juspay.in/nammayatri/images/driver/ny_ic_add_image"
       ]
       , textView
       [ weight 1.0
       , height WRAP_CONTENT
       , text if (length state.data.addedImages > 0) then (show $ length state.data.addedImages) <> " " <> (if length state.data.addedImages > 1 then (getString IMAGES) else (getString IMAGE)) else (getString ADD_IMAGE)
       , color if (length state.data.addedImages > 0) then Color.blue900 else Color.black900
       , textSize FontSize.a_14
       ]
       , imageView
       [ width $ V 24
       , height $ V 24
       , margin (MarginRight 6)
       , imageWithFallback "ny_ic_close_bold,https://assets.juspay.in/nammayatri/images/driver/ny_ic_close_bold"
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
      , cornerRadius 25.0
      , onClick push (const $ AddAudio false)
      , background Color.white900
      , padding (Padding 8 8 8 8)
      ][ imageView
       [ width $ V 36
       , height $ V 36
       , margin (MarginRight 8)
       , imageWithFallback "ny_ic_add_audio,https://assets.juspay.in/nammayatri/images/driver/ny_ic_add_audio"
       ]
       , textView
       [ weight 1.0
       , height WRAP_CONTENT
       , text if (isJust state.data.recordedAudioUrl) then (getString VOICE_NOTE) else (getString ADD_VOICE_NOTE)
       , color if (isJust state.data.recordedAudioUrl) then Color.blue900 else Color.black900
       , textSize FontSize.a_14
       ]
       , imageView
       [ width $ V 24
       , height $ V 24
       , margin (MarginRight 6)
       , imageWithFallback "ny_ic_close_bold,https://assets.juspay.in/nammayatri/images/driver/ny_ic_close_bold"
       , visibility if (isJust state.data.recordedAudioUrl) then VISIBLE else GONE
       , onClick push (const DeleteRecordedAudio)
       ]
      ]
     ]
    ]
    , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.blueBtn
    , cornerRadius 26.0
    , padding (Padding 0 16 0 16)
    , orientation HORIZONTAL
    , alpha if (STR.length (STR.trim state.data.messageToBeSent) > 0) || (isJust state.data.uploadedAudioId) || (length state.data.uploadedImagesIds > 0) then 1.0 else 0.5
    , onClick push (if (STR.length (STR.trim state.data.messageToBeSent) > 0) || (isJust state.data.uploadedAudioId) || (length state.data.uploadedImagesIds > 0) then (const SubmitIssue) else (const NoAction))
    , gravity CENTER
    ][ textView
     [ color Color.white900
     , textSize FontSize.a_16
     , text (getString SUBMIT_ISSUE_DETAILS)
     , maxWidth ((EHC.screenWidth unit) - 150)
     , gravity CENTER
     ]
     , imageView
     [ width $ V 18
     , height $ V 18
     , margin (MarginLeft 12)
     , height WRAP_CONTENT
     , imageWithFallback "ny_ic_submit_issue_arrow,https://assets.juspay.in/nammayatri/images/driver/ny_ic_submit_issue_arrow"
     ]
    ]
  ]

helperView :: (Action -> Effect Unit) -> ReportIssueChatScreenState -> forall w. PrestoDOM (Effect Unit) w
helperView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.grey700
  , visibility if state.props.showSubmitComp then GONE else VISIBLE
  , orientation VERTICAL
  ][ linearLayout
     [ width MATCH_PARENT
     , height $ V 1
     , background Color.greyLight
     ]
     []
     , textView[ 
        width MATCH_PARENT
      , layoutGravity "center"
      , color Color.black700
      , visibility if (null state.data.options) then GONE else VISIBLE
      , text (getString CHOOSE_AN_OPTION)
      , textSize FontSize.a_16
      , margin (MarginVertical 18 24)
      , gravity CENTER
      ]
     , linearLayout[
      width MATCH_PARENT
     , gravity CENTER
     , margin (MarginVertical 18 24)
     , visibility if (null state.data.options) then VISIBLE else GONE
     ][
      linearLayout[
      width MATCH_PARENT
      , gravity CENTER
      , visibility if state.data.showStillHaveIssue then VISIBLE else GONE
      ][
        textView[ 
          width WRAP_CONTENT
        , layoutGravity "center"
        , color Color.black700
        , text (getString ISSUE_MARKED_AS_RESOLVED)
        , textSize FontSize.a_16
        , gravity CENTER
        , margin (MarginRight 2)
        ],
        textView[ 
          width WRAP_CONTENT
        , layoutGravity "center"
        , color Color.blue900
        , text (getString STILL_HAVING_ISSUE)
        , textSize FontSize.a_16
        , gravity CENTER
        , onClick push $ const ReopenIssuePress
        ]
      ]
      , textView
      [ width MATCH_PARENT
      , layoutGravity "center"
      , color Color.black700
      , visibility if state.data.showStillHaveIssue then GONE else VISIBLE
      , text if state.data.isResolved then getString ISSUE_RESOLVED_TEXT else getString ISSUE_SUBMITTED_TEXT
      , textSize FontSize.a_16
      , gravity CENTER
      ]
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

callModel :: ReportIssueChatScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
callModel state push = 
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
    [ text $ getString (if state.props.showCallDriverModel then CALL_DRIVER_TITLE else CALL_SUPPORT_TITLE)
    , textSize FontSize.a_20
    , fontStyle $ bold LanguageStyle
    , color Color.black800
    ]
    , textView
    [ text $ getString (if state.props.showCallDriverModel then CALL_DRIVER_DESCRIPTION else CALL_SUPPORT_DESCRIPTION)
    , textSize FontSize.a_16
    , margin (Margin 16 16 16 16)
    , color Color.black800
    , gravity CENTER
    ]
    , linearLayout
    [ orientation VERTICAL
    , height MATCH_PARENT
    , width MATCH_PARENT
    , margin (MarginTop 16)
    , gravity CENTER
    ][
       linearLayout
     [ weight 1.0
     , margin (MarginBottom 4)
     , height MATCH_PARENT
     , width MATCH_PARENT
     ][ PrimaryButton.view (push <<< ConfirmCall) (doneButtonConfig state)]
      , linearLayout
     [ weight 1.0
     , height MATCH_PARENT
     , width MATCH_PARENT
     ][ PrimaryButton.view (push <<< CancelCall) (cancelButtonConfig state)]
    ]
   ]
