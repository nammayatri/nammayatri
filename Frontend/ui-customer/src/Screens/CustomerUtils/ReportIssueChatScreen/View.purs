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
import Data.Array (length, null, all)
import Data.Array.ST (push)
import Data.Function.Uncurried (runFn1)
import Data.String (length, trim) as STR
import Debug (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn2)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Commons as EHC
import Font.Size (a_16, a_20, a_14, a_17, a_18) as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge (getLayoutBounds)
import JBridge (storeCallBackImageUpload, storeCallBackUploadMultiPartData, storeKeyBoardCallback) as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Prelude (Unit, bind, const, discard, not, pure, show, unit, void, when, ($), (-), (<<<), (<>), (==), (>), (||), (&&), (/=), map, (>=))
import PrestoDOM (Margin(..), background, frameLayout, imageView, linearLayout, relativeLayout, textView, rippleColor, accessibility, Accessiblity(..), accessibilityHint)
import PrestoDOM.Events (afterRender, onBackPressed, onClick)
import PrestoDOM.Properties (adjustViewWithKeyboard, alignParentBottom, alpha, background, color, cornerRadii, cornerRadius, fontStyle, gravity, height, imageUrl, imageWithFallback, layoutGravity, lineHeight, margin, maxWidth, orientation, padding, position, stroke, text, textSize, visibility, weight, width, id)
import PrestoDOM.Types.Core (Corners(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Position(..), PrestoDOM, Screen, Visibility(..))
import Screens.ReportIssueChatScreen.ComponentConfig (cancelButtonConfig, doneButtonConfig, primaryEditTextConfig, viewImageModelConfig, addImageModelConfig, recordAudioModelConfig, addAudioModelConfig, chatConfig)
import Screens.ReportIssueChatScreen.Controller (Action(..), ScreenOutput, eval, isFileTypeRequired)
import Styles.Colors as Color
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Screens.ReportIssueChatScreen.ScreenData (ReportIssueChatScreenState, ReportIssueChatScreenEntryPoint(..))
import Data.Tuple as DT
import Services.API as SA

screen :: ReportIssueChatScreenState -> Screen Action ReportIssueChatScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "ReportIssueChatScreen"
  , globalEvents: [(\push ->  globalEvents' push)]
  , eval:
      \action state -> do
        let
          _ = spy "ReportIssueChatScreen action " action
        let
          _ = spy "ReportIssueChatScreen state " state
        eval action state
  }
  where
  globalEvents' :: (Action -> Effect Unit) -> Effect (Effect Unit)
  globalEvents' push = do 
    void $ push ShowOptions
    when (not initialState.props.initalizedCallbacks) $ do
        void $ JB.storeCallBackImageUpload push ImageUploadCallback
        void $ runEffectFn2 JB.storeCallBackUploadMultiPartData push UploadMultiPartDataCallback
        void $ runEffectFn2 JB.storeKeyBoardCallback push KeyboardCallback
        void $ push InitializeCallback 
        pure unit
    pure $ pure unit
    

view :: forall w. (Action -> Effect Unit) -> ReportIssueChatScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimation
    $ frameLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , onBackPressed push $ const BackPressed
        , afterRender push $ const AfterRender
        , padding $ PaddingTop EHC.safeMarginTop 
        ]
        ( [ linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , adjustViewWithKeyboard "true"
              , background Color.white900
              ]
              [ linearLayout
                  [ width MATCH_PARENT
                  , orientation VERTICAL
                  , weight 1.0
                  ]
                  [ headerLayout state push
                  , issueIdHeader state push
                  , chatView push state
                  ]
              , linearLayout
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , background Color.white900 
                  , orientation VERTICAL
                  ]
                  [ submitView push state
                  , helperView push state
                  ]
              ]
          , linearLayout
              [ width MATCH_PARENT
              , height MATCH_PARENT
              , background Color.black9000
              , onClick push $ const NoAction
              , gravity CENTER
              , visibility $ boolToVisibility $ state.props.isPopupModelOpen 
              ]
              ( if state.props.showAudioModel then [ addAudioModel state push ] else[]
                    <> if state.props.showImageModel then [ addImageModel state push ] else []
                    <> if state.props.showRecordModel then [ recordAudioModel state push ] else []
                    <> if state.props.showCallDriverModel || state.props.showCallSupportModel then [ callModel state push ] else []
              )
          ]
            <> if state.props.showViewImageModel then [ viewImageModel state push ] else []
        )

-------------------------------------------------- headerLayout --------------------------
headerLayout :: ReportIssueChatScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
headerLayout state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.white900
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
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
            , onClick push $ const BackPressed
            , padding $ Padding 7 7 7 7
            , rippleColor Color.rippleShade
            , cornerRadius 20.0
            , accessibility ENABLE
            , accessibilityHint "Back : Button"
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ getString REPORT_AN_ISSUE
              , margin $ MarginLeft 5
              , weight 1.0
              , color Color.black900
              ]
            <> FontStyle.h3 TypoGraphy
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , visibility $ boolToVisibility $ isJust state.data.selectedRide && state.data.entryPoint /= TripDetailsScreenEntry
              , text $ getString RIDE_DETAILS
              , weight 1.0
              , color Color.blue900
              , gravity RIGHT
              , margin $ MarginRight 12
              , onClick push $ const GoToRideDetails
            ]
            <> FontStyle.body6 TypoGraphy
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.greyLight
        ]
        []
    ]

issueIdHeader :: ReportIssueChatScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
issueIdHeader state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , padding $ Padding 15 10 10 10
    , background Color.lightGreyBlue
    , visibility $ boolToVisibility $ isJust state.data.issueId
    ]
    [ textView $ 
        [ width WRAP_CONTENT
        , height MATCH_PARENT
        , text $ (getString ISSUE_NO) <> ": " <> fromMaybe (fromMaybe "" state.data.issueId) state.data.issueReportShortId
        , gravity CENTER_VERTICAL
        , color Color.black800
        ] <> FontStyle.body2 TypoGraphy
    , linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        ]
        [ textView $ 
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , gravity RIGHT
            , color Color.blueTextColor
            ] <> FontStyle.h3 TypoGraphy 
        ]
    ]

chatView :: forall w. (Action -> Effect Unit) -> ReportIssueChatScreenState -> PrestoDOM (Effect Unit) w
chatView push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    ]
    [ ChatView.view (push <<< ChatViewActionController) (chatConfig state)
    ]

submitView :: (Action -> Effect Unit) -> ReportIssueChatScreenState -> forall w. PrestoDOM (Effect Unit) w
submitView push state =
  let checkUploadRequirements (SA.MandatoryUploads upload) = do
        case upload.fileType of
          SA.Image -> length state.data.uploadedImagesIds >= upload.limit
          SA.Audio -> (maybe 0 STR.length state.data.uploadedAudioId) >= upload.limit
          _ -> false

      enableSubmit = do
        case state.data.mandatoryUploads of
          Nothing -> (STR.length (STR.trim state.data.messageToBeSent)) > 10 || (isJust state.data.uploadedAudioId) || (length state.data.uploadedImagesIds) > 0
          Just uploads -> all checkUploadRequirements uploads
  in 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , stroke $ "1," <> Color.grey900
    , visibility $ boolToVisibility $ state.props.showSubmitComp
    , padding $ Padding 20 (if EHC.os == "IOS" then 20 else 0) 20 20
    , background Color.grey700
    , cornerRadii $ Corners 20.0 true true false false
    , orientation VERTICAL
    ]
    [ PrimaryEditText.view (push <<< PrimaryEditTextActionController) (primaryEditTextConfig state)
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , orientation HORIZONTAL
        , margin $ MarginVertical 16 16
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , visibility $ boolToVisibility $ not state.props.isKeyboardOpen
            ]
            [ linearLayout
                [ weight 1.0
                , width WRAP_CONTENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                , gravity CENTER
                , stroke $ "1," <> Color.grey900
                , cornerRadius 25.0
                , onClick push $ const AddImage
                , margin $ MarginRight 12
                , background Color.white900
                , padding $ Padding 8 8 8 8
                ]
                [ imageView
                    [ width $ V 36
                    , height $ V 36
                    , margin (MarginRight 8)
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET  "ny_ic_add_image"
                    ]
                , textView $ 
                    [ height WRAP_CONTENT
                    , text if not (null state.data.addedImages)  then (show $ length state.data.addedImages) <> " " <> (if length state.data.addedImages > 1 then (getString IMAGES) else (getString IMAGE)) else (getString ADD_IMAGE_S)
                    , color if not (null state.data.addedImages) then Color.blue900 else Color.black900
                    ] <> FontStyle.body1 TypoGraphy
                , helperStarView SA.Image state
                , imageView
                    [ width $ V 24
                    , height $ V 24
                    , margin $ MarginRight 6
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close_bold"
                    , visibility $ boolToVisibility $ not (null state.data.addedImages)
                    , onClick push $ const DeleteSelectedImages
                    ]
                ]
            , linearLayout
                [ weight 1.0
                , width WRAP_CONTENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                , gravity CENTER
                , stroke $ "1," <> Color.grey900
                , cornerRadius 25.0
                , onClick push $ const $ AddAudio false
                , background Color.white900
                , padding $ Padding 8 8 8 8
                ]
                [ imageView
                    [ width $ V 36
                    , height $ V 36
                    , margin $ MarginRight 8
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_add_audio"
                    ]
                , textView $ 
                    [ height WRAP_CONTENT
                    , text if isJust state.data.recordedAudioUrl then (getString VOICE_NOTE) else (getString ADD_VOICE_NOTE)
                    , color if isJust state.data.recordedAudioUrl then Color.blue900 else Color.black900
                    ] <> FontStyle.body1 TypoGraphy
                , helperStarView SA.Audio state
                , imageView
                    [ width $ V 24
                    , height $ V 24
                    , margin $ MarginRight 6
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close_bold"
                    , visibility $ boolToVisibility $ isJust state.data.recordedAudioUrl
                    , onClick push $ const DeleteRecordedAudio
                    ]
                ]
            ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background $ state.data.config.submitIssueBtnColor
        , cornerRadius 26.0
        , padding $ PaddingVertical 16 16
        , orientation HORIZONTAL
        , alpha if enableSubmit then 1.0 else 0.5
        , onClick push if enableSubmit then (const SubmitIssue) else (const NoAction)
        , gravity CENTER
        ]
        [ textView $ 
            [ color Color.white900
            , text $ getString SUBMIT_ISSUE_DETAILS
            , maxWidth $ (EHC.screenWidth unit) - 150
            , gravity CENTER
            ] <> FontStyle.body5 TypoGraphy
        , imageView
            [ width $ V 18
            , height $ V 18
            , margin $ MarginLeft 12
            , height WRAP_CONTENT
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_submit_issue_arrow"
            ]
        ]
    ]


helperView :: (Action -> Effect Unit) -> ReportIssueChatScreenState -> forall w. PrestoDOM (Effect Unit) w
helperView push state =
  PrestoAnim.animationSet [Anim.fadeIn true ] $ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.grey700
    , visibility $ boolToVisibility $ not state.props.showSubmitComp
    , orientation VERTICAL
    , padding $ PaddingVertical (if EHC.os == "IOS" then 24 else 18 ) 24
    ][
      textView $ 
        [ width MATCH_PARENT
        , layoutGravity "center"
        , color Color.black700
        , visibility $ boolToVisibility $ not $ null state.data.options
        , text $ getString CHOOSE_AN_OPTION
        , gravity CENTER
        ] <> FontStyle.body5 TypoGraphy
  , linearLayout
      [ width MATCH_PARENT
      , gravity CENTER
      , visibility $ boolToVisibility $ null state.data.options 
      ][
        linearLayout
          [ width MATCH_PARENT
          , gravity CENTER
          , visibility $ boolToVisibility state.data.showStillHaveIssue
          ][ textView $ 
              [ width WRAP_CONTENT
              , layoutGravity "center"
              , color Color.black700
              , text $ getString ISSUE_MARKED_AS_RESOLVED
              , gravity CENTER
              , margin $ MarginRight 2
              ] <> FontStyle.body5 TypoGraphy
          , textView $
              [ width WRAP_CONTENT
              , layoutGravity "center"
              , color Color.blue900
              , text $ getString STILL_HAVING_ISSUE
              , gravity CENTER
              , onClick push $ const ReopenIssuePress
              ] <> FontStyle.body5 TypoGraphy
          ]
      , textView $ 
          [ width MATCH_PARENT
          , layoutGravity "center"
          , color Color.black700
          , visibility $ boolToVisibility $ not state.data.showStillHaveIssue
          , text $ getString 
              if state.props.isResolved then 
                ISSUE_RESOLVED_TEXT 
              else if state.props.showEndFlowMessage then 
                WE_HOPE_THE_ISSUE_IS_RESOLVED "WE_HOPE_THE_ISSUE_IS_RESOLVED" 
              else 
                ISSUE_SUBMITTED_TEXT
          , gravity CENTER
          ] <> FontStyle.paragraphText TypoGraphy
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
        , onBackPressed push $ const BackPressed
        , cornerRadius 16.0
        , background Color.white900
        , padding $ Padding 16 24 16 24
        , margin $ MarginHorizontal 16 16
        , gravity CENTER
        ]
        [ textView $ 
            [ text $ getString if state.props.showCallDriverModel then CALL_DRIVER_TITLE else CALL_SUPPORT_TITLE
            , color Color.black800
            ] <> FontStyle.body8 TypoGraphy
        , textView $ 
            [ text $ getString $ if state.props.showCallDriverModel then CALL_DRIVER_DESCRIPTION else CALL_SUPPORT_DESCRIPTION "CALL_SUPPORT_DESCRIPTION"
            , margin $ Margin 16 16 16 16
            , color Color.black800
            , gravity CENTER
            ] <> FontStyle.body5 TypoGraphy
        , linearLayout
            [ orientation VERTICAL
            , height MATCH_PARENT
            , width MATCH_PARENT
            , margin $ MarginTop 16
            , gravity CENTER
            ]
            [ linearLayout
                [ weight 1.0
                , margin $ MarginBottom 4
                , height MATCH_PARENT
                , width MATCH_PARENT
                ]
                [ PrimaryButton.view (push <<< ConfirmCall) (doneButtonConfig state) ]
            , linearLayout
                [ weight 1.0
                , height MATCH_PARENT
                , width MATCH_PARENT
                ]
                [ PrimaryButton.view (push <<< CancelCall) (cancelButtonConfig state) ]
            ]
        ]

helperStarView :: SA.MediaType -> ReportIssueChatScreenState -> forall w. PrestoDOM (Effect Unit) w
helperStarView fileType state =
  textView $
    [ height WRAP_CONTENT
    , text $ if isFileTypeRequired fileType state.data.mandatoryUploads then "*" else ""
    , color Color.red900
    ]