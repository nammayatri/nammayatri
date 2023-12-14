{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyScreen.VideoRecordScreen.View where

import Prelude
import PrestoDOM
import Screens.NammaSafetyScreen.ComponentConfig

import Animation (fadeIn)
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Data.Function.Uncurried (runFn2)
import Debug (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import PrestoDOM.Animation as PrestoAnim
import Screens.NammaSafetyScreen.VideoRecordScreen.Controller (Action(..), ScreenOutput, eval, getRecordViewWidth)
import Screens.NammaSafetyScreen.View (getSafePadding, emptyTextView, getHeaderTitle)
import Screens.Types as ST
import Styles.Colors as Color
import Data.Int as DI

screen :: ST.NammaSafetyScreenState -> Screen Action ST.NammaSafetyScreenState ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "NammaSafetyVideoRecordScreen"
  , globalEvents:
      [ ( \push -> do
            if initialState.props.timerId == "" then pure unit else pure $ EHC.clearCountDownTimer "RecordingTimer"
            if initialState.props.shareTimerId == "" then pure unit else pure $ EHC.clearCountDownTimer "ShareTimer"
            pure $ pure unit
        )
      ]
  , eval:
      \action state -> do
        let
          _ = spy "NammaSafety VideoRecord action " action
        let
          _ = spy "NammaSafety VideoRecord state " state
        eval action state
  }

view ::
  forall w. (Action -> Effect Unit) -> ST.NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.black900
    , orientation VERTICAL
    , onBackPressed push $ const BackPressed
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , visibility $ boolToVisibility $ state.props.recordingState /= ST.SHARED
        ]
        [ GenericHeader.view (push <<< GenericHeaderAC)
            ( genericHeaderConfig
                ( getHeaderTitle state.props.currentStage $ isSafetyPlus state
                )
                state
            )
        ]
    , relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        ]
        [ videoRecordSOSView state push $ state.props.currentStage == ST.NammaSafetyVideoRecord && state.props.recordingState /= ST.SHARED
        , videoSharedView push state $ state.props.recordingState == ST.SHARED
        ]
    ]

videoRecordSOSView :: ST.NammaSafetyScreenState -> (Action -> Effect Unit) -> Boolean -> forall w. PrestoDOM (Effect Unit) w
videoRecordSOSView state push visibility' =
  let
    value = 15 - state.props.timerValue

    timerval = if (value < 10) then "0" <> show value else show value
  in
    PrestoAnim.animationSet [ fadeIn $ true ]
      $ relativeLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , color $ Color.white900
          , orientation VERTICAL
          , padding $ getSafePadding
          , visibility $ boolToVisibility visibility'
          , onAnimationEnd
              ( \_ -> do
                  void $ pure $ runFn2 JB.storeOnResumeCallback push OnResumeCallback
                  pure unit
              )
              (const NoAction)
          ]
          [ linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              ]
              [ linearLayout
                  [ height MATCH_PARENT
                  , width MATCH_PARENT
                  , orientation VERTICAL
                  ]
                  [ frameLayout
                      [ height MATCH_PARENT
                      , width MATCH_PARENT
                      ]
                      [ linearLayout
                          [ height MATCH_PARENT
                          , width MATCH_PARENT
                          , padding $ Padding 16 20 16 5
                          ]
                          [ linearLayout
                              [ id $ EHC.getNewIDWithTag "VideoCamView"
                              , afterRender
                                  ( \_ -> do
                                      if EHC.os == "IOS" && state.props.currentStage == ST.NammaSafetyVideoRecord then do
                                        void $ pure $ HU.requestCameraAndMicrophonePermissions unit
                                      else
                                        pure unit
                                      pure $ JB.setupCamera (EHC.getNewIDWithTag "VideoCamView") true
                                  )
                                  (const NoAction)
                              , height if EHC.os == "IOS" then V 400 else MATCH_PARENT
                              , width MATCH_PARENT
                              , orientation VERTICAL
                              ]
                              []
                          ]
                      , if state.props.recordingState == ST.SHARING && not state.props.enableLocalPoliceSupport then shareTimerView state push else emptyTextView
                      ]
                  ]
              , linearLayout
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , orientation VERTICAL
                  , visibility $ boolToVisibility $ state.props.recordingState /= ST.UPLOADING
                  ]
                  [ frameLayout
                      [ height $ V 4
                      , width MATCH_PARENT
                      , margin $ MarginHorizontal 16 16
                      , visibility $ boolToVisibility $ state.props.recordingState /= ST.SHARING
                      ]
                      [ linearLayout
                          [ height $ V 4
                          , width MATCH_PARENT
                          , background Color.white900
                          , id $ EHC.getNewIDWithTag "recordProgress"
                          , gravity LEFT
                          ]
                          []
                      , linearLayout
                          [ height $ V 4
                          , width $ V $ DI.ceil $ ((getRecordViewWidth "" / 15.0) * (DI.toNumber $ 15 - state.props.timerValue))
                          , background Color.blue800
                          ]
                          []
                      ]
                  , linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , padding $ PaddingHorizontal 16 16
                      , visibility $ boolToVisibility $ state.props.recordingState /= ST.SHARING
                      ]
                      [ textView
                          [ text $ "0:" <> timerval
                          , weight 1.0
                          , color Color.white900
                          ]
                      , textView
                          [ text "0:15"
                          , color Color.white900
                          ]
                      ]
                  , textView
                      $ [ text $ getString if state.props.enableLocalPoliceSupport then VIDEO_SHARE_INFO_TO_POLICE else THE_VIDEO_WILL_BE_RECORDED
                        , color "#B9BABE"
                        , padding $ PaddingHorizontal 16 16
                        , margin $ MarginTop 12
                        ]
                      <> FontStyle.body3 TypoGraphy
                  ]
              ]
          , linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              , gravity CENTER
              , alignParentBottom "true,-1"
              , margin $ MarginBottom 16
              , onClick
                  ( \action -> do
                      when (state.props.recordingState /= ST.RECORDING) $ 
                        do
                          void $ JB.startRecord push VideoStatusCallBack
                          _ <- pure $ spy "startRecord" state.props.timerValue
                          if (EHC.os == "IOS") then
                            JB.startTimerWithTime (show state.props.timerValue) "RecordingTimer" "1" push CountDown
                          else
                            EHC.countDown state.props.timerValue "RecordingTimer" push CountDown
                      push action
                  )
                  (const ToggleRecord)
              ]
              [ imageView
                  [ imageWithFallback
                      $ HU.fetchImage HU.FF_ASSET case state.props.recordingState, state.props.enableLocalPoliceSupport of
                          ST.RECORDING, _ -> "ny_ic_stop_record"
                          ST.SHARING, false -> "ny_ic_cancel_share"
                          _, _ -> "ny_ic_start_record"
                  , height $ V 45
                  , width $ V 45
                  , visibility $ boolToVisibility $ state.props.recordingState /= ST.UPLOADING
                  ]
              , textView
                  $ [ text
                        $ getString case state.props.recordingState, state.props.enableLocalPoliceSupport of
                            ST.RECORDING, _ -> STOP_AND_SHARE_RECORDING
                            ST.SHARING, false -> CANCEL_SHARING
                            _, _ -> START_RECORDING
                    , color Color.white900
                    , visibility $ boolToVisibility $ state.props.recordingState /= ST.UPLOADING
                    ]
                  <> FontStyle.tags TypoGraphy
              ]
          ]

shareTimerView :: ST.NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
shareTimerView state push =
  PrestoAnim.animationSet [ fadeIn $ true ]
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        , orientation VERTICAL
        , padding $ Padding 16 20 16 5
        , visibility $ boolToVisibility $ state.props.recordingState == ST.SHARING
        , background Color.blackLessTrans
        , onAnimationEnd
            ( \action -> do
                if (EHC.os == "IOS") then
                  JB.startTimerWithTime (show state.props.shareTimerValue) "ShareTimer" "1" push CountDownShare
                else do
                  _ <- pure $ spy "countDown" state.props.shareTimerValue
                  EHC.countDown state.props.shareTimerValue "ShareTimer" push CountDownShare
                push action

            )
            (const NoAction)
        ]
        [ textView
            $ [ text $ getString SHARING_THE_VIDEO_IN
              , height WRAP_CONTENT
              , width MATCH_PARENT
              , gravity CENTER
              , color Color.white900
              ]
            <> FontStyle.paragraphText TypoGraphy
        , textView
            $ [ text $ show state.props.shareTimerValue
              , height WRAP_CONTENT
              , width MATCH_PARENT
              , color Color.white900
              , gravity CENTER
              ]
            <> FontStyle.priceFont TypoGraphy
        ]

videoSharedView :: forall w. (Action -> Effect Unit) -> ST.NammaSafetyScreenState -> Boolean -> PrestoDOM (Effect Unit) w
videoSharedView push state visibility' =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , visibility $ boolToVisibility visibility'
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , gravity CENTER
        , padding $ PaddingTop 100
        ]
        [ imageView
            [ imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_video_shared"
            , height $ V 300
            , padding $ PaddingHorizontal 16 16
            ]
        , textView
            $ [ text $ getString EMERGENCY_INFO_SHARED
              , gravity CENTER
              , color Color.white900
              ]
            <> FontStyle.h1 TypoGraphy
        , textView
            $ [ text
                  $ getString
                      if state.props.enableLocalPoliceSupport then
                        EMERGENCY_INFO_SHARED_ACTION_POLICE
                      else
                        EMERGENCY_INFO_SHARED_ACTION
              , padding $ PaddingHorizontal 20 20
              , gravity CENTER
              , color Color.white900
              , margin $ MarginTop 8
              ]
            <> FontStyle.body1 TypoGraphy
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , alignParentBottom "true,-1"
        , margin $ MarginTop 50
        ]
        [ PrimaryButton.view (push <<< VideoShared) (goBackBtnConfig state) ]
    ]
