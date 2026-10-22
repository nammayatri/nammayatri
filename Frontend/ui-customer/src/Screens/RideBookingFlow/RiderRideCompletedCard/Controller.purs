module Screens.RideBookingFlow.RiderRideCompletedCard.Controller where

import Prelude
import Prim.TypeError as String
import Screens.Types (RiderRideCompletedScreenState)
import Components.RideCompletedCard.Controller ( CustomerIssueCard(..))
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit, updateWithCmdAndExit)
import PrestoDOM.Types.Core (class Loggable, defaultPerformLog)
import Common.Types.App
import Data.Array (filter, any, length, elem, (!!), find, findIndex, updateAt)
import Effect.Uncurried (runEffectFn1, runEffectFn7, runEffectFn3, runEffectFn5)
import JBridge as JB
import Timers(clearTimerWithId, waitingCountdownTimerV2)
import Components.RecordAudioModel as RecordAudioModel
import Effect (Effect)
import Data.Maybe
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Commons (getNewIDWithTag, flowRunner)
import Components.PrimaryButton as PrimaryButton
import Types.EndPoint as EndPoint
import Data.Function.Uncurried (runFn1, runFn2, runFn3)
import PrestoDOM.Core (getPushFn)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Debug (spy)
import Engineering.Helpers.Commons (os)
import Data.Function.Uncurried (runFn3, runFn4)
import Timers (timeInMinFormat, startTimer, timeStringToSeconds)
import Components.FavouriteDriverInfoCard.Controller as FavouriteDriverInfoCard
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM.List (ListItem)
import Components.BannerCarousel as BannerCarousel
import Components.RideCompletedCard as RideCompletedCard
import Common.Types.App as CTP
import Data.Int (fromString)
import Data.Array as DA
import Common.Types.App (CustomerIssueTypes(..))
import Resources.LocalizableV2.Strings (getStringV2)
import Resources.LocalizableV2.Types
import Helpers.Utils (isParentView,emitTerminateApp)
import Effect.Aff (launchAff)
import Services.Backend as Remote
import Services.API as API
import Services.Backend as Remote
import Effect.Aff
import Types.App
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Services.Config

data Action =
              RideDetails
            | RateClick Int
            | HelpAndSupportAC
            | Skip
            | GoToSOS
            | Back
            | SelectPill String String
            | MakeFavourite
            | NoAction
            | OnClickRecord (Action -> Effect Unit)
            | OnClickStop
            | OnClickPlay
            | UpdateRecordModelPlayer String
            | TimerCallback String String Int
            | UpdateState RiderRideCompletedScreenState
            | PrimaryButtonAC PrimaryButton.Action
            | UploadMultiPartDataCallback  String String
            | OnClickDone
            | OnClickClose
            | OnClickPause
            | FeedbackChanged String String
            | OnAudioCompleted String
            | GoToDriverProfile
            | CountDown Int String String
            | DriverInfoCard
            | DriverInfocardAC FavouriteDriverInfoCard.Action
            | SelectButton Boolean Int
            | BannerChanged String
            | BannerMoving String
            | SetBannerItem ListItem
            | BannerCarousel BannerCarousel.Action
            | SetIssueReportBannerItems ListItem
            | RideCompletedAC RideCompletedCard.Action
            | PrimaryButtonCarousel PrimaryButton.Action
            | PrimaryBtnRentalTripDetailsAC PrimaryButton.Action
            | KeyboardCallback String

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog = defaultPerformLog

data ScreenOutput = NoOutput RiderRideCompletedScreenState
                  | RideDetailsScreen RiderRideCompletedScreenState
                  | GoToHelpAndSupport RiderRideCompletedScreenState
                  | HomeScreen RiderRideCompletedScreenState
                  | GoToNammaSafety RiderRideCompletedScreenState Boolean Boolean
                  | SubmitRating RiderRideCompletedScreenState String
                  | GoToDriversProfile RiderRideCompletedScreenState
                  | GoToIssueReportChatScreenWithIssue RiderRideCompletedScreenState CTP.CustomerIssueTypes
                  | DriverInfoComponent


data RentalRowView = RideTime | RideDistance | RideStartedAt | RideEndedAt | EstimatedFare | ExtraTimeFare | ExtraDistanceFare | TotalFare | Surcharges

derive instance genericRentalRowView :: Generic RentalRowView _
instance eqRentalRowView :: Eq RentalRowView where eq = genericEq

type RentalTextConfig = {
  title :: String,
  subTitle :: String,
  estimatedValue :: String,
  actualValue :: String,
  color :: String
}

eval :: Action -> RiderRideCompletedScreenState -> Eval Action ScreenOutput RiderRideCompletedScreenState

eval (SetBannerItem bannerItem) state = continue state{customerIssue{bannerComputedView = Just bannerItem}}

eval (PrimaryButtonAC PrimaryButton.OnClick) state = do
  let audio = if state.ratingCard.recordAudioState.recordedAudioUrl == Nothing then "" else JB.convertAudioToBase64 (fromMaybe "" state.ratingCard.recordAudioState.recordedAudioUrl)
  updateAndExit state $ SubmitRating state audio

eval (PrimaryButtonCarousel PrimaryButton.OnClick) state = do
  let
    negativeResp = filter (\issueResp -> ((issueResp.selectedYes == Just false && issueResp.issueType /= DemandExtraTollAmount) || (issueResp.selectedYes == Just true && issueResp.issueType == DemandExtraTollAmount))) state.customerIssue.customerResponse

    hasAssistanceIssue = any (\issueResp -> issueResp.issueType == CTP.Accessibility) negativeResp
    hasSafetyIssue = any (\issueResp -> issueResp.issueType == CTP.NightSafety) negativeResp
    demandExtraTollAmountIssue = any (\issueResp -> issueResp.issueType == CTP.DemandExtraTollAmount) negativeResp

    priorityIssue = case hasSafetyIssue, demandExtraTollAmountIssue of
      true , _ -> CTP.NightSafety
      _ , true -> CTP.DemandExtraTollAmount
      _ , _ -> CTP.NoIssue

    ratingUpdatedState = state {
      customerIssue {
        showIssueBanners = false
      }
    , ratingViewState{
      wasOfferedAssistance = Just $ not hasAssistanceIssue,
      nightSafety = Just $ not hasSafetyIssue
    }
    }

  if demandExtraTollAmountIssue && (not hasSafetyIssue) then
    continueWithCmd ratingUpdatedState [ do
      void $ reportTollIssue "lazy"
      pure $ NoAction
    ]
  else if priorityIssue == CTP.NoIssue then do
    continue state {customerIssue {showIssueBanners = false}, ratingViewState{ nightSafety = Just true }}
  else
    updateWithCmdAndExit ratingUpdatedState  [do
      if demandExtraTollAmountIssue then void $ reportTollIssue "lazy" else pure unit
      pure NoAction
    ] $ GoToIssueReportChatScreenWithIssue ratingUpdatedState priorityIssue

  where
    reportTollIssue lazy = do
      void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
        let
          tollConfig = (getQuickReportIssueConfig "lazy").tollIssue
          postIssueBody = API.PostIssueReqBody {
            optionId : tollConfig.optionId
          , rideId : Just state.rideRatingState.rideId
          , categoryId : tollConfig.categoryId
          , mediaFiles : []
          , description : "RIDE_END_QUICK_TOLL_REPORT"
          , chats : []
          , createTicket : false
          }
        void $ Remote.postIssueBT "en" postIssueBody
      pure unit
eval (PrimaryBtnRentalTripDetailsAC PrimaryButton.OnClick) state = continue state {showRentalRideDetails = false}

eval (RideDetails) state = exit $ RideDetailsScreen state

eval (DriverInfoCard) state = continue state { favDriverInfoCard = true }

eval (DriverInfocardAC FavouriteDriverInfoCard.Back) state = continue state { favDriverInfoCard = false }

eval (DriverInfocardAC (FavouriteDriverInfoCard.OnClickDone PrimaryButton.OnClick)) state = continue state { favDriverInfoCard = false }

eval (CountDown seconds status timerID) state = do
  if status == "EXPIRED" then do
    continue state { countDownValue = "0:00" }
  else
    continue state { countDownValue = timeInMinFormat seconds }

eval (GoToDriverProfile) state = exit $ GoToDriversProfile state

eval (FeedbackChanged id value) state = continue state { ratingCard { feedbackText = value } }

eval ( RateClick index ) state = do
  continue state { ratingCard { rating = index }, isRatingCard = true }

eval (HelpAndSupportAC) state = exit $ GoToHelpAndSupport state

eval (GoToSOS) state = exit $ GoToNammaSafety state true false

eval (Back) state = do
  continue state {
    recordedView = false,
    timerValue = "0:00",
    ratingCard {
    favDriver = false,
    rating = state.ratingCard.rating,
    feedbackText = "",
    feedbackList = [],
    recordAudioState { pauseLootie = false, recordedFile = Nothing, recordingDone = false, isRecording = false, openAddAudioModel = false, isUploading = false, timer = "00 : 00", recordedAudioUrl = Nothing, uploadedAudioId = Nothing, isListening = false } },
    isRatingCard = false }

eval (Skip) state = if isParentView FunctionCall then do
                        void $ pure $ emitTerminateApp Nothing true
                        continueWithCmd state [ do
                          void $ launchAff $ EHC.flowRunner defaultGlobalState $ do
                            void $ Remote.notifyFlowEvent $ Remote.makeNotifyFlowEventReq "RATE_DRIVER_SKIPPED"
                            pure unit
                          pure NoAction
                        ]
                    else exit $ HomeScreen state

eval (SelectPill feedbackItem id) state = do
  let newFeedbackList = updateFeedback id feedbackItem state.ratingCard.feedbackList
      filterFeedbackList = filter (\item -> length item.answer > 0) newFeedbackList
  continue state { ratingCard {  feedbackList = filterFeedbackList } }

eval (MakeFavourite) state = do
  continue state { ratingCard {  favDriver = not state.ratingCard.favDriver, feedbackText = "", recordAudioState { isRecording = false, timer = "00:00", recordingDone = false, recordedFile = Nothing, openAddAudioModel = false, isUploading = false, uploadedAudioId = Nothing, recordedAudioUrl = Nothing, isListening = false, pauseLootie = false} } }

eval (TimerCallback timerID timeInMinutes seconds) state = continue state { ratingCard { recordAudioState { timer = timeInMinutes } }, timerId = timerID, timerValue = timeInMinutes }

eval (OnClickRecord push) state = do
   continueWithCmd state { ratingCard { recordAudioState { timer = "00 : 00" } } } [do
    recordingStarted <- runEffectFn1 JB.startAudioRecording ""
    if recordingStarted then do
      void $ runEffectFn1 JB.removeMediaPlayer ""
      void $  waitingCountdownTimerV2 0 "1" "record_issue_audio" push TimerCallback
      pure $ UpdateState state {ratingCard { recordAudioState { isRecording = true, timer = "00:00" } }, recordedView = true }
    else
      pure $ NoAction
  ]

eval (UpdateState updatedState) state = continue updatedState

eval (OnClickStop) state =
  continueWithCmd state { ratingCard { recordAudioState { isRecording = false, recordingDone = true, timer = "00 : 00" } } } [do
    res <- runEffectFn1 JB.stopAudioRecording ""
    pure $ UpdateRecordModelPlayer res
  ]

eval (OnAudioCompleted _) state = do
  continue state {ratingCard { recordAudioState { isListening = false, pauseLootie = true } }}

eval (OnClickPlay) state = handleMediaPlayerRestart state

eval (OnClickPause) state = pauseMediaPlayer state

eval (UpdateRecordModelPlayer url) state = do
  continueWithCmd state { ratingCard { recordAudioState { recordedFile = Just url } } } [do
    pure $ OnClickDone
  ]

eval (OnClickDone) state =
  continueWithCmd state { ratingCard { recordAudioState { isUploading = true, pauseLootie = true } } } [do
    void $ pure $ JB.startLottieProcess JB.lottieAnimationConfig{ rawJson = "audio_upload_animation.json", lottieId = (EHC.getNewIDWithTag "audio_recording_done"), scaleType = "FIT_CENTER", speed = 1.0 }
    void $ pure $ clearTimerWithId state.timerId
    case state.ratingCard.recordAudioState.recordedFile of
      Just url -> do
                  res <- runEffectFn1 JB.saveAudioFile url
                  void $ runEffectFn5 JB.uploadMultiPartData res (EndPoint.uploadFile "") "Audio" "fileId" "file"
                  pure $  UpdateState state { ratingCard { recordAudioState { recordedAudioUrl = Just res}}}
      Nothing  -> do
                  pure $ UpdateState state { ratingCard { recordAudioState { recordedAudioUrl = Nothing } } }
  ]

eval (UploadMultiPartDataCallback fileType fileId) state = do
  continueWithCmd state { ratingCard { recordAudioState { uploadedAudioId = Just fileId } } } [do
    void $ runEffectFn1 JB.removeMediaPlayer ""
    pure $ NoAction
  ]

eval (OnClickClose) state = do
  pure $ JB.clearAudioPlayer ""
  let updatedState = state { ratingCard { recordAudioState { pauseLootie = false, recordedFile = Nothing, recordingDone = false, isRecording = false, openAddAudioModel = false, isUploading = false, timer = "00 : 00", recordedAudioUrl = Nothing, uploadedAudioId = Nothing, isListening = false } }, timerValue = "0:00", recordedView = false}
  continueWithCmd updatedState [do
    void $ runEffectFn1 JB.stopAudioRecording ""
    void $ pure $ clearTimerWithId state.timerId
    pure $ UpdateState updatedState
  ]

eval (RideCompletedAC (RideCompletedCard.SelectButton selectedYes pageIndex)) state = continueWithCmd state [pure $ SelectButton selectedYes pageIndex]

eval (SelectButton selectedYes pageIndex) state = do
  let
    availableBanners = issueReportBannerConfigs state
    noOfAvailableBanners = length availableBanners
    bannerAtIndex = availableBanners !! pageIndex

  case bannerAtIndex of
    Just bannerObj -> do
      let
        issueType = bannerObj.issueType
        issueResponse = find (\obj -> obj.issueType == issueType) state.customerIssue.customerResponse
        issueResponseIndex = findIndex (\obj -> obj.issueType == issueType) state.customerIssue.customerResponse
      case issueResponse , issueResponseIndex of
        Just respObj, Just respIdx -> do
          let
            updatedResponse = respObj {selectedYes = Just selectedYes}
            updatedIssueResponseArr = updateAt respIdx updatedResponse state.customerIssue.customerResponse
          case updatedIssueResponseArr of
            Just updatedIssueResponseArrObj -> do
              let
                updatedState = state {customerIssue {customerResponse = updatedIssueResponseArrObj}}
                userRespondedIssues = filter (\issueResp -> issueResp.selectedYes /= Nothing && (isIssue state issueResp.issueType)) updatedIssueResponseArrObj
                userRespondedIssuesCount = length userRespondedIssues

              if noOfAvailableBanners == userRespondedIssuesCount then do
                if(isIssue state DemandExtraTollAmount && updatedState.goToLastBanner) then continue updatedState{goToLastBanner = false, customerIssue {respondedValidIssues = true, buttonActive = true, currentPageIndex = if  (pageIndex + 1) < noOfAvailableBanners then pageIndex + 1 else pageIndex}}
                else continue updatedState{customerIssue {respondedValidIssues = true, buttonActive = true}}
              else continue updatedState{customerIssue {currentPageIndex = if  (pageIndex + 1) < noOfAvailableBanners then pageIndex + 1 else pageIndex} }
            Nothing -> update state
        _ , _ -> update state
    Nothing -> update state

eval (SetIssueReportBannerItems bannerItem) state = continue state {
  customerIssue {
    bannerComputedView = Just bannerItem
  }
}

eval (BannerChanged item) state = continue state{customerIssue{currentPageIndex = fromMaybe 0 (fromString item)}}

eval (KeyboardCallback keyBoardState) state = do
  let isOpen = case keyBoardState of
                    "onKeyboardOpen" -> true
                    "onKeyboardClose" -> false
                    _ -> false
  when(keyBoardState == "onKeyboardOpen") $ void $ pure $ JB.scrollToEnd (EHC.getNewIDWithTag "RideCompletedScrollView") true
  continue state{isKeyBoardOpen = isOpen}

eval _ state = update state

handleMediaPlayerRestart :: RiderRideCompletedScreenState -> Eval Action ScreenOutput RiderRideCompletedScreenState
handleMediaPlayerRestart state =
  continueWithCmd state {ratingCard { recordAudioState { isListening = true, pauseLootie = false } }, countDownValue = state.timerValue}
    [ do
      push <- getPushFn Nothing "RiderRideCompletedScreen"
      void $ startTimer ((timeStringToSeconds state.timerValue) + 1) "countDown" "1" push CountDown
      void $ pure $ runFn4 JB.startAudioPlayer (fromMaybe "" state.ratingCard.recordAudioState.recordedAudioUrl) push OnAudioCompleted "1"
      pure NoAction
    ]

pauseMediaPlayer :: RiderRideCompletedScreenState -> Eval Action ScreenOutput RiderRideCompletedScreenState
pauseMediaPlayer state =
  continueWithCmd state{ ratingCard { recordAudioState { isListening = false, pauseLootie = true } } }
    [ do
      void $ pure $ JB.pauseAudioPlayer ""
      pure NoAction
    ]

updateFeedback :: String -> String -> Array FeedbackAnswer -> Array FeedbackAnswer
updateFeedback feedbackId feedbackItem feedbackList =
  if hasFeedbackId feedbackId feedbackList
    then updateFeedbackAnswer feedbackId feedbackItem feedbackList
    else addFeedbackAnswer feedbackId feedbackItem feedbackList
  where
    hasFeedbackId :: String -> Array FeedbackAnswer -> Boolean
    hasFeedbackId fid list = any (\feedback -> feedback.questionId == fid) list

    updateFeedbackAnswer :: String -> String -> Array FeedbackAnswer -> Array FeedbackAnswer
    updateFeedbackAnswer fid newItem list =
      map (\feedback ->
        if feedback.questionId == fid
                then feedback { answer = if newItem `elem` (feedback.answer) then filter (\x -> x /= newItem) feedback.answer else feedback.answer <> [newItem] }
          else feedback
        ) list

    addFeedbackAnswer :: String -> String -> Array FeedbackAnswer -> Array FeedbackAnswer
    addFeedbackAnswer fid newItem list = do
      let config = {questionId : fid, answer : [newItem]}
      list <> [config]

isIssue :: RiderRideCompletedScreenState -> CustomerIssueTypes -> Boolean
isIssue state customerIssue = do
    let bannerConfigs = issueReportBannerConfigs state
    any(\x -> x.issueType == customerIssue) bannerConfigs

issueReportBannerConfigs :: forall action. RiderRideCompletedScreenState -> Array (CustomerIssueCard)
issueReportBannerConfigs state =
  let
    nightSafetyIssue = state.customerIssue.hasSafetyIssue
    accessibilityIssue =  state.customerIssue.hasAccessibilityIssue
    customerResposeArray = state.customerIssue.customerResponse
    demandExtraTollAmountIssue = state.customerIssue.demandExtraTollAmountIssue


    (nightSafetyIssueConfig :: CustomerIssueCard) = {
      issueType : NightSafety
    , selectedYes : findYesNoState customerResposeArray NightSafety
    , title : getString WAS_RIDE_SAFE
    , subTitle : getString WAS_RIDE_SAFE_DESC
    , yesText :  getString YES
    , noText : getString NO
    }

    (accessibilityIssueConfig :: CustomerIssueCard) = {
      issueType : Accessibility
    , selectedYes : findYesNoState customerResposeArray Accessibility
    , title : getString WAS_DRIVER_HELPFUL
    , subTitle : getString WAS_DRIVER_HELPFUL_DESC
    , yesText :  getString YES
    , noText : getString NO
    }

    (demandExtraTollAmountIssueConfig :: CustomerIssueCard) = {
      issueType : DemandExtraTollAmount
    , selectedYes : findYesNoState customerResposeArray DemandExtraTollAmount
    , title : getStringV2 driver_demand_extra
    , subTitle : getStringV2 demand_extra_toll_amount
    , yesText :  getStringV2 yes
    , noText : getStringV2 no
    }

  in
    if accessibilityIssue then [accessibilityIssueConfig] else
      (if nightSafetyIssue then [nightSafetyIssueConfig] else [])
      <> (if demandExtraTollAmountIssue then [demandExtraTollAmountIssueConfig] else [])

  where
    findYesNoState customerResp issueType =
      case find (\x -> x.issueType == issueType) customerResp of
        Just issue -> issue.selectedYes
        Nothing -> Nothing
