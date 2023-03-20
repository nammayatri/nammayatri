{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NotificationsScreen.Controller where

import Prelude
import Components.ErrorModal as ErrorModalController
import Components.NotificationCard.Controller as NotificationCardAC
import Components.NotificationDetailModel as NotificationDetailModel
import Components.NotificationDetailModel.Controller (fetchTitleAndUrl)
import Components.PopUpModal as PopUpModal
import Components.PrimaryEditText as PrimaryEditText
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array ((!!), union, length, unionBy, any) as Array
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, length, take, drop, joinWith, trim)
import Data.String.CodeUnits (charAt)
import Effect.Aff (launchAff_)
import Language.Strings (getString)
import Language.Types(STR(..))
import Engineering.Helpers.Commons (getNewIDWithTag, strToBool, flowRunner)
import Helpers.Utils (getImageUrl, getTimeStampString, removeMediaPlayer, setEnabled, setRefreshing, setYoutubePlayer)
import JBridge (hideKeyboardOnNavigation, requestKeyboardShow)
import PrestoDOM (Eval, ScrollState(..), Visibility(..), continue, exit, toPropValue, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (AnimationState(..), NotificationCardState, NotificationDetailModelState, NotificationsScreenState, NotificationCardPropState, YoutubeData, YoutubeVideoStatus(..))
import Services.APITypes (MediaFileApiResponse(..), MediaType(..), MessageAPIEntityResponse(..), MessageListRes(..), MessageType(..))
import Services.Backend as Remote
import Debug.Trace

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data ScreenOutput
  = RefreshScreen NotificationsScreenState
  | GoBack
  | LoaderOutput NotificationsScreenState

data Action
  = OnFadeComplete String
  | Refresh
  | BackPressed
  | ErrorModalActionController ErrorModalController.Action
  | NotificationCardClick NotificationCardAC.Action
  | Scroll String
  | ScrollStateChanged ScrollState
  | NotificationDetailModelAC NotificationDetailModel.Action
  | MessageListResAction MessageListRes
  | NoAction
  | LoadMore

eval :: Action -> NotificationsScreenState -> Eval Action ScreenOutput NotificationsScreenState
eval Refresh state = exit $ RefreshScreen state

eval BackPressed state = do
  if state.notifsDetailModelVisibility == VISIBLE && state.notificationDetailModelState.addCommentModelVisibility == GONE then
    continueWithCmd state { notifsDetailModelVisibility = GONE }
    [ do
        _ <- pure $ setYoutubePlayer youtubeData (getNewIDWithTag "youtubeView") $ show PAUSE
        _ <- removeMediaPlayer ""
        pure NoAction
    ]
  else if state.notificationDetailModelState.addCommentModelVisibility == VISIBLE then
    continue state { notificationDetailModelState { addCommentModelVisibility = GONE, comment = Nothing} }
  else
    exit $ GoBack 

eval (NotificationCardClick (NotificationCardAC.Action1Click index)) state = do
  case state.notificationList Array.!! index of
    Nothing -> continue state
    Just notificationItem -> continue state { notificationDetailModelState = notifisDetailStateTransformer notificationItem, notifsDetailModelVisibility = VISIBLE }

eval (NotificationCardClick (NotificationCardAC.Action2Click index)) state = continue state

eval (NotificationCardClick (NotificationCardAC.IllutrationClick index)) state = do
  case state.notificationList Array.!! index of
    Nothing -> continue state
    Just notificationItem -> continue state { notificationDetailModelState = notifisDetailStateTransformer notificationItem, notifsDetailModelVisibility = VISIBLE }

eval (NotificationDetailModelAC NotificationDetailModel.BackArrow) state =
  continueWithCmd state
    [ pure BackPressed
    ]

eval (NotificationDetailModelAC NotificationDetailModel.AddCommentClick) state = do
  _ <- pure $ requestKeyboardShow $ getNewIDWithTag "NotificationDetailModel"
  continue state { notificationDetailModelState { addCommentModelVisibility = VISIBLE } }

eval (NotificationDetailModelAC (NotificationDetailModel.AddCommentModelAction PopUpModal.OnImageClick)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  continue state { notificationDetailModelState { addCommentModelVisibility = GONE, comment = Nothing } }

eval (NotificationDetailModelAC (NotificationDetailModel.AddCommentModelAction (PopUpModal.ETextController (PrimaryEditText.TextChanged id text)))) state =
  continue
    state
      { notificationDetailModelState
        { comment = Just text
        , commentBtnActive = if length text > 5 then true else false
        }
      }

eval (NotificationDetailModelAC (NotificationDetailModel.AddCommentModelAction PopUpModal.OnButton2Click)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  case state.notificationDetailModelState.comment of
    Just comment -> do
      let updatedNotificationList = (map(\item -> if(item.messageId == state.notificationDetailModelState.messageId) then item{comment = Just comment } else item)state.notificationList)
      continueWithCmd state { notificationDetailModelState { addCommentModelVisibility = GONE }, notificationList = updatedNotificationList }
        [ do
            launchAff_ $ flowRunner $ runExceptT $ runBackT
              $ do
                  res <- Remote.messageResponseBT state.notificationDetailModelState.messageId (Remote.makeMessageReplyReq comment)
                  pure unit
            pure NoAction
        ]
    Nothing -> continue state

eval (NotificationDetailModelAC NotificationDetailModel.AfterRender) state = do
  let 
    updatedItem = map (\a -> if a.messageId == state.notificationDetailModelState.messageId then a{ notificationNotSeen = false } else a) state.notificationList
    updatedPrestoItem = map (\a -> if a.messageId == toPropValue state.notificationDetailModelState.messageId then a{ notificationNotSeen = toPropValue "gone" } else a) state.prestoListArrayItems
  continue state{ prestoListArrayItems = updatedPrestoItem, notificationList = updatedItem }

eval (ScrollStateChanged scrollState) state = do
  _ <- if scrollState == SCROLL_STATE_FLING then pure $ setEnabled (getNewIDWithTag "NotificationSwipeRefresh") false else pure unit
  continue state

eval (Scroll value) state = do
  let
    sRLayoutId = getNewIDWithTag "NotificationSwipeRefresh"
  let
    firstIndex = fromMaybe 0 (fromString (fromMaybe "0" ((split (Pattern ",") (value)) Array.!! 0)))
  let
    visibleItems = fromMaybe 0 (fromString (fromMaybe "0" ((split (Pattern ",") (value)) Array.!! 1)))
  let
    totalItems = fromMaybe 0 (fromString (fromMaybe "0" ((split (Pattern ",") (value)) Array.!! 2)))
  let
    canScrollUp = fromMaybe true (strToBool (fromMaybe "true" ((split (Pattern ",") (value)) Array.!! 3)))
  let
    loadMoreButton = if (totalItems == (firstIndex + visibleItems) && totalItems /= 0 && totalItems /= visibleItems) then true else false
  _ <- if canScrollUp then (pure $ setEnabled sRLayoutId false) else pure $ setEnabled sRLayoutId true
  continue state { loaderButtonVisibility = loadMoreButton }

eval (OnFadeComplete _) state = do
  if (state.recievedResponse == false) then
    continue state
  else
    continue
      state
        { shimmerLoader =
          case state.shimmerLoader of
            AnimatedIn -> AnimatedOut
            AnimatingOut -> AnimatedOut
            a -> a
        }

eval (MessageListResAction (MessageListRes notificationArray)) state = do
  let
    propValueList = propValueTransformer notificationArray
  let
    notificationsList = notificationListTransformer notificationArray
  _ <- pure $ setRefreshing (getNewIDWithTag "NotificationSwipeRefresh") false
  let
    loadBtnDisabled = if (Array.length notificationArray == 0) then true else false
  if state.loadMore then
      continue $ state { shimmerLoader = AnimatedOut, recievedResponse = true, notificationList = Array.unionBy (\a b -> a.messageId == b.messageId) state.notificationList notificationsList, prestoListArrayItems = Array.unionBy (\a b -> a.messageId == b.messageId) state.prestoListArrayItems propValueList, loadMoreDisabled = loadBtnDisabled, loadMore = false }
  else
      continue $ state { shimmerLoader = AnimatedOut, recievedResponse = true, notificationList = Array.unionBy (\a b -> a.messageId == b.messageId) notificationsList state.notificationList, prestoListArrayItems = Array.unionBy (\a b -> a.messageId == b.messageId) propValueList state.prestoListArrayItems, loadMoreDisabled = loadBtnDisabled }

eval LoadMore state = do
  exit $ LoaderOutput state{loadMore = true}

eval _ state = continue state

notifisDetailStateTransformer :: NotificationCardState -> NotificationDetailModelState
notifisDetailStateTransformer selectedItem =
  { mediaUrl: selectedItem.mediaUrl
  , title: selectedItem.title
  , timeLabel: selectedItem.timeLabel
  , description: splitUrlsAndText selectedItem.description
  , actionText: selectedItem.action2Text
  , actionVisibility: if selectedItem.action2Text == "" then GONE else VISIBLE
  , addCommentModelVisibility: GONE
  , comment: selectedItem.comment
  , commentBtnActive: false
  , messageId: selectedItem.messageId
  , notificationNotSeen: selectedItem.notificationNotSeen
  , imageUrl: getImageUrl $ selectedItem.mediaUrl
  , mediaType: selectedItem.mediaType
  }

notificationListTransformer :: Array MessageAPIEntityResponse -> Array NotificationCardState
notificationListTransformer notificationArray =
  ( map
      ( \(MessageAPIEntityResponse notificationItem) ->
          let
            (MediaFileApiResponse media) = (fromMaybe dummyMedia ((notificationItem.mediaFiles) Array.!! 0))
          in
            { mediaUrl: media.url
            , title: notificationItem.title
            , description: notificationItem.description
            , messageId: notificationItem.messageId
            , notificationNotSeen: not notificationItem.readStatus
            , action1Text: (getString SHOW_MORE)
            , action2Text: ""
            , notificationLabel: "New"
            , timeLabel: getTimeStampString notificationItem.created_at <> " ago"
            , comment: notificationItem.reply
            , imageUrl: 
                case media.fileType of
                  VideoLink -> getImageUrl $ media.url
                  Video -> ""
                  ImageLink -> ""
                  Image -> ""
                  AudioLink -> "ny_ic_audio_file"
                  Audio -> "ny_ic_audio_file"
            , mediaType: Just media.fileType
            }
      )
      notificationArray
  )

propValueTransformer :: Array MessageAPIEntityResponse -> Array NotificationCardPropState
propValueTransformer notificationArray =
  ( map
      ( \(MessageAPIEntityResponse notificationItem) ->
          let
            (MediaFileApiResponse media) = (fromMaybe dummyMedia ((notificationItem.mediaFiles) Array.!! 0))
          in
            { mediaUrl: toPropValue $ media.url
            , description: toPropValue $ notificationCardDesc notificationItem.description
            , title: toPropValue notificationItem.title
            , notificationNotSeen: toPropValue $ if notificationItem.readStatus then "gone" else "visible"
            , action1Text: toPropValue (getString SHOW_MORE)
            , action2Text: toPropValue "Take me there"
            , notificationLabel: toPropValue "New"
            , timeLabel: toPropValue $ getTimeStampString notificationItem.created_at <> " ago"
            , cardVisibility: toPropValue "visible"
            , shimmerVisibility: toPropValue "gone"
            , notificationLabelColor: toPropValue "#FF8533"
            , action1Visibility: toPropValue "visible"
            , action2Visibility: toPropValue "gone"
            , descriptionVisibility: toPropValue "visible"
            , illustrationVisibility: toPropValue if Array.any (_ == media.fileType) [ VideoLink, Audio, Image ] then "visible" else "gone"
            , playBtnVisibility: toPropValue $ if media.fileType == VideoLink then "visible" else "gone"
            , playButton: toPropValue "ny_ic_play_btn"
            , imageUrl:
                toPropValue
                  $ case media.fileType of
                      VideoLink -> getImageUrl $ media.url
                      Video -> ""
                      ImageLink -> ""
                      Image -> ""
                      AudioLink -> "ny_ic_audio_file"
                      Audio -> "ny_ic_audio_file"
            , previewImage: toPropValue $ if media.fileType == Image then "visible" else "gone"
            , imageVisibility : toPropValue $ if media.fileType /= Image then "visible" else "gone" 
            , previewImageTitle: toPropValue "Preview Image"
            , messageId: toPropValue notificationItem.messageId
            }
      )
      notificationArray
  )

dummyMedia :: MediaFileApiResponse
dummyMedia =
  MediaFileApiResponse
    { url: ""
    , fileType: Video
    }

youtubeData :: YoutubeData
youtubeData =
  { videoTitle: ""
  , setVideoTitle: false
  , showMenuButton: false
  , showDuration: true
  , showSeekBar: true
  , videoId: ""
  }

splitUrlsAndText :: String -> Array String
splitUrlsAndText str = split (Pattern "$$") str

notificationCardDesc :: String -> String
notificationCardDesc text = 
  let
    splittedArray = splitUrlsAndText text
    filteredArray = map (\word ->
    let
      wordLength = length word
      in
        if charAt 0 word == Just '*' && charAt (wordLength - 1) word == Just '*'
          then let
          titleAndUrl = fetchTitleAndUrl wordLength word 
          linkTitle = trim $ fromMaybe "" (titleAndUrl Array.!! 0)
          in
            linkTitle
          else
          word 
          ) splittedArray
    combinedString = joinWith " " filteredArray
  in 
    combinedString
  
