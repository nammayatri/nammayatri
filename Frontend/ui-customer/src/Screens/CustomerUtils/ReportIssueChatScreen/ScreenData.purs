{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReportIssueChatScreen.ScreenData where

import Prelude

import Data.Maybe (Maybe(..))
import Screens.Types (ReportIssueChatScreenState, ReportIssueChatScreenEntryPoint(..))
import Components.ChatView.Controller as ChatView
import Common.Styles.Colors (black700, black800, blue600, blue800, blue900, green200, grey700, grey800, grey900, transparentGrey, white900) as Color
import PrestoDOM (Visibility(..)) 
import MerchantConfig.DefaultConfig as DC

initData :: ReportIssueChatScreenState
initData = {
    data: {
        tripId: Nothing,
        selectedOption: Nothing,
        categoryName: "",
        issueId: Nothing,
        categoryId: "",
        messageToBeSent: "",
        chatConfig: chatConfig',
        recordAudioState: recordAudioState',
        addImagesState: addImagesState',
        addedImages: [],
        viewImageState: { image : "", imageName : Nothing},
        recordedAudioUrl: Nothing,
        addAudioState: addAudioState',
        uploadedImagesIds: [],
        uploadedAudioId: Nothing,
        options: [],
        chats: [],
        merchantExoPhone : Nothing,
        showStillHaveIssue : false,
        selectedRide : Nothing, 
        entryPoint : TripDetailsScreenEntry,
        issueReportShortId : Nothing,
        config : DC.config
    },
    props : {
      showSubmitComp: false,
      showImageModel: false,
      showAudioModel: false,
      showCallDriverModel: false,
      showCallSupportModel: false,
      showRecordModel: false,
      showViewImageModel: false,
      isPopupModelOpen: false,
      isKeyboardOpen: false,
      timerId : "",
      initalizedCallbacks : false,
      isResolved: false, 
      isEndFlow : false,
      showEndFlowMessage : false
    }
}

type OptionType = {
    optionName :: String,
    optionId :: Number
}
chatConfig' :: ChatView.Config
chatConfig' = ChatView.config {
                       userConfig {
                         userName = "later",
                         appType = "Customer"
                       }
                     , messages = []
                     , sendMessageActive = false
                     , vehicleNo = ""
                     , chatSuggestionsList = []
                     , hint = "Message"
                     , suggestionHeader = "Start Chat"
                     , suggestionDelay = 750
                     , emptyChatHeader = "Type something"
                     , showHeader = false
                     , showStroke = false
                     , mapsText = "maps"
                     , enableSuggestionClick = false
                     , transparentGrey = Color.transparentGrey
                     , showTextEdit = false
                     , spanParent = true
              }

recordAudioState' = {
  isRecording: false,
  recordedFile: Nothing,
  recordingDone: false,
  openAddAudioModel: false,
  isUploading: false,
  timer: "00:00"
}

addImagesState' = {
  images: [],
  stateChanged: false,
  isLoading: false,
  imageMediaIds: []
}

addAudioState' = {
  audioFile: Nothing,
  stateChanged: false
}