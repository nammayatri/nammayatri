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
import Screens.Types (ReportIssueChatScreenState)
import Components.ChatView.Controller (config) as ChatView
import Styles.Colors (black700, black800, blue600, blue800, blue900, green200, grey700, grey800, grey900, transparentGrey, white900) as Color

initData :: ReportIssueChatScreenState
initData =
  { data:
      { tripId: Nothing
      , selectedOptionId: Nothing
      , categoryName: ""
      , categoryAction: ""
      , issueId: Nothing
      , categoryId: ""
      , messageToBeSent: ""
      , chatConfig: chatConfig'
      , recordAudioState: recordAudioState'
      , addImagesState: addImagesState'
      , addedImages: []
      , viewImageState: { image: "", imageName: Nothing }
      , recordedAudioUrl: Nothing
      , addAudioState: addAudioState'
      , uploadedImagesIds: []
      , uploadedAudioId: Nothing
      , options: []
      }
  , props:
      { showSubmitComp: false
      , showImageModel: false
      , showAudioModel: false
      , showCallCustomerModel: false
      , isReversedFlow: false
      , showRecordModel: false
      , showViewImageModel: false
      , isPopupModelOpen: false
      , submitIsInProgress: false
      }
  }

type OptionType
  = { optionName :: String
    , optionId :: Number
    }

chatConfig' =
  ChatView.config
    { userConfig
      { userName = "later"
      , appType = "Driver"
      }
    , messages = []
    , sendMessageActive = false
    , vehicleNo = ""
    , suggestionsList = []
    , hint = "Message"
    , suggestionHeader = "Start Chat"
    , suggestionDelay = 750
    , emptyChatHeader = "Type something"
    , showHeader = false
    , showStroke = false
    , mapsText = "maps"
    , grey700 = Color.grey700
    , blue600 = Color.blue600
    , enableSuggestionClick = false
    , blue900 = Color.blue900
    , transparentGrey = Color.transparentGrey
    , green200 = Color.green200
    , grey900 = Color.grey900
    , grey800 = Color.grey800
    , blue800 = Color.blue800
    , white900 = Color.white900
    , showTextEdit = false
    , spanParent = true
    , black800 = Color.black800
    , black700 = Color.black700
    }

recordAudioState' =
  { isRecording: false
  , recordedFile: Nothing
  , recordingDone: false
  , openAddAudioModel: false
  , isUploading: false
  , timer: "00:00"
  }

addImagesState' =
  { images: []
  , stateChanged: false
  , isLoading: false
  , imageMediaIds: []
  }

addAudioState' =
  { audioFile: Nothing
  , stateChanged: false
  }
