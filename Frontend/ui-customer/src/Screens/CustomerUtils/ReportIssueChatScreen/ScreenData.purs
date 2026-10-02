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
import Components.ChatView.Controller as ChatView
import Common.Styles.Colors as Color
import PrestoDOM (Visibility(..)) 
import MerchantConfig.DefaultConfig as DC
import Foreign.Class (class Decode, class Encode)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Components.ChatView.Controller (ChatComponentConfig, Config)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode, defaultDecode, defaultEncode)
import Data.Show.Generic (genericShow)
import Services.API (Chat)
import Screens.Types (IndividualRideCardState)
import MerchantConfig.Types (AppConfig)
import Common.Types.App (CategoryListType)
import ConfigProvider
import Services.API (MandatoryUploads(..))

initData :: ReportIssueChatScreenState
initData = {
    data: {
        tripId: Nothing,
        selectedOption: Nothing,
        issueId: Nothing,
        selectedCategory: selectedCategory',
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
        config : getAppConfig appConfig,
        mandatoryUploads : Nothing
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

selectedCategory' = {
  categoryName : "", 
  categoryImageUrl : Nothing, 
  categoryId : "", 
  categoryAction : Nothing,  
  isRideRequired : false,
  maxAllowedRideAge : Nothing,
  allowedRideStatuses : Nothing,
  categoryType: ""
}

type ReportIssueChatScreenState = {
    data :: ReportIssueChatScreenData,
    props :: ReportIssueChatScreenProps
}

type ReportIssueChatScreenData = {
  tripId :: Maybe String,
  selectedCategory :: CategoryListType,
  messageToBeSent :: String,
  issueId :: Maybe String,
  chatConfig :: Config,
  selectedOption :: Maybe Option,
  addedImages :: Array { image :: String, imageName :: String },
  recordAudioState :: RecordAudioState,
  addImagesState :: AddImageState,
  viewImageState :: ViewImageState,
  recordedAudioUrl :: Maybe String,
  addAudioState :: AddAudioState,
  uploadedImagesIds :: Array String,
  uploadedAudioId :: Maybe String,
  options :: Array Option,
  chats :: Array Chat,
  showStillHaveIssue :: Boolean,
  merchantExoPhone :: Maybe String,
  selectedRide :: Maybe IndividualRideCardState,
  entryPoint :: ReportIssueChatScreenEntryPoint,
  config :: AppConfig,
  issueReportShortId :: Maybe String,
  mandatoryUploads :: Maybe (Array MandatoryUploads)
}
data ReportIssueChatScreenEntryPoint = TripDetailsScreenEntry | RideSelectionScreenEntry | HelpAndSupportScreenEntry | OldChatEntry | SafetyScreen | HomeScreenEntry | FaqEntry | RiderRideCompletedScreen
derive instance genericReportIssueChatScreenEntryPoint :: Generic ReportIssueChatScreenEntryPoint _
instance showReportIssueChatScreenEntryPoint :: Show ReportIssueChatScreenEntryPoint where show = genericShow
instance eqReportIssueChatScreenEntryPoint :: Eq ReportIssueChatScreenEntryPoint where eq = genericEq
instance encodeReportIssueChatScreenEntryPoint :: Encode ReportIssueChatScreenEntryPoint where encode = defaultEnumEncode
instance decodeReportIssueChatScreenEntryPoint :: Decode ReportIssueChatScreenEntryPoint where decode = defaultEnumDecode

type RecordAudioState = {
  timer         :: String,
  isRecording   :: Boolean,
  isUploading   :: Boolean,
  recordedFile  :: Maybe String,
  recordingDone :: Boolean,
  openAddAudioModel :: Boolean
}

type AddImageState = {
  images :: Array Image,
  stateChanged :: Boolean,
  isLoading :: Boolean,
  imageMediaIds :: Array String
}

type ViewImageState = {
   image :: String,
   imageName :: Maybe String
}

type AddAudioState = {
  audioFile :: Maybe String,
  stateChanged :: Boolean
}

type Image = {
  image :: String, 
  imageName :: String
}

type Option = { 
  issueOptionId :: String
, option :: String
, label :: String
, mandatoryUploads :: Maybe (Array MandatoryUploads)
}

type ReportIssueChatScreenProps = {
  showSubmitComp :: Boolean,
  showImageModel :: Boolean,
  showAudioModel :: Boolean,
  showRecordModel :: Boolean,
  showCallDriverModel :: Boolean,
  showCallSupportModel :: Boolean,
  showViewImageModel :: Boolean,
  isPopupModelOpen :: Boolean,
  isKeyboardOpen :: Boolean, 
  timerId :: String, 
  initalizedCallbacks :: Boolean,
  isResolved :: Boolean,
  isEndFlow :: Boolean,
  showEndFlowMessage :: Boolean
}