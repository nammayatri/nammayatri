module Common.Resources.Constants where
import Engineering.Helpers.Commons (os)
import Prelude ((==))

zoomLevel :: Number
zoomLevel = if (os == "IOS") then 19.0 else 17.0

pickupZoomLevel :: Number
pickupZoomLevel = 18.0

chatSuggestion :: String
chatSuggestion = "chat_suggestions"

emChatSuggestion :: String
emChatSuggestion = "emergency_chat_suggestions"

policeNumber :: String
policeNumber = "112"

assetDomain :: String
assetDomain = "assets.moving.tech"

chatService :: String 
chatService = "in.juspay.mobility.app.ChatService"

locateOnMapLabelMaxWidth :: Int
locateOnMapLabelMaxWidth = if (os == "IOS") then 140 else 400

maxImageUploadInIssueReporting :: Int  
maxImageUploadInIssueReporting = 4