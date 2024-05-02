module Common.Resources.Constants where
import Engineering.Helpers.Commons (os)
import Prelude ((==))

zoomLevel :: Number
zoomLevel = if (os == "IOS") then 19.0 else 18.0

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

cancelRideSuggestion :: String
cancelRideSuggestion = "cacs3"

acRideCnfSuggestion :: String
acRideCnfSuggestion = "dacs5"

issueWithACSuggestion :: String
issueWithACSuggestion = "dacs1"
