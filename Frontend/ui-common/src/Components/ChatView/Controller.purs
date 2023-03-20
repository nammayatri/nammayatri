module Components.ChatView.Controller where

data Action = SendMessage
            | SendSuggestion String
            | BackPressed
            | TextChanged String
            | Call
            | Navigate
            | NoAction

type Config = 
  { userConfig :: UserConfig
  , messages :: Array ChatComponent
  , sendMessageActive :: Boolean
  , distance :: String
  , suggestionsList :: Array String
  , hint :: String
  , suggestionHeader :: String
  , emptyChatHeader :: String
  , mapsText :: String
  , grey700 :: String
  , blue600 :: String
  , blue900 :: String
  , transparentGrey :: String
  , green200 :: String
  , grey900 :: String
  , grey800 :: String
  , blue800 :: String
  , white900 :: String
  , black800 :: String
  , black700 :: String
  }

type UserConfig =
  { userName :: String
  , appType :: String
  }

type ChatComponent = {
    message :: String 
  , sentBy :: String 
  , timeStamp :: String
}

config :: Config
config = 
  {
    userConfig : 
        {
          userName : ""
        , appType : ""
        }
    , messages : []
    , sendMessageActive : false
    , distance : ""
    , suggestionsList : []
    , hint : ""
    , suggestionHeader : ""
    , emptyChatHeader : ""
    , mapsText : ""
    , grey700 : ""
    , blue900 : ""
    , blue600 : ""
    , transparentGrey : ""
    , green200 : ""
    , grey800 : ""
    , blue800 : ""
    , grey900 : ""
    , white900 : ""
    , black800 : ""
    , black700 : ""
  }

makeChatComponent :: String -> String -> String -> ChatComponent
makeChatComponent message sender timeStamp =  {
  "message" : message 
, "sentBy" : sender
, "timeStamp" : timeStamp
}
