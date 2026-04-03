{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ChatCompletion.AzureOpenAI.ToolCallingTypes where

import Data.Text as T
import Kernel.Prelude
import Kernel.Utils.JSON
import ChatCompletion.Interface.ToolCalling as CIT

-- | Azure OpenAI Chat Completion Request with Tools
data ChatCompletionToolReq = ChatCompletionToolReq
  { messages :: [Message],
    tools :: Maybe [Tool],
    toolChoice :: Maybe ToolChoice
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Tool definition for Azure OpenAI
data Tool = Tool
  { toolType :: Text,
    toolFunction :: ToolFunction
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Tool function definition
data ToolFunction = ToolFunction
  { functionName :: Text,
    functionDescription :: Text,
    functionParameters :: ToolParameters
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Tool parameters schema
data ToolParameters = ToolParameters
  { paramsType :: Text,
    paramsProperties :: Map Text ParameterProperty,
    paramsRequired :: [Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Individual parameter property
data ParameterProperty = ParameterProperty
  { propertyType :: Text,
    propertyDescription :: Text,
    propertyEnum :: Maybe [Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Tool choice configuration
data ToolChoice
  = ToolChoiceAuto
  | ToolChoiceNone
  | ToolChoiceFunction Text
  deriving (Show, Generic)

instance ToJSON ToolChoice where
  toJSON ToolChoiceAuto = "auto"
  toJSON ToolChoiceNone = "none"
  toJSON (ToolChoiceFunction name) = object ["type" .= ("function" :: Text), "function" .= object ["name" .= name]]

instance FromJSON ToolChoice where
  parseJSON (String "auto") = return ToolChoiceAuto
  parseJSON (String "none") = return ToolChoiceNone
  parseJSON (Object obj) = do
    ty <- obj .: "type"
    if ty == ("function" :: Text)
      then do
        funcObj <- obj .: "function"
        name <- funcObj .: "name"
        return $ ToolChoiceFunction name
      else fail "Invalid tool_choice type"
  parseJSON _ = fail "Invalid tool_choice"

-- | Message with tool calls
data Message = Message
  { role :: Text,
    content :: Maybe Text,
    toolCalls :: Maybe [ToolCall],
    toolCallId :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Message where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Message where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

-- | Tool call from assistant
data ToolCall = ToolCall
  { toolCallId :: Text,
    toolCallType :: Text,
    toolCallFunction :: ToolCallFunction
  }
  deriving (Show, Generic)

instance FromJSON ToolCall where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ToolCall where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

-- | Function call details
data ToolCallFunction = ToolCallFunction
  { functionName :: Text,
    functionArguments :: Text
  }
  deriving (Show, Generic)

instance FromJSON ToolCallFunction where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ToolCallFunction where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

-- | Chat completion response with tool calls
data ChatCompletionToolResponse = ChatCompletionToolResponse
  { choices :: [Choice],
    created :: Int,
    id :: Text,
    model :: Text,
    _object :: Text,
    systemFingerprint :: Text,
    usage :: Usage
  }
  deriving (Show, Generic)

instance FromJSON ChatCompletionToolResponse where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ChatCompletionToolResponse where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

-- | Choice in response
data Choice = Choice
  { choiceFinishReason :: Text,
    choiceIndex :: Int,
    choiceMessage :: Message
  }
  deriving (Show, Generic)

instance FromJSON Choice where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Choice where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

-- | Token usage
data Usage = Usage
  { usageCompletionTokens :: Int,
    usagePromptTokens :: Int,
    usageTotalTokens :: Int
  }
  deriving (Show, Generic)

instance FromJSON Usage where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Usage where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

-- | Convert generic tool definition to Azure OpenAI tool
convertToAzureTool :: CIT.ToolDefinition -> Tool
convertToAzureTool CIT.ToolDefinition {..} =
  Tool
    { toolType = "function",
      toolFunction =
        ToolFunction
          { functionName = toolName,
            functionDescription = toolDescription,
            functionParameters = convertParameters toolParameters
          }
    }

convertParameters :: CIT.ToolParameters -> ToolParameters
convertParameters CIT.ToolParameters {..} =
  ToolParameters
    { paramsType = paramType,
      paramsProperties = Map.map convertProperty paramProperties,
      paramsRequired = paramRequired
    }

convertProperty :: CIT.ParameterProperty -> ParameterProperty
convertProperty CIT.ParameterProperty {..} =
  ParameterProperty
    { propertyType = propType,
      propertyDescription = propDescription,
      propertyEnum = propEnum
    }

-- | Convert Azure OpenAI tool call to generic tool call
convertFromAzureToolCall :: ToolCall -> CIT.ToolCall
convertFromAzureToolCall ToolCall {..} =
  CIT.ToolCall
    { toolCallId = toolCallId,
      toolCallName = toolCallFunction.functionName,
      toolCallArguments = toolCallFunction.functionArguments
    }

-- | Build tool response message
buildToolResponseMessage :: Text -> Text -> Text -> Message
buildToolResponseMessage callId toolName result =
  Message
    { role = "tool",
      content = Just result,
      toolCalls = Nothing,
      toolCallId = Just callId
    }
