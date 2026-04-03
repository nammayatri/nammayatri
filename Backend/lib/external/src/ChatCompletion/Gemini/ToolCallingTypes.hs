{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ChatCompletion.Gemini.ToolCallingTypes where

import Data.Text as T
import Kernel.Prelude
import Kernel.Utils.JSON
import ChatCompletion.Interface.ToolCalling as CIT

-- | Gemini Chat Completion Request with Tools
data ContentsToolReq = ContentsToolReq
  { contents :: [Content],
    tools :: Maybe [Tool]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Tool definition for Gemini
data Tool = Tool
  { toolFunctionDeclarations :: [FunctionDeclaration]
  }
  deriving (Show, Generic)

instance FromJSON Tool where
  parseJSON = withObject "Tool" $ \obj -> do
    funcDecls <- obj .: "functionDeclarations"
    return $ Tool funcDecls

instance ToJSON Tool where
  toJSON Tool {..} = object ["functionDeclarations" .= toolFunctionDeclarations]

-- | Function declaration
data FunctionDeclaration = FunctionDeclaration
  { funcName :: Text,
    funcDescription :: Text,
    funcParameters :: ToolParameters
  }
  deriving (Show, Generic)

instance FromJSON FunctionDeclaration where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON FunctionDeclaration where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

-- | Tool parameters schema
data ToolParameters = ToolParameters
  { paramsType :: Text,
    paramsProperties :: Map Text ParameterProperty,
    paramsRequired :: [Text]
  }
  deriving (Show, Generic)

instance FromJSON ToolParameters where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ToolParameters where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

-- | Individual parameter property
data ParameterProperty = ParameterProperty
  { propertyType :: Text,
    propertyDescription :: Text,
    propertyEnum :: Maybe [Text]
  }
  deriving (Show, Generic)

instance FromJSON ParameterProperty where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ParameterProperty where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

-- | Content with role and parts
data Content = Content
  { contentRole :: Text,
    contentParts :: [Part]
  }
  deriving (Show, Generic)

instance FromJSON Content where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Content where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

-- | Part of content - can be text or function call
data Part = Part
  { partText :: Maybe Text,
    partFunctionCall :: Maybe FunctionCall
  }
  deriving (Show, Generic)

instance FromJSON Part where
  parseJSON = withObject "Part" $ \obj -> do
    text <- obj .:? "text"
    funcCall <- obj .:? "functionCall"
    return $ Part text funcCall

instance ToJSON Part where
  toJSON Part {..} =
    case (partText, partFunctionCall) of
      (Just t, Nothing) -> object ["text" .= t]
      (Nothing, Just fc) -> object ["functionCall" .= fc]
      _ -> object []

-- | Function call from model
data FunctionCall = FunctionCall
  { funcCallName :: Text,
    funcCallArgs :: Value -- JSON object of arguments
  }
  deriving (Show, Generic)

instance FromJSON FunctionCall where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON FunctionCall where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

-- | Gemini response with tool calls
data ContentsToolResp = ContentsToolResp
  { candidates :: [Candidate],
    usageMetadata :: UsageMetadata
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Candidate response
data Candidate = Candidate
  { candidateContent :: Content,
    candidateFinishReason :: Text,
    candidateIndex :: Int
  }
  deriving (Show, Generic)

instance FromJSON Candidate where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Candidate where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

-- | Usage metadata
data UsageMetadata = UsageMetadata
  { promptTokenCount :: Int,
    candidatesTokenCount :: Int,
    totalTokenCount :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Convert generic tool definition to Gemini tool
convertToGeminiTool :: CIT.ToolDefinition -> Tool
convertToGeminiTool CIT.ToolDefinition {..} =
  Tool
    { toolFunctionDeclarations =
        [ FunctionDeclaration
            { funcName = toolName,
              funcDescription = toolDescription,
              funcParameters = convertParameters toolParameters
            }
        ]
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

-- | Extract function calls from Gemini response
extractFunctionCalls :: ContentsToolResp -> [CIT.ToolCall]
extractFunctionCalls ContentsToolResp {..} =
  concatMap extractFromCandidate candidates
  where
    extractFromCandidate Candidate {..} =
      concatMap extractFromPart candidateContent.contentParts
    extractFromPart Part {..} =
      case partFunctionCall of
        Just FunctionCall {..} ->
          [ CIT.ToolCall
              { toolCallId = funcCallName <> "_" <> T.pack (show candidateIndex),
                toolCallName = funcCallName,
                toolCallArguments = encodeToText funcCallArgs
              }
          ]
        Nothing -> []
    encodeToText = T.pack . show

-- | Build function response content for Gemini
buildFunctionResponseContent :: Text -> Value -> Content
buildFunctionResponseContent funcName result =
  Content
    { contentRole = "function",
      contentParts =
        [ Part
            { partText = Nothing,
              partFunctionCall =
                Just
                  FunctionCall
                    { funcCallName = funcName,
                      funcCallArgs = result
                    }
            }
        ]
    }
