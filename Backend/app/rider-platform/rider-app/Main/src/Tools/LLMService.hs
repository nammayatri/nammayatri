{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingStrategies #-}

module Tools.LLMService
  ( -- * LLM Service Interface
    LLMServiceConfig (..),
    LLMProvider (..),
    LLMMessage (..),
    LLMChatCompletionReq (..),
    LLMChatCompletionResp (..),
    LLMStreamChunk (..),
    LLMStreamEvent (..),

    -- * Chat Completion
    chatCompletion,
    chatCompletionWithTools,

    -- * Streaming Chat Completion
    chatCompletionStream,
    chatCompletionStreamWithTools,

    -- * Configuration
    getLLMServiceConfig,

    -- * Utility Functions
    createUserMessage,
    createSystemMessage,
    createAssistantMessage,
    mergeStreamChunks,
  )
where

import qualified ChatCompletion.AzureOpenAI.API as AzureAPI
import qualified ChatCompletion.AzureOpenAI.Config as AzureCfg
import qualified ChatCompletion.AzureOpenAI.ToolCallingAPI as AzureToolAPI
import qualified ChatCompletion.AzureOpenAI.ToolCallingTypes as AzureToolTypes
import qualified ChatCompletion.AzureOpenAI.Types as AzureTypes
import qualified ChatCompletion.Gemini.API as GeminiAPI
import qualified ChatCompletion.Gemini.Config as GeminiCfg
import qualified ChatCompletion.Gemini.ToolCallingAPI as GeminiToolAPI
import qualified ChatCompletion.Gemini.ToolCallingTypes as GeminiToolTypes
import qualified ChatCompletion.Gemini.Types as GeminiTypes
import ChatCompletion.Interface.ToolCalling as CIT
import qualified Data.Aeson as A
import qualified Data.Text as T
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common

-- | LLM Provider type
data LLMProvider
  = AzureOpenAI Text -- deployment name
  | Gemini Text -- model name
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | LLM Message for chat completion
data LLMMessage = LLMMessage
  { role :: Text,
    content :: Text,
    toolCalls :: Maybe [CIT.ToolCall],
    toolCallId :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | LLM Service Configuration
data LLMServiceConfig = LLMServiceConfig
  { provider :: LLMProvider,
    temperature :: Maybe Double,
    maxTokens :: Maybe Int,
    topP :: Maybe Double,
    frequencyPenalty :: Maybe Double,
    presencePenalty :: Maybe Double
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | LLM Chat Completion Request
data LLMChatCompletionReq = LLMChatCompletionReq
  { messages :: [LLMMessage],
    tools :: Maybe [CIT.ToolDefinition],
    stream :: Bool
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | LLM Chat Completion Response
data LLMChatCompletionResp = LLMChatCompletionResp
  { content :: Text,
    toolCalls :: [CIT.ToolCall],
    usage :: Maybe LLMUsage,
    finishReason :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | LLM Usage statistics
data LLMUsage = LLMUsage
  { promptTokens :: Int,
    completionTokens :: Int,
    totalTokens :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | LLM Stream Chunk (for streaming responses)
data LLMStreamChunk = LLMStreamChunk
  { chunkIndex :: Int,
    chunkContent :: Text,
    chunkToolCalls :: [CIT.ToolCall],
    chunkFinishReason :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | LLM Stream Event type
data LLMStreamEvent
  = StreamContent Text
  | StreamToolCall CIT.ToolCall
  | StreamFinish Text
  | StreamError Text
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Create a user message
createUserMessage :: Text -> LLMMessage
createUserMessage content =
  LLMMessage
    { role = "user",
      content = content,
      toolCalls = Nothing,
      toolCallId = Nothing
    }

-- | Create a system message
createSystemMessage :: Text -> LLMMessage
createSystemMessage content =
  LLMMessage
    { role = "system",
      content = content,
      toolCalls = Nothing,
      toolCallId = Nothing
    }

-- | Create an assistant message
createAssistantMessage :: Text -> LLMMessage
createAssistantMessage content =
  LLMMessage
    { role = "assistant",
      content = content,
      toolCalls = Nothing,
      toolCallId = Nothing
    }

-- | Get LLM Service configuration from environment
getLLMServiceConfig :: Flow LLMServiceConfig
getLLMServiceConfig = do
  -- Default configuration - can be extended to read from merchant config
  return
    LLMServiceConfig
      { provider = AzureOpenAI "genius-south-india",
        temperature = Just 0.7,
        maxTokens = Just 2048,
        topP = Just 1.0,
        frequencyPenalty = Just 0.0,
        presencePenalty = Just 0.0
      }

-- | Perform chat completion with the configured LLM provider
chatCompletion ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    MonadReader r m,
    HasField "llmConfig" r LLMConfig
  ) =>
  LLMServiceConfig ->
  LLMChatCompletionReq ->
  m LLMChatCompletionResp
chatCompletion config req =
  case config.provider of
    AzureOpenAI deployment -> azureChatCompletion deployment req
    Gemini model -> geminiChatCompletion model req

-- | Perform chat completion with tools
chatCompletionWithTools ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    MonadReader r m,
    HasField "llmConfig" r LLMConfig
  ) =>
  LLMServiceConfig ->
  LLMChatCompletionReq ->
  m LLMChatCompletionResp
chatCompletionWithTools config req =
  case config.provider of
    AzureOpenAI deployment -> azureChatCompletionWithTools deployment req
    Gemini model -> geminiChatCompletionWithTools model req

-- | Azure OpenAI chat completion
azureChatCompletion ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    MonadReader r m,
    HasField "llmConfig" r LLMConfig
  ) =>
  Text ->
  LLMChatCompletionReq ->
  m LLMChatCompletionResp
azureChatCompletion deployment req = do
  cfg <- asks (.llmConfig.azureOpenAICfg)
  let azureReq =
        AzureTypes.ChatCompletionReq
          { messages = map convertToAzureMessage req.messages
          }
  response <- AzureAPI.chatCompletion cfg.azureOpenAIChatCompletionUrl cfg.azureApiVersion cfg.azureApiKey azureReq
  return $ convertFromAzureResponse response

-- | Azure OpenAI chat completion with tools
azureChatCompletionWithTools ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    MonadReader r m,
    HasField "llmConfig" r LLMConfig
  ) =>
  Text ->
  LLMChatCompletionReq ->
  m LLMChatCompletionResp
azureChatCompletionWithTools deployment req = do
  cfg <- asks (.llmConfig.azureOpenAICfg)
  let azureMessages = map convertToAzureToolMessage req.messages
      tools = fromMaybe [] req.tools
  response <- AzureToolAPI.chatCompletionWithTools cfg deployment azureMessages tools
  return $ convertFromAzureToolResponse response

-- | Gemini chat completion
geminiChatCompletion ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    MonadReader r m,
    HasField "llmConfig" r LLMConfig
  ) =>
  Text ->
  LLMChatCompletionReq ->
  m LLMChatCompletionResp
geminiChatCompletion model req = do
  cfg <- asks (.llmConfig.geminiCfg)
  let geminiReq =
        GeminiTypes.ContentsReq
          { contents = map convertToGeminiContent req.messages
          }
  response <- GeminiAPI.geminiChatCompletion cfg.geminiChatCompletionUrl cfg.geminiApiKey geminiReq
  return $ convertFromGeminiResponse response

-- | Gemini chat completion with tools
geminiChatCompletionWithTools ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    MonadReader r m,
    HasField "llmConfig" r LLMConfig
  ) =>
  Text ->
  LLMChatCompletionReq ->
  m LLMChatCompletionResp
geminiChatCompletionWithTools model req = do
  cfg <- asks (.llmConfig.geminiCfg)
  let geminiContents = map convertToGeminiToolContent req.messages
      tools = fromMaybe [] req.tools
  response <- GeminiToolAPI.chatCompletionWithTools cfg model geminiContents tools
  return $ convertFromGeminiToolResponse response

-- | Streaming chat completion (simulated - returns chunks)
-- Note: True streaming requires Server-Sent Events support
chatCompletionStream ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    MonadReader r m,
    HasField "llmConfig" r LLMConfig
  ) =>
  LLMServiceConfig ->
  LLMChatCompletionReq ->
  (LLMStreamChunk -> m ()) ->
  m LLMChatCompletionResp
chatCompletionStream config req chunkHandler = do
  -- For now, we simulate streaming by breaking the response into chunks
  -- In production, this would use SSE (Server-Sent Events)
  response <- chatCompletion config req
  let chunks = simulateStreaming response.content
  forM_ (zip [0 ..] chunks) $ \(idx, chunk) ->
    chunkHandler
      LLMStreamChunk
        { chunkIndex = idx,
          chunkContent = chunk,
          chunkToolCalls = [],
          chunkFinishReason = Nothing
        }
  -- Final chunk
  chunkHandler
    LLMStreamChunk
      { chunkIndex = length chunks,
      chunkContent = "",
      chunkToolCalls = response.toolCalls,
      chunkFinishReason = response.finishReason
    }
  return response

-- | Streaming chat completion with tools
chatCompletionStreamWithTools ::
  ( CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    MonadReader r m,
    HasField "llmConfig" r LLMConfig
  ) =>
  LLMServiceConfig ->
  LLMChatCompletionReq ->
  (LLMStreamChunk -> m ()) ->
  m LLMChatCompletionResp
chatCompletionStreamWithTools config req chunkHandler = do
  -- Similar to chatCompletionStream but with tool support
  response <- chatCompletionWithTools config req
  let chunks = simulateStreaming response.content
  forM_ (zip [0 ..] chunks) $ \(idx, chunk) ->
    chunkHandler
      LLMStreamChunk
        { chunkIndex = idx,
          chunkContent = chunk,
          chunkToolCalls = [],
          chunkFinishReason = Nothing
        }
  -- Final chunk with tool calls
  chunkHandler
    LLMStreamChunk
      { chunkIndex = length chunks,
        chunkContent = "",
        chunkToolCalls = response.toolCalls,
        chunkFinishReason = response.finishReason
      }
  return response

-- | Simulate streaming by splitting content into word chunks
simulateStreaming :: Text -> [Text]
simulateStreaming content =
  let words_ = T.words content
      chunkSize = 3 -- words per chunk
      chunks = chunksOf chunkSize words_
   in map T.unwords chunks

-- | Merge stream chunks into final response
mergeStreamChunks :: [LLMStreamChunk] -> LLMChatCompletionResp
mergeStreamChunks chunks =
  let content = T.concat $ map (.chunkContent) chunks
      allToolCalls = concatMap (.chunkToolCalls) chunks
      finishReason = lastMaybe chunks >>= (.chunkFinishReason)
   in LLMChatCompletionResp
        { content = content,
          toolCalls = allToolCalls,
          usage = Nothing, -- Usage not available in streaming mode
          finishReason = finishReason
        }

-- Helper functions for conversions

convertToAzureMessage :: LLMMessage -> AzureTypes.Message
convertToAzureMessage LLMMessage {..} =
  AzureTypes.Message
    { role = role,
      content = content
    }

convertFromAzureResponse :: AzureTypes.ChatCompletionResponse -> LLMChatCompletionResp
convertFromAzureResponse response =
  case response.choices of
    (choice : _) ->
      LLMChatCompletionResp
        { content = choice.message.content,
          toolCalls = [],
          usage =
            Just
              LLMUsage
                { promptTokens = response.usage.prompt_tokens,
                  completionTokens = response.usage.completion_tokens,
                  totalTokens = response.usage.total_tokens
                },
          finishReason = Just choice.finish_reason
        }
    _ ->
      LLMChatCompletionResp
        { content = "",
          toolCalls = [],
          usage = Nothing,
          finishReason = Nothing
        }

convertToAzureToolMessage :: LLMMessage -> AzureToolTypes.Message
convertToAzureToolMessage LLMMessage {..} =
  AzureToolTypes.Message
    { role = role,
      content = Just content,
      toolCalls = Nothing,
      toolCallId = toolCallId
    }

convertFromAzureToolResponse :: AzureToolTypes.ChatCompletionToolResponse -> LLMChatCompletionResp
convertFromAzureToolResponse response =
  case response.choices of
    (choice : _) ->
      let msg = choice.choiceMessage
          toolCalls = fromMaybe [] msg.toolCalls
          genericToolCalls = map convertAzureToolCall toolCalls
       in LLMChatCompletionResp
            { content = fromMaybe "" msg.content,
              toolCalls = genericToolCalls,
              usage =
                Just
                  LLMUsage
                    { promptTokens = response.usage.usagePromptTokens,
                      completionTokens = response.usage.usageCompletionTokens,
                      totalTokens = response.usage.usageTotalTokens
                    },
              finishReason = Just choice.choiceFinishReason
            }
    _ ->
      LLMChatCompletionResp
        { content = "",
          toolCalls = [],
          usage = Nothing,
          finishReason = Nothing
        }

convertAzureToolCall :: AzureToolTypes.ToolCall -> CIT.ToolCall
convertAzureToolCall tc =
  CIT.ToolCall
    { toolCallId = tc.toolCallId,
      toolCallName = tc.toolCallFunction.functionName,
      toolCallArguments = tc.toolCallFunction.functionArguments
    }

convertToGeminiContent :: LLMMessage -> GeminiTypes.Content
convertToGeminiContent LLMMessage {..} =
  GeminiTypes.Content
    { role = role,
      parts = [GeminiTypes.Part text]
    }

convertFromGeminiResponse :: GeminiTypes.ContentsResp -> LLMChatCompletionResp
convertFromGeminiResponse response =
  case response.candidates of
    (candidate : _) ->
      case candidate.content.parts of
        (part : _) ->
          LLMChatCompletionResp
            { content = fromMaybe "" part.text,
              toolCalls = [],
              usage =
                Just
                  LLMUsage
                    { promptTokens = response.usageMetadata.promptTokenCount,
                      completionTokens = response.usageMetadata.candidatesTokenCount,
                      totalTokens = response.usageMetadata.totalTokenCount
                    },
              finishReason = Just candidate.finishReason
            }
        _ ->
          LLMChatCompletionResp
            { content = "",
              toolCalls = [],
              usage = Nothing,
              finishReason = Nothing
            }
    _ ->
      LLMChatCompletionResp
        { content = "",
          toolCalls = [],
          usage = Nothing,
          finishReason = Nothing
        }

convertToGeminiToolContent :: LLMMessage -> GeminiToolTypes.Content
convertToGeminiToolContent LLMMessage {..} =
  GeminiToolTypes.Content
    { contentRole = role,
      contentParts = [GeminiToolTypes.Part (Just content) Nothing]
    }

convertFromGeminiToolResponse :: GeminiToolTypes.ContentsToolResp -> LLMChatCompletionResp
convertFromGeminiToolResponse response =
  case response.candidates of
    (candidate : _) ->
      let toolCalls = GeminiToolTypes.extractFunctionCalls response
          content = getGeminiContentText candidate
       in LLMChatCompletionResp
            { content = content,
              toolCalls = toolCalls,
              usage =
                Just
                  LLMUsage
                    { promptTokens = response.usageMetadata.promptTokenCount,
                      completionTokens = response.usageMetadata.candidatesTokenCount,
                      totalTokens = response.usageMetadata.totalTokenCount
                    },
              finishReason = Just candidate.candidateFinishReason
            }
    _ ->
      LLMChatCompletionResp
        { content = "",
          toolCalls = [],
          usage = Nothing,
          finishReason = Nothing
        }

getGeminiContentText :: GeminiToolTypes.Candidate -> Text
getGeminiContentText candidate =
  case candidate.candidateContent.contentParts of
    (part : _) -> fromMaybe "" part.partText
    _ -> ""

-- Utility function
lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just $ last xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
