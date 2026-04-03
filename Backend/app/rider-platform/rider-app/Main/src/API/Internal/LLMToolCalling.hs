{-# OPTIONS_GHC -Wno-orphans #-}

module API.Internal.LLMToolCalling where

import qualified ChatCompletion.AzureOpenAI.ToolCallingAPI as AzureToolAPI
import qualified ChatCompletion.AzureOpenAI.ToolCallingTypes as AzureToolTypes
import qualified ChatCompletion.Gemini.ToolCallingAPI as GeminiToolAPI
import qualified ChatCompletion.Gemini.ToolCallingTypes as GeminiToolTypes
import qualified ChatCompletion.AzureOpenAI.Config as AzureOpenAICfg
import qualified ChatCompletion.Gemini.Config as GeminiCfg
import ChatCompletion.Interface.ToolCalling as CIT
import Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Action.LLM.RideBookingTools as RideBookingTools
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type LLMToolCallingAPI =
  "llm"
    :> "tool-calling"
    :> Capture "personId" (Id Person)
    :> ReqBody '[JSON] LLMToolCallingReq
    :> Post '[JSON] LLMToolCallingResp

data LLMToolCallingReq = LLMToolCallingReq
  { provider :: LLMProvider,
    messages :: [LLMMessage],
    maxIterations :: Maybe Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data LLMProvider
  = AzureOpenAI Text -- deployment name
  | Gemini Text -- model name
  deriving (Show, Generic, ToJSON, FromJSON)

data LLMMessage = LLMMessage
  { role :: Text,
    content :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data LLMToolCallingResp = LLMToolCallingResp
  { finalResponse :: Text,
    toolCalls :: [ToolCallResult],
    iterations :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ToolCallResult = ToolCallResult
  { toolName :: Text,
    toolArguments :: Text,
    toolResult :: A.Value
  }
  deriving (Show, Generic, ToJSON, FromJSON)

handler :: FlowServer LLMToolCallingAPI
handler = handleLLMToolCalling

handleLLMToolCalling ::
  Id Person ->
  LLMToolCallingReq ->
  FlowHandler LLMToolCallingResp
handleLLMToolCalling personId req = withFlowHandlerAPI $ do
  let tools = RideBookingTools.rideBookingToolDefinitions
      maxIter = fromMaybe 5 req.maxIterations

  -- Convert messages to provider-specific format
  case req.provider of
    AzureOpenAI deployment -> handleAzureToolCalling personId deployment req.messages tools maxIter
    Gemini model -> handleGeminiToolCalling personId model req.messages tools maxIter

handleAzureToolCalling ::
  Id Person ->
  Text ->
  [LLMMessage] ->
  [CIT.ToolDefinition] ->
  Int ->
  Flow LLMToolCallingResp
handleAzureToolCalling personId deployment messages tools maxIter = do
  cfg <- asks (.llmConfig.azureOpenAICfg)

  let azureMessages = map convertToAzureMessage messages

  -- Initial call
  response <- AzureToolAPI.chatCompletionWithTools cfg deployment azureMessages tools

  -- Process tool calls iteratively
  processAzureToolCalls personId cfg deployment azureMessages tools maxIter 0 [] response

convertToAzureMessage :: LLMMessage -> AzureToolTypes.Message
convertToAzureMessage LLMMessage {..} =
  AzureToolTypes.Message
    { role = role,
      content = Just content,
      toolCalls = Nothing,
      toolCallId = Nothing
    }

processAzureToolCalls ::
  Id Person ->
  AzureToolAPI.CAC.AzureOpenAICfg ->
  Text ->
  [AzureToolTypes.Message] ->
  [CIT.ToolDefinition] ->
  Int ->
  Int ->
  [ToolCallResult] ->
  AzureToolTypes.ChatCompletionToolResponse ->
  Flow LLMToolCallingResp
processAzureToolCalls personId cfg deployment messages tools maxIter currentIter results response
  | currentIter >= maxIter = do
      -- Max iterations reached, return final response
      let finalContent = getFinalContent response
      return
        LLMToolCallingResp
          { finalResponse = finalContent,
            toolCalls = results,
            iterations = currentIter
          }
  | otherwise = do
      case getToolCalls response of
        [] -> do
          -- No tool calls, return final response
          let finalContent = getFinalContent response
          return
            LLMToolCallingResp
              { finalResponse = finalContent,
                toolCalls = results,
                iterations = currentIter
              }
        toolCalls -> do
          -- Execute tool calls and continue
          (newResults, toolResponseMessages) <- executeAzureToolCalls personId toolCalls
          let allResults = results ++ newResults
              newMessages = messages ++ toolResponseMessages

          -- Make another LLM call with tool results
          newResponse <- AzureToolAPI.chatCompletionWithTools cfg deployment newMessages tools
          processAzureToolCalls personId cfg deployment newMessages tools maxIter (currentIter + 1) allResults newResponse

getToolCalls :: AzureToolTypes.ChatCompletionToolResponse -> [AzureToolTypes.ToolCall]
getToolCalls response =
  case response.choices of
    (choice : _) -> fromMaybe [] choice.choiceMessage.toolCalls
    _ -> []

getFinalContent :: AzureToolTypes.ChatCompletionToolResponse -> Text
getFinalContent response =
  case response.choices of
    (choice : _) -> fromMaybe "" choice.choiceMessage.content
    _ -> ""

executeAzureToolCalls ::
  Id Person ->
  [AzureToolTypes.ToolCall] ->
  Flow ([ToolCallResult], [AzureToolTypes.Message])
executeAzureToolCalls personId toolCalls = do
  results <- forM toolCalls $ \toolCall -> do
    let genericToolCall = AzureToolTypes.convertFromAzureToolCall toolCall
    result <- RideBookingTools.executeToolCall personId genericToolCall
    return
      ( ToolCallResult
          { toolName = genericToolCall.toolCallName,
            toolArguments = genericToolCall.toolCallArguments,
            toolResult = result
          },
        AzureToolTypes.buildToolResponseMessage toolCall.toolCallId genericToolCall.toolCallName (T.pack $ show result)
      )

  let toolResults = map fst results
      toolMessages = map snd results

  return (toolResults, toolMessages)

handleGeminiToolCalling ::
  Id Person ->
  Text ->
  [LLMMessage] ->
  [CIT.ToolDefinition] ->
  Int ->
  Flow LLMToolCallingResp
handleGeminiToolCalling personId model messages tools maxIter = do
  cfg <- asks (.llmConfig.geminiCfg)

  let geminiContents = map convertToGeminiContent messages

  -- Initial call
  response <- GeminiToolAPI.chatCompletionWithTools cfg model geminiContents tools

  -- Process tool calls iteratively
  processGeminiToolCalls personId cfg model geminiContents tools maxIter 0 [] response

convertToGeminiContent :: LLMMessage -> GeminiToolTypes.Content
convertToGeminiContent LLMMessage {..} =
  GeminiToolTypes.Content
    { contentRole = role,
      contentParts = [GeminiToolTypes.Part (Just content) Nothing]
    }

processGeminiToolCalls ::
  Id Person ->
  GeminiToolAPI.CGC.GeminiCfg ->
  Text ->
  [GeminiToolTypes.Content] ->
  [CIT.ToolDefinition] ->
  Int ->
  Int ->
  [ToolCallResult] ->
  GeminiToolTypes.ContentsToolResp ->
  Flow LLMToolCallingResp
processGeminiToolCalls personId cfg model contents tools maxIter currentIter results response
  | currentIter >= maxIter = do
      -- Max iterations reached, return final response
      let finalContent = getGeminiFinalContent response
      return
        LLMToolCallingResp
          { finalResponse = finalContent,
            toolCalls = results,
            iterations = currentIter
          }
  | otherwise = do
      let toolCalls = GeminiToolTypes.extractFunctionCalls response
      if null toolCalls
        then do
          -- No tool calls, return final response
          let finalContent = getGeminiFinalContent response
          return
            LLMToolCallingResp
              { finalResponse = finalContent,
                toolCalls = results,
                iterations = currentIter
              }
        else do
          -- Execute tool calls and continue
          (newResults, toolResponseContents) <- executeGeminiToolCalls personId toolCalls
          let allResults = results ++ newResults
              newContents = contents ++ toolResponseContents

          -- Make another LLM call with tool results
          newResponse <- GeminiToolAPI.chatCompletionWithTools cfg model newContents tools
          processGeminiToolCalls personId cfg model newContents tools maxIter (currentIter + 1) allResults newResponse

getGeminiFinalContent :: GeminiToolTypes.ContentsToolResp -> Text
getGeminiFinalContent response =
  case response.candidates of
    (candidate : _) ->
      case candidate.candidateContent.contentParts of
        (part : _) -> fromMaybe "" part.partText
        _ -> ""
    _ -> ""

executeGeminiToolCalls ::
  Id Person ->
  [CIT.ToolCall] ->
  Flow ([ToolCallResult], [GeminiToolTypes.Content])
executeGeminiToolCalls personId toolCalls = do
  results <- forM toolCalls $ \toolCall -> do
    result <- RideBookingTools.executeToolCall personId toolCall
    return
      ( ToolCallResult
          { toolName = toolCall.toolCallName,
            toolArguments = toolCall.toolCallArguments,
            toolResult = result
          },
        GeminiToolTypes.buildFunctionResponseContent toolCall.toolCallName result
      )

  let toolResults = map fst results
      toolContents = map snd results

  return (toolResults, toolContents)
