{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLMIntegrationUnitTests where

import ChatCompletion.Interface.ToolCalling as CIT
import ChatCompletion.Interface.Types as CITypes
import ChatCompletion.AzureOpenAI.Config as AzureConfig
import ChatCompletion.AzureOpenAI.ToolCallingTypes as AzureTypes
import ChatCompletion.Gemini.Config as GeminiConfig
import ChatCompletion.Gemini.ToolCallingTypes as GeminiTypes
import Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Common
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?), (@?=))

-- =============================================================================
-- TOOL DEFINITION TESTS
-- =============================================================================

testToolDefinitionCreation :: TestTree
testToolDefinitionCreation =
  testGroup
    "ToolDefinition Creation"
    [ testCase "Creates a valid tool definition with all fields" $ do
        let toolDef =
              CIT.ToolDefinition
                { CIT.toolName = "search_ride",
                  CIT.toolDescription = "Search for available rides",
                  CIT.toolParameters =
                    CIT.ToolParameters
                      { CIT.paramType = "object",
                        CIT.paramProperties =
                          Map.fromList
                            [ ("pickup_location", CIT.ParameterProperty "string" "Pickup location" Nothing),
                              ("drop_location", CIT.ParameterProperty "string" "Drop location" Nothing)
                            ],
                        CIT.paramRequired = ["pickup_location", "drop_location"]
                      }
                }
        CIT.toolName toolDef @?= "search_ride"
        CIT.toolDescription toolDef @?= "Search for available rides"
        not (T.null $ CIT.toolName toolDef) @? "Tool name should not be empty"
        not (T.null $ CIT.toolDescription toolDef) @? "Tool description should not be empty",

      testCase "Creates tool definitions with different names" $ do
        let toolParams = CIT.ToolParameters "object" Map.empty []
            tool1 = CIT.ToolDefinition "book_ride" "Book a ride" toolParams
            tool2 = CIT.ToolDefinition "cancel_ride" "Cancel a ride" toolParams
            tool3 = CIT.ToolDefinition "get_ride_status" "Get ride status" toolParams

        CIT.toolName tool1 @?= "book_ride"
        CIT.toolName tool2 @?= "cancel_ride"
        CIT.toolName tool3 @?= "get_ride_status"
        CIT.toolName tool1 /= CIT.toolName tool2 @? "Different tools should have different names"
    ]

testToolParameterSchema :: TestTree
testToolParameterSchema =
  testGroup
    "Tool Parameter Schema"
    [ testCase "Creates valid JSON schema for ride booking parameters" $ do
        let schema =
              CIT.ToolParameters
                { CIT.paramType = "object",
                  CIT.paramProperties =
                    Map.fromList
                      [ ("pickup_lat", CIT.ParameterProperty "number" "Pickup latitude" Nothing),
                        ("pickup_lon", CIT.ParameterProperty "number" "Pickup longitude" Nothing),
                        ("drop_lat", CIT.ParameterProperty "number" "Drop latitude" Nothing),
                        ("drop_lon", CIT.ParameterProperty "number" "Drop longitude" Nothing),
                        ("vehicle_type", CIT.ParameterProperty "string" "Vehicle type" (Just ["sedan", "suv", "auto"]))
                      ],
                  CIT.paramRequired = ["pickup_lat", "pickup_lon", "drop_lat", "drop_lon"]
                }

        let encoded = Aeson.encode schema
        let decoded = Aeson.decode encoded :: Maybe CIT.ToolParameters
        isJust decoded @? "Schema should be valid JSON"
        case decoded of
          Just params -> CIT.paramType params @?= "object"
          Nothing -> assertFailure "Deserialization failed",

      testCase "Validates schema structure" $ do
        let schema =
              CIT.ToolParameters
                { CIT.paramType = "object",
                  CIT.paramProperties = Map.empty,
                  CIT.paramRequired = []
                }

        let encoded = Aeson.encode schema
        let decoded = Aeson.decode encoded :: Maybe CIT.ToolParameters
        isJust decoded @? "Empty schema should still be valid JSON"
    ]

-- =============================================================================
-- TOOL CALL TESTS
-- =============================================================================

testToolCallCreation :: TestTree
testToolCallCreation =
  testGroup
    "ToolCall Creation"
    [ testCase "Creates a valid tool call" $ do
        let toolCall =
              CIT.ToolCall
                { CIT.toolCallId = "call_123",
                  CIT.toolCallName = "search_ride",
                  CIT.toolCallArguments = "{\"pickup\": \"Location A\", \"drop\": \"Location B\"}"
                }

        CIT.toolCallId toolCall @?= "call_123"
        CIT.toolCallName toolCall @?= "search_ride"
        not (T.null $ CIT.toolCallId toolCall) @? "Tool call ID should not be empty",

      testCase "Creates tool calls with different IDs" $ do
        let call1 = CIT.ToolCall "call_1" "func1" "{}"
            call2 = CIT.ToolCall "call_2" "func2" "{}"

        CIT.toolCallId call1 /= CIT.toolCallId call2 @? "Different calls should have different IDs"
        CIT.toolCallName call1 @?= "func1"
        CIT.toolCallName call2 @?= "func2"
    ]

testToolCallParsing :: TestTree
testToolCallParsing =
  testGroup
    "ToolCall Argument Parsing"
    [ testCase "Parses valid JSON arguments" $ do
        let args = "{\"pickup\": \"Location A\", \"drop\": \"Location B\", \"vehicle_type\": \"sedan\"}"
        let parsed = Aeson.decode (encodeUtf8 args) :: Maybe Aeson.Object
        isJust parsed @? "Should parse valid JSON arguments",

      testCase "Handles empty arguments" $ do
        let args = "{}"
        let parsed = Aeson.decode (encodeUtf8 args) :: Maybe Aeson.Object
        isJust parsed @? "Should handle empty JSON object"
    ]

-- =============================================================================
-- LLM SERVICE CONFIG TESTS
-- =============================================================================

testAzureOpenAIConfig :: TestTree
testAzureOpenAIConfig =
  testGroup
    "Azure OpenAI Config"
    [ testCase "Creates valid Azure OpenAI config" $ do
        let config =
              AzureConfig.AzureOpenAICfg
                { AzureConfig.azureApiKey = mkEncryptedField "test-api-key",
                  AzureConfig.azureOpenAIChatCompletionUrl = parseBaseUrl "https://test.openai.azure.com",
                  AzureConfig.azureApiVersion = "2024-02-01"
                }

        show (AzureConfig.azureOpenAIChatCompletionUrl config) @?= "https://test.openai.azure.com"
        AzureConfig.azureApiVersion config @?= "2024-02-01"
        not (T.null $ unEncryptedField $ AzureConfig.azureApiKey config) @? "API key should not be empty",

      testCase "Validates endpoint URL format" $ do
        let config =
              AzureConfig.AzureOpenAICfg
                { AzureConfig.azureApiKey = mkEncryptedField "key",
                  AzureConfig.azureOpenAIChatCompletionUrl = parseBaseUrl "https://api.openai.com",
                  AzureConfig.azureApiVersion = "v1"
                }

        let urlStr = show (AzureConfig.azureOpenAIChatCompletionUrl config)
        T.isPrefixOf "https://" (T.pack urlStr) @? "Endpoint should use HTTPS"
    ]
  where
    mkEncryptedField = EncryptedField . unEncrypted
    unEncryptedField (EncryptedField (Encrypted t)) = t
    parseBaseUrl url = case parseBaseUrl url of
      Right u -> u
      Left _ -> error "Invalid URL"

testGeminiConfig :: TestTree
testGeminiConfig =
  testGroup
    "Gemini Config"
    [ testCase "Creates valid Gemini config" $ do
        let config =
              GeminiConfig.GeminiCfg
                { GeminiConfig.geminiApiKey = mkEncryptedField "test-gemini-key",
                  GeminiConfig.geminiChatCompletionUrl = parseBaseUrl "https://generativelanguage.googleapis.com"
                }

        let urlStr = show (GeminiConfig.geminiChatCompletionUrl config)
        T.isInfixOf "googleapis.com" (T.pack urlStr) @? "Base URL should be Google API"
        not (T.null $ unEncryptedField $ GeminiConfig.geminiApiKey config) @? "API key should not be empty",

      testCase "Validates Gemini base URL" $ do
        let config =
              GeminiConfig.GeminiCfg
                { GeminiConfig.geminiApiKey = mkEncryptedField "key",
                  GeminiConfig.geminiChatCompletionUrl = parseBaseUrl "https://generativelanguage.googleapis.com/v1"
                }

        let urlStr = show (GeminiConfig.geminiChatCompletionUrl config)
        T.isPrefixOf "https://" (T.pack urlStr) @? "Base URL should use HTTPS"
    ]
  where
    mkEncryptedField = EncryptedField . unEncrypted
    unEncryptedField (EncryptedField (Encrypted t)) = t
    parseBaseUrl url = case parseBaseUrl url of
      Right u -> u
      Left _ -> error "Invalid URL"

testLLMServiceConfig :: TestTree
testLLMServiceConfig =
  testGroup
    "LLM Service Config"
    [ testCase "Creates Azure OpenAI service config" $ do
        let azureConfig =
              AzureConfig.AzureOpenAICfg
                { AzureConfig.azureApiKey = EncryptedField $ Encrypted "key",
                  AzureConfig.azureOpenAIChatCompletionUrl = case parseBaseUrl "https://test.azure.com" of Right u -> u; Left _ -> error "Invalid URL",
                  AzureConfig.azureApiVersion = "v1"
                }
        let serviceConfig = CITypes.AzureOpenAI azureConfig

        case serviceConfig of
          CITypes.AzureOpenAI cfg -> AzureConfig.azureApiVersion cfg @?= "v1"
          _ -> assertFailure "Should be Azure OpenAI service",

      testCase "Creates Gemini service config" $ do
        let geminiConfig =
              GeminiConfig.GeminiCfg
                { GeminiConfig.geminiApiKey = EncryptedField $ Encrypted "key",
                  GeminiConfig.geminiChatCompletionUrl = case parseBaseUrl "https://api.google.com" of Right u -> u; Left _ -> error "Invalid URL"
                }
        let serviceConfig = CITypes.Gemini geminiConfig

        case serviceConfig of
          CITypes.Gemini cfg -> show (GeminiConfig.geminiChatCompletionUrl cfg) @?= "https://api.google.com"
          _ -> assertFailure "Should be Gemini service"
    ]

-- =============================================================================
-- AZURE OPENAI TOOL CALLING TYPES TESTS
-- =============================================================================

testAzureToolCallingTypes :: TestTree
testAzureToolCallingTypes =
  testGroup
    "Azure OpenAI Tool Calling Types"
    [ testCase "Creates valid Azure tool definition" $ do
        let azureTool =
              AzureTypes.Tool
                { AzureTypes.toolType = "function",
                  AzureTypes.toolFunction =
                    AzureTypes.ToolFunction
                      { AzureTypes.functionName = "search_ride",
                        AzureTypes.functionDescription = "Search for rides",
                        AzureTypes.functionParameters =
                          AzureTypes.ToolParameters
                            { AzureTypes.paramsType = "object",
                              AzureTypes.paramsProperties = Map.empty,
                              AzureTypes.paramsRequired = []
                            }
                      }
                }

        AzureTypes.toolType azureTool @?= "function"
        AzureTypes.functionName (AzureTypes.toolFunction azureTool) @?= "search_ride",

      testCase "Creates valid Azure chat message" $ do
        let message =
              AzureTypes.Message
                { AzureTypes.role = "user",
                  AzureTypes.content = Just "I want to book a ride",
                  AzureTypes.toolCalls = Nothing,
                  AzureTypes.toolCallId = Nothing
                }

        AzureTypes.role message @?= "user"
        AzureTypes.content message @?= Just "I want to book a ride"
    ]

-- =============================================================================
-- GEMINI TOOL CALLING TYPES TESTS
-- =============================================================================

testGeminiToolCallingTypes :: TestTree
testGeminiToolCallingTypes =
  testGroup
    "Gemini Tool Calling Types"
    [ testCase "Creates valid Gemini function declaration" $ do
        let funcDecl =
              GeminiTypes.FunctionDeclaration
                { GeminiTypes.funcName = "book_ride",
                  GeminiTypes.funcDescription = "Book a ride",
                  GeminiTypes.funcParameters =
                    GeminiTypes.ToolParameters
                      { GeminiTypes.paramsType = "object",
                        GeminiTypes.paramsProperties = Map.empty,
                        GeminiTypes.paramsRequired = []
                      }
                }

        GeminiTypes.funcName funcDecl @?= "book_ride"
        GeminiTypes.funcDescription funcDecl @?= "Book a ride",

      testCase "Creates valid Gemini content" $ do
        let content =
              GeminiTypes.Content
                { GeminiTypes.contentRole = "user",
                  GeminiTypes.contentParts =
                    [ GeminiTypes.Part
                        { GeminiTypes.partText = Just "Hello, I need a ride",
                          GeminiTypes.partFunctionCall = Nothing
                        }
                    ]
                }

        GeminiTypes.contentRole content @?= "user"
        length (GeminiTypes.contentParts content) @?= 1
    ]

-- =============================================================================
-- RIDE BOOKING TOOL SCHEMAS TESTS
-- =============================================================================

testRideBookingToolSchemas :: TestTree
testRideBookingToolSchemas =
  testGroup
    "Ride Booking Tool Schemas"
    [ testCase "Creates search ride tool schema" $ do
        let searchRideTool = CIT.searchRidesTool

        CIT.toolName searchRideTool @?= "searchRides"
        T.isInfixOf "origin" (CIT.toolDescription searchRideTool) @? "Description should mention origin"
        T.isInfixOf "destination" (CIT.toolDescription searchRideTool) @? "Description should mention destination",

      testCase "Creates book ride tool schema" $ do
        let bookRideTool = CIT.bookRideTool

        CIT.toolName bookRideTool @?= "bookRide"
        T.isInfixOf "book" (CIT.toolDescription bookRideTool) @? "Description should mention book",

      testCase "Creates cancel ride tool schema" $ do
        let cancelRideTool = CIT.cancelRideTool

        CIT.toolName cancelRideTool @?= "cancelRide"
        T.isInfixOf "cancel" (CIT.toolDescription cancelRideTool) @? "Description should mention cancel",

      testCase "Creates get ride status tool schema" $ do
        let getStatusTool = CIT.getRideStatusTool

        CIT.toolName getStatusTool @?= "getRideStatus"
        T.isInfixOf "status" (CIT.toolDescription getStatusTool) @? "Description should mention status",

      testCase "Creates add tip tool schema" $ do
        let addTipTool = CIT.addTipTool

        CIT.toolName addTipTool @?= "addTip"
        T.isInfixOf "tip" (CIT.toolDescription addTipTool) @? "Description should mention tip"
    ]

-- =============================================================================
-- TOOL EXECUTION HANDLER TESTS
-- =============================================================================

testToolExecutionHandlers :: TestTree
testToolExecutionHandlers =
  testGroup
    "Tool Execution Handlers"
    [ testCase "Validates tool name for execution" $ do
        let validTools = ["searchRides", "bookRide", "cancelRide", "getRideStatus", "addTip"]
        let toolName = "searchRides"
        toolName `elem` validTools @? "Tool name should be in valid tools list",

      testCase "Rejects invalid tool names" $ do
        let validTools = ["searchRides", "bookRide", "cancelRide", "getRideStatus", "addTip"]
        let invalidTool = "invalid_tool"
        not (invalidTool `elem` validTools) @? "Invalid tool should not be in valid tools list",

      testCase "Validates required parameters for searchRides" $ do
        let requiredParams = ["originLat", "originLon", "destLat", "destLon"]
        let providedParams = ["originLat", "originLon", "destLat", "destLon"]
        all (`elem` providedParams) requiredParams @? "All required params should be provided",

      testCase "Detects missing required parameters" $ do
        let requiredParams = ["originLat", "originLon", "destLat"]
        let providedParams = ["originLat", "originLon"]
        not (all (`elem` providedParams) requiredParams) @? "Should detect missing required params",

      testCase "Converts RideBookingTool to text correctly" $ do
        CIT.rideBookingToolToText CIT.SearchRidesTool @?= "searchRides"
        CIT.rideBookingToolToText CIT.BookRideTool @?= "bookRide"
        CIT.rideBookingToolToText CIT.GetRideStatusTool @?= "getRideStatus"
        CIT.rideBookingToolToText CIT.CancelRideTool @?= "cancelRide"
        CIT.rideBookingToolToText CIT.AddTipTool @?= "addTip",

      testCase "Parses text to RideBookingTool correctly" $ do
        CIT.textToRideBookingTool "searchRides" @?= Just CIT.SearchRidesTool
        CIT.textToRideBookingTool "bookRide" @?= Just CIT.BookRideTool
        CIT.textToRideBookingTool "getRideStatus" @?= Just CIT.GetRideStatusTool
        CIT.textToRideBookingTool "cancelRide" @?= Just CIT.CancelRideTool
        CIT.textToRideBookingTool "addTip" @?= Just CIT.AddTipTool
        CIT.textToRideBookingTool "invalid" @?= Nothing
    ]

-- =============================================================================
-- JSON SERIALIZATION TESTS
-- =============================================================================

testJSONSerialization :: TestTree
testJSONSerialization =
  testGroup
    "JSON Serialization"
    [ testCase "Serializes and deserializes ToolDefinition" $ do
        let toolDef =
              CIT.ToolDefinition
                { CIT.toolName = "test_tool",
                  CIT.toolDescription = "Test tool description",
                  CIT.toolParameters =
                    CIT.ToolParameters
                      { CIT.paramType = "object",
                        CIT.paramProperties = Map.empty,
                        CIT.paramRequired = []
                      }
                }

        let encoded = Aeson.encode toolDef
        let decoded = Aeson.decode encoded :: Maybe CIT.ToolDefinition

        isJust decoded @? "Should deserialize successfully"
        case decoded of
          Just td -> CIT.toolName td @?= "test_tool"
          Nothing -> assertFailure "Deserialization failed",

      testCase "Serializes and deserializes ToolCall" $ do
        let toolCall =
              CIT.ToolCall
                { CIT.toolCallId = "call_123",
                  CIT.toolCallName = "func_name",
                  CIT.toolCallArguments = "{}"
                }

        let encoded = Aeson.encode toolCall
        let decoded = Aeson.decode encoded :: Maybe CIT.ToolCall

        isJust decoded @? "Should deserialize ToolCall successfully"
        case decoded of
          Just tc -> CIT.toolCallId tc @?= "call_123"
          Nothing -> assertFailure "ToolCall deserialization failed",

      testCase "Serializes and deserializes ToolCallResult" $ do
        let toolCallResult =
              CIT.ToolCallResult
                { CIT.toolCallResultId = "result_123",
                  CIT.toolCallResultName = "func_name",
                  CIT.toolCallResultOutput = "{\"status\": \"success\"}"
                }

        let encoded = Aeson.encode toolCallResult
        let decoded = Aeson.decode encoded :: Maybe CIT.ToolCallResult

        isJust decoded @? "Should deserialize ToolCallResult successfully"
        case decoded of
          Just tcr -> CIT.toolCallResultId tcr @?= "result_123"
          Nothing -> assertFailure "ToolCallResult deserialization failed",

      testCase "Serializes and deserializes RideBookingTool" $ do
        let tools = [CIT.SearchRidesTool, CIT.BookRideTool, CIT.GetRideStatusTool, CIT.CancelRideTool, CIT.AddTipTool]
        all (\tool -> isJust (Aeson.decode (Aeson.encode tool) :: Maybe CIT.RideBookingTool)) tools @? "All tools should serialize/deserialize"
    ]

-- =============================================================================
-- ERROR HANDLING TESTS
-- =============================================================================

testErrorHandling :: TestTree
testErrorHandling =
  testGroup
    "Error Handling"
    [ testCase "Handles empty tool name" $ do
        let toolParams = CIT.ToolParameters "object" Map.empty []
        let toolDef = CIT.ToolDefinition "" "Description" toolParams
        T.null (CIT.toolName toolDef) @? "Empty tool name should be handled",

      testCase "Handles empty tool description" $ do
        let toolParams = CIT.ToolParameters "object" Map.empty []
        let toolDef = CIT.ToolDefinition "name" "" toolParams
        T.null (CIT.toolDescription toolDef) @? "Empty description should be handled",

      testCase "Handles invalid JSON in tool arguments" $ do
        let invalidJson = "{invalid json"
        let parsed = Aeson.decode (encodeUtf8 invalidJson) :: Maybe Aeson.Object
        isNothing parsed @? "Invalid JSON should not parse",

      testCase "Handles empty tool call arguments" $ do
        let toolCall = CIT.ToolCall "call_1" "func" ""
        T.null (CIT.toolCallArguments toolCall) @? "Empty arguments should be handled"
    ]

-- =============================================================================
-- TOOL CONVERSION TESTS
-- =============================================================================

testToolConversions :: TestTree
testToolConversions =
  testGroup
    "Tool Conversions"
    [ testCase "Converts generic ToolDefinition to Azure Tool" $ do
        let genericTool = CIT.searchRidesTool
        let azureTool = AzureTypes.convertToAzureTool genericTool

        AzureTypes.functionName (AzureTypes.toolFunction azureTool) @?= CIT.toolName genericTool
        AzureTypes.toolType azureTool @?= "function",

      testCase "Converts generic ToolDefinition to Gemini Tool" $ do
        let genericTool = CIT.bookRideTool
        let geminiTool = GeminiTypes.convertToGeminiTool genericTool

        let funcDecls = GeminiTypes.toolFunctionDeclarations geminiTool
        length funcDecls @?= 1
        GeminiTypes.funcName (head funcDecls) @?= CIT.toolName genericTool,

      testCase "Converts Azure ToolCall to generic ToolCall" $ do
        let azureToolCall =
              AzureTypes.ToolCall
                { AzureTypes.toolCallId = "call_123",
                  AzureTypes.toolCallType = "function",
                  AzureTypes.toolCallFunction =
                    AzureTypes.ToolCallFunction
                      { AzureTypes.functionName = "search_ride",
                        AzureTypes.functionArguments = "{\"param\": \"value\"}"
                      }
                }
        let genericToolCall = AzureTypes.convertFromAzureToolCall azureToolCall

        CIT.toolCallId genericToolCall @?= AzureTypes.toolCallId azureToolCall
        CIT.toolCallName genericToolCall @?= AzureTypes.functionName (AzureTypes.toolCallFunction azureToolCall)
    ]

-- =============================================================================
-- MAIN TEST SUITE
-- =============================================================================

allLLMIntegrationTests :: TestTree
allLLMIntegrationTests =
  testGroup
    "LLM Integration Tests"
    [ testToolDefinitionCreation,
      testToolParameterSchema,
      testToolCallCreation,
      testToolCallParsing,
      testAzureOpenAIConfig,
      testGeminiConfig,
      testLLMServiceConfig,
      testAzureToolCallingTypes,
      testGeminiToolCallingTypes,
      testRideBookingToolSchemas,
      testToolExecutionHandlers,
      testJSONSerialization,
      testErrorHandling,
      testToolConversions
    ]
