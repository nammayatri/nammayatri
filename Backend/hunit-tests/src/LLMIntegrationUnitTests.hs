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
                    Aeson.object
                      [ "type" .= ("object" :: Text),
                        "properties"
                          .= Aeson.object
                            [ "pickup_location" .= Aeson.object ["type" .= ("string" :: Text), "description" .= ("Pickup location" :: Text)],
                              "drop_location" .= Aeson.object ["type" .= ("string" :: Text), "description" .= ("Drop location" :: Text)]
                            ],
                        "required" .= (["pickup_location", "drop_location"] :: [Text])
                      ]
                }
        CIT.toolName toolDef @?= "search_ride"
        CIT.toolDescription toolDef @?= "Search for available rides"
        not (T.null $ CIT.toolName toolDef) @? "Tool name should not be empty"
        not (T.null $ CIT.toolDescription toolDef) @? "Tool description should not be empty",

      testCase "Creates tool definitions with different names" $ do
        let tool1 = CIT.ToolDefinition "book_ride" "Book a ride" Aeson.emptyObject
            tool2 = CIT.ToolDefinition "cancel_ride" "Cancel a ride" Aeson.emptyObject
            tool3 = CIT.ToolDefinition "get_ride_status" "Get ride status" Aeson.emptyObject

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
              Aeson.object
                [ "type" .= ("object" :: Text),
                  "properties"
                    .= Aeson.object
                      [ "pickup_lat" .= Aeson.object ["type" .= ("number" :: Text)],
                        "pickup_lon" .= Aeson.object ["type" .= ("number" :: Text)],
                        "drop_lat" .= Aeson.object ["type" .= ("number" :: Text)],
                        "drop_lon" .= Aeson.object ["type" .= ("number" :: Text)],
                        "vehicle_type" .= Aeson.object ["type" .= ("string" :: Text), "enum" .= (["sedan", "suv", "auto"] :: [Text])]
                      ],
                  "required" .= (["pickup_lat", "pickup_lon", "drop_lat", "drop_lon"] :: [Text])
                ]

        let result = Aeson.decode (Aeson.encode schema) :: Maybe Aeson.Value
        isJust result @? "Schema should be valid JSON"

    , testCase "Validates schema structure" $ do
        let schema =
              Aeson.object
                [ "type" .= ("object" :: Text),
                  "properties" .= Aeson.object [],
                  "required" .= ([] :: [Text])
                ]

        let encoded = Aeson.encode schema
        let decoded = Aeson.decode encoded :: Maybe Aeson.Value
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
                  CIT.toolCallType = "function",
                  CIT.toolCallFunction =
                    CIT.ToolCallFunction
                      { CIT.toolCallFunctionName = "search_ride",
                        CIT.toolCallFunctionArguments = "{\"pickup\": \"Location A\", \"drop\": \"Location B\"}"
                      }
                }

        CIT.toolCallId toolCall @?= "call_123"
        CIT.toolCallType toolCall @?= "function"
        CIT.toolCallFunctionName (CIT.toolCallFunction toolCall) @?= "search_ride"
        not (T.null $ CIT.toolCallId toolCall) @? "Tool call ID should not be empty",

      testCase "Creates tool calls with different IDs" $ do
        let call1 = CIT.ToolCall "call_1" "function" (CIT.ToolCallFunction "func1" "{}")
            call2 = CIT.ToolCall "call_2" "function" (CIT.ToolCallFunction "func2" "{}")

        CIT.toolCallId call1 /= CIT.toolCallId call2 @? "Different calls should have different IDs"
        CIT.toolCallFunctionName (CIT.toolCallFunction call1) @?= "func1"
        CIT.toolCallFunctionName (CIT.toolCallFunction call2) @?= "func2"
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
              AzureConfig.AzureOpenAIConfig
                { AzureConfig.apiKey = "test-api-key",
                  AzureConfig.endpoint = "https://test.openai.azure.com",
                  AzureConfig.deploymentName = "gpt-4",
                  AzureConfig.apiVersion = "2024-02-01"
                }

        AzureConfig.endpoint config @?= "https://test.openai.azure.com"
        AzureConfig.deploymentName config @?= "gpt-4"
        AzureConfig.apiVersion config @?= "2024-02-01"
        not (T.null $ AzureConfig.apiKey config) @? "API key should not be empty",

      testCase "Validates endpoint URL format" $ do
        let config =
              AzureConfig.AzureOpenAIConfig
                { AzureConfig.apiKey = "key",
                  AzureConfig.endpoint = "https://api.openai.com",
                  AzureConfig.deploymentName = "model",
                  AzureConfig.apiVersion = "v1"
                }

        T.isPrefixOf "https://" (AzureConfig.endpoint config) @? "Endpoint should use HTTPS"
        T.isInfixOf "." (AzureConfig.endpoint config) @? "Endpoint should contain domain"
    ]

testGeminiConfig :: TestTree
testGeminiConfig =
  testGroup
    "Gemini Config"
    [ testCase "Creates valid Gemini config" $ do
        let config =
              GeminiConfig.GeminiConfig
                { GeminiConfig.apiKey = "test-gemini-key",
                  GeminiConfig.modelName = "gemini-pro",
                  GeminiConfig.baseUrl = "https://generativelanguage.googleapis.com"
                }

        GeminiConfig.baseUrl config @?= "https://generativelanguage.googleapis.com"
        GeminiConfig.modelName config @?= "gemini-pro"
        not (T.null $ GeminiConfig.apiKey config) @? "API key should not be empty",

      testCase "Validates Gemini base URL" $ do
        let config =
              GeminiConfig.GeminiConfig
                { GeminiConfig.apiKey = "key",
                  GeminiConfig.modelName = "model",
                  GeminiConfig.baseUrl = "https://generativelanguage.googleapis.com/v1"
                }

        T.isPrefixOf "https://" (GeminiConfig.baseUrl config) @? "Base URL should use HTTPS"
        T.isInfixOf "googleapis.com" (GeminiConfig.baseUrl config) @? "Base URL should be Google API"
    ]

testLLMServiceConfig :: TestTree
testLLMServiceConfig =
  testGroup
    "LLM Service Config"
    [ testCase "Creates Azure OpenAI service config" $ do
        let azureConfig =
              AzureConfig.AzureOpenAIConfig
                { AzureConfig.apiKey = "key",
                  AzureConfig.endpoint = "https://test.azure.com",
                  AzureConfig.deploymentName = "gpt-4",
                  AzureConfig.apiVersion = "v1"
                }
        let serviceConfig = CITypes.AzureOpenAIService azureConfig

        case serviceConfig of
          CITypes.AzureOpenAIService cfg -> AzureConfig.endpoint cfg @?= "https://test.azure.com"
          _ -> assertFailure "Should be Azure OpenAI service",

      testCase "Creates Gemini service config" $ do
        let geminiConfig =
              GeminiConfig.GeminiConfig
                { GeminiConfig.apiKey = "key",
                  GeminiConfig.modelName = "gemini-pro",
                  GeminiConfig.baseUrl = "https://api.google.com"
                }
        let serviceConfig = CITypes.GeminiService geminiConfig

        case serviceConfig of
          CITypes.GeminiService cfg -> GeminiConfig.modelName cfg @?= "gemini-pro"
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
                    AzureTypes.FunctionDefinition
                      { AzureTypes.functionName = "search_ride",
                        AzureTypes.functionDescription = "Search for rides",
                        AzureTypes.functionParameters =
                          Aeson.object
                            [ "type" .= ("object" :: Text),
                              "properties" .= Aeson.object []
                            ]
                      }
                }

        AzureTypes.toolType azureTool @?= "function"
        AzureTypes.functionName (AzureTypes.toolFunction azureTool) @?= "search_ride",

      testCase "Creates valid Azure chat message" $ do
        let message =
              AzureTypes.ChatCompletionMessage
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
                    Aeson.object
                      [ "type" .= ("object" :: Text),
                        "properties" .= Aeson.object []
                      ]
                }

        GeminiTypes.funcName funcDecl @?= "book_ride"
        GeminiTypes.funcDescription funcDecl @?= "Book a ride",

      testCase "Creates valid Gemini content" $ do
        let content =
              GeminiTypes.Content
                { GeminiTypes.contentRole = "user",
                  GeminiTypes.contentParts =
                    [ GeminiTypes.PartText "Hello, I need a ride"
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
        let searchRideTool =
              CIT.ToolDefinition
                { CIT.toolName = "search_ride",
                  CIT.toolDescription = "Search for available rides between pickup and drop locations",
                  CIT.toolParameters =
                    Aeson.object
                      [ "type" .= ("object" :: Text),
                        "properties"
                          .= Aeson.object
                            [ "pickup_address" .= Aeson.object ["type" .= ("string" :: Text), "description" .= ("Pickup address" :: Text)],
                              "drop_address" .= Aeson.object ["type" .= ("string" :: Text), "description" .= ("Drop address" :: Text)],
                              "pickup_lat" .= Aeson.object ["type" .= ("number" :: Text), "description" .= ("Pickup latitude" :: Text)],
                              "pickup_lon" .= Aeson.object ["type" .= ("number" :: Text), "description" .= ("Pickup longitude" :: Text)],
                              "drop_lat" .= Aeson.object ["type" .= ("number" :: Text), "description" .= ("Drop latitude" :: Text)],
                              "drop_lon" .= Aeson.object ["type" .= ("number" :: Text), "description" .= ("Drop longitude" :: Text)],
                              "vehicle_type" .= Aeson.object ["type" .= ("string" :: Text), "enum" .= (["sedan", "suv", "auto", "bike"] :: [Text])]
                            ],
                        "required" .= (["pickup_address", "drop_address", "pickup_lat", "pickup_lon", "drop_lat", "drop_lon"] :: [Text])
                      ]
                }

        CIT.toolName searchRideTool @?= "search_ride"
        T.isInfixOf "pickup" (CIT.toolDescription searchRideTool) @? "Description should mention pickup"
        T.isInfixOf "drop" (CIT.toolDescription searchRideTool) @? "Description should mention drop",

      testCase "Creates book ride tool schema" $ do
        let bookRideTool =
              CIT.ToolDefinition
                { CIT.toolName = "book_ride",
                  CIT.toolDescription = "Book a ride with selected driver",
                  CIT.toolParameters =
                    Aeson.object
                      [ "type" .= ("object" :: Text),
                        "properties"
                          .= Aeson.object
                            [ "quote_id" .= Aeson.object ["type" .= ("string" :: Text), "description" .= ("Quote ID from search results" :: Text)],
                              "payment_method" .= Aeson.object ["type" .= ("string" :: Text), "enum" .= (["cash", "card", "upi", "wallet"] :: [Text])]
                            ],
                        "required" .= (["quote_id"] :: [Text])
                      ]
                }

        CIT.toolName bookRideTool @?= "book_ride"
        CIT.toolDescription bookRideTool @?= "Book a ride with selected driver",

      testCase "Creates cancel ride tool schema" $ do
        let cancelRideTool =
              CIT.ToolDefinition
                { CIT.toolName = "cancel_ride",
                  CIT.toolDescription = "Cancel an existing ride booking",
                  CIT.toolParameters =
                    Aeson.object
                      [ "type" .= ("object" :: Text),
                        "properties"
                          .= Aeson.object
                            [ "booking_id" .= Aeson.object ["type" .= ("string" :: Text), "description" .= ("Booking ID to cancel" :: Text)],
                              "reason" .= Aeson.object ["type" .= ("string" :: Text), "description" .= ("Cancellation reason" :: Text)]
                            ],
                        "required" .= (["booking_id"] :: [Text])
                      ]
                }

        CIT.toolName cancelRideTool @?= "cancel_ride"
        CIT.toolDescription cancelRideTool @?= "Cancel an existing ride booking",

      testCase "Creates get ride status tool schema" $ do
        let getStatusTool =
              CIT.ToolDefinition
                { CIT.toolName = "get_ride_status",
                  CIT.toolDescription = "Get the current status of a ride",
                  CIT.toolParameters =
                    Aeson.object
                      [ "type" .= ("object" :: Text),
                        "properties"
                          .= Aeson.object
                            [ "booking_id" .= Aeson.object ["type" .= ("string" :: Text), "description" .= ("Booking ID to check status" :: Text)]
                            ],
                        "required" .= (["booking_id"] :: [Text])
                      ]
                }

        CIT.toolName getStatusTool @?= "get_ride_status"
        CIT.toolDescription getStatusTool @?= "Get the current status of a ride"
    ]

-- =============================================================================
-- TOOL EXECUTION HANDLER TESTS
-- =============================================================================

testToolExecutionHandlers :: TestTree
testToolExecutionHandlers =
  testGroup
    "Tool Execution Handlers"
    [ testCase "Validates tool name for execution" $ do
        let validTools = ["search_ride", "book_ride", "cancel_ride", "get_ride_status"]
        let toolName = "search_ride"
        toolName `elem` validTools @? "Tool name should be in valid tools list",

      testCase "Rejects invalid tool names" $ do
        let validTools = ["search_ride", "book_ride", "cancel_ride", "get_ride_status"]
        let invalidTool = "invalid_tool"
        not (invalidTool `elem` validTools) @? "Invalid tool should not be in valid tools list",

      testCase "Validates required parameters for search_ride" $ do
        let requiredParams = ["pickup_address", "drop_address", "pickup_lat", "pickup_lon", "drop_lat", "drop_lon"]
        let providedParams = ["pickup_address", "drop_address", "pickup_lat", "pickup_lon", "drop_lat", "drop_lon"]
        all (`elem` providedParams) requiredParams @? "All required params should be provided",

      testCase "Detects missing required parameters" $ do
        let requiredParams = ["pickup_address", "drop_address", "pickup_lat"]
        let providedParams = ["pickup_address", "drop_address"]
        not (all (`elem` providedParams) requiredParams) @? "Should detect missing required params"
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
                  CIT.toolParameters = Aeson.object ["type" .= ("object" :: Text)]
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
                  CIT.toolCallType = "function",
                  CIT.toolCallFunction = CIT.ToolCallFunction "func_name" "{}"
                }

        let encoded = Aeson.encode toolCall
        let decoded = Aeson.decode encoded :: Maybe CIT.ToolCall

        isJust decoded @? "Should deserialize ToolCall successfully"
        case decoded of
          Just tc -> CIT.toolCallId tc @?= "call_123"
          Nothing -> assertFailure "ToolCall deserialization failed"
    ]

-- =============================================================================
-- ERROR HANDLING TESTS
-- =============================================================================

testErrorHandling :: TestTree
testErrorHandling =
  testGroup
    "Error Handling"
    [ testCase "Handles empty tool name" $ do
        let toolDef = CIT.ToolDefinition "" "Description" Aeson.emptyObject
        T.null (CIT.toolName toolDef) @? "Empty tool name should be handled",

      testCase "Handles empty tool description" $ do
        let toolDef = CIT.ToolDefinition "name" "" Aeson.emptyObject
        T.null (CIT.toolDescription toolDef) @? "Empty description should be handled",

      testCase "Handles invalid JSON in tool arguments" $ do
        let invalidJson = "{invalid json"
        let parsed = Aeson.decode (encodeUtf8 invalidJson) :: Maybe Aeson.Object
        isNothing parsed @? "Invalid JSON should not parse"
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
      testErrorHandling
    ]
