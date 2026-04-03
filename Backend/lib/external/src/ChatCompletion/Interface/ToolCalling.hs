{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ChatCompletion.Interface.ToolCalling where

import Kernel.Prelude
import Data.Aeson.Types

-- | Tool/Function definition for LLM function calling
data ToolDefinition = ToolDefinition
  { toolName :: Text,
    toolDescription :: Text,
    toolParameters :: ToolParameters
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Tool parameters schema
data ToolParameters = ToolParameters
  { paramType :: Text,
    paramProperties :: Map Text ParameterProperty,
    paramRequired :: [Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Individual parameter property
data ParameterProperty = ParameterProperty
  { propType :: Text,
    propDescription :: Text,
    propEnum :: Maybe [Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Tool call request from LLM
data ToolCall = ToolCall
  { toolCallId :: Text,
    toolCallName :: Text,
    toolCallArguments :: Text -- JSON string of arguments
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Tool call result to send back to LLM
data ToolCallResult = ToolCallResult
  { toolCallResultId :: Text,
    toolCallResultName :: Text,
    toolCallResultOutput :: Text -- JSON string of result
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Message with tool calls from LLM
data ToolCallMessage = ToolCallMessage
  { toolCallMessageRole :: Text,
    toolCallMessageContent :: Maybe Text,
    toolCallMessageToolCalls :: Maybe [ToolCall]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Tool response message
data ToolResponseMessage = ToolResponseMessage
  { toolResponseMessageRole :: Text,
    toolResponseMessageToolCallId :: Text,
    toolResponseMessageContent :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Ride booking tool types
data RideBookingTool
  = SearchRidesTool
  | BookRideTool
  | GetRideStatusTool
  | CancelRideTool
  | AddTipTool
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

rideBookingToolToText :: RideBookingTool -> Text
rideBookingToolToText = \case
  SearchRidesTool -> "searchRides"
  BookRideTool -> "bookRide"
  GetRideStatusTool -> "getRideStatus"
  CancelRideTool -> "cancelRide"
  AddTipTool -> "addTip"

textToRideBookingTool :: Text -> Maybe RideBookingTool
textToRideBookingTool = \case
  "searchRides" -> Just SearchRidesTool
  "bookRide" -> Just BookRideTool
  "getRideStatus" -> Just GetRideStatusTool
  "cancelRide" -> Just CancelRideTool
  "addTip" -> Just AddTipTool
  _ -> Nothing

-- | Get tool definitions for ride booking
getRideBookingToolDefinitions :: [ToolDefinition]
getRideBookingToolDefinitions =
  [ searchRidesTool,
    bookRideTool,
    getRideStatusTool,
    cancelRideTool,
    addTipTool
  ]

searchRidesTool :: ToolDefinition
searchRidesTool =
  ToolDefinition
    { toolName = "searchRides",
      toolDescription = "Search for available rides between origin and destination locations. Returns a list of available ride options with estimates.",
      toolParameters =
        ToolParameters
          { paramType = "object",
            paramProperties =
              Map.fromList
                [ ("originLat", ParameterProperty "number" "Latitude of the origin location" Nothing),
                  ("originLon", ParameterProperty "number" "Longitude of the origin location" Nothing),
                  ("originAddress", ParameterProperty "string" "Human-readable address of the origin" Nothing),
                  ("destLat", ParameterProperty "number" "Latitude of the destination location" Nothing),
                  ("destLon", ParameterProperty "number" "Longitude of the destination location" Nothing),
                  ("destAddress", ParameterProperty "string" "Human-readable address of the destination" Nothing),
                  ("vehicleVariant", ParameterProperty "string" "Preferred vehicle variant (e.g., AUTO_RICKSHAW, CAB, BIKE, SUV, SEDAN)" Nothing)
                ],
            paramRequired = ["originLat", "originLon", "destLat", "destLon"]
          }
    }

bookRideTool :: ToolDefinition
bookRideTool =
  ToolDefinition
    { toolName = "bookRide",
      toolDescription = "Book a ride using a selected quote/estimate. This confirms the booking and assigns a driver.",
      toolParameters =
        ToolParameters
          { paramType = "object",
            paramProperties =
              Map.fromList
                [ ("estimateId", ParameterProperty "string" "The ID of the selected estimate/quote to book" Nothing),
                  ("paymentMethod", ParameterProperty "string" "Payment method (CASH, ONLINE, UPI, etc.)" Nothing),
                  ("customerExtraFee", ParameterProperty "number" "Optional tip amount to add to the fare" Nothing)
                ],
            paramRequired = ["estimateId"]
          }
    }

getRideStatusTool :: ToolDefinition
getRideStatusTool =
  ToolDefinition
    { toolName = "getRideStatus",
      toolDescription = "Get the current status of an active ride or booking. Returns driver details, vehicle info, ETA, and current ride state.",
      toolParameters =
        ToolParameters
          { paramType = "object",
            paramProperties =
              Map.fromList
                [ ("bookingId", ParameterProperty "string" "The ID of the booking/ride to check status for" Nothing)
                ],
            paramRequired = ["bookingId"]
          }
    }

cancelRideTool :: ToolDefinition
cancelRideTool =
  ToolDefinition
    { toolName = "cancelRide",
      toolDescription = "Cancel an existing booking or active ride. May incur cancellation charges depending on timing.",
      toolParameters =
        ToolParameters
          { paramType = "object",
            paramProperties =
              Map.fromList
                [ ("bookingId", ParameterProperty "string" "The ID of the booking/ride to cancel" Nothing),
                  ("reasonCode", ParameterProperty "string" "Reason for cancellation (e.g., DRIVER_NOT_MOVING, CHANGE_OF_PLANS, etc.)" Nothing),
                  ("additionalInfo", ParameterProperty "string" "Additional details about cancellation reason" Nothing)
                ],
            paramRequired = ["bookingId"]
          }
    }

addTipTool :: ToolDefinition
addTipTool =
  ToolDefinition
    { toolName = "addTip",
      toolDescription = "Add a tip to an active ride or booking to incentivize driver acceptance or as appreciation.",
      toolParameters =
        ToolParameters
          { paramType = "object",
            paramProperties =
              Map.fromList
                [ ("bookingId", ParameterProperty "string" "The ID of the booking/ride to add tip to" Nothing),
                  ("tipAmount", ParameterProperty "number" "Tip amount in local currency" Nothing),
                  ("currency", ParameterProperty "string" "Currency code (e.g., INR, USD)" Nothing)
                ],
            paramRequired = ["bookingId", "tipAmount"]
          }
    }
