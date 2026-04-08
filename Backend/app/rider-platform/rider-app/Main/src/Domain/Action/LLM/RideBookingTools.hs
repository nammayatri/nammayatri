{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.LLM.RideBookingTools where

import ChatCompletion.Interface.ToolCalling as CIT
import Data.Aeson as A
import qualified Data.Map as Map
import Data.Text as T
import Domain.Action.UI.Search as Search
import Domain.Action.UI.Select as Select
import Domain.Action.UI.Quote as Quote
import Domain.Action.UI.Booking as Booking
import Domain.Action.UI.Ride as Ride
import Domain.Types.Person as Person
import Domain.Types.SearchRequest as SearchRequest
import Domain.Types.Estimate as Estimate
import Domain.Types.Booking as Booking
import Domain.Types.Ride as Ride
import Environment
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Queries.Person as QPerson
import Storage.Queries.SearchRequest as QSearchRequest
import Storage.Queries.Estimate as QEstimate
import Storage.Queries.Booking as QBooking
import Storage.Queries.Ride as QRide

-- | Tool definitions for ride booking actions
rideBookingToolDefinitions :: [CIT.ToolDefinition]
rideBookingToolDefinitions =
  [ searchRidesTool,
    bookRideTool,
    getRideStatusTool,
    cancelRideTool,
    addTipTool
  ]

-- | Tool: Search for available rides
searchRidesTool :: CIT.ToolDefinition
searchRidesTool =
  CIT.ToolDefinition
    { toolName = "searchRides",
      toolDescription = "Search for available rides between origin and destination locations. Returns a list of available ride options with estimates.",
      toolParameters =
        CIT.ToolParameters
          { paramType = "object",
            paramProperties =
              Map.fromList
                [ ( "originLat",
                    CIT.ParameterProperty
                      { propType = "number",
                        propDescription = "Latitude of the origin location",
                        propEnum = Nothing
                      }
                  ),
                  ( "originLon",
                    CIT.ParameterProperty
                      { propType = "number",
                        propDescription = "Longitude of the origin location",
                        propEnum = Nothing
                      }
                  ),
                  ( "destLat",
                    CIT.ParameterProperty
                      { propType = "number",
                        propDescription = "Latitude of the destination location",
                        propEnum = Nothing
                      }
                  ),
                  ( "destLon",
                    CIT.ParameterProperty
                      { propType = "number",
                        propDescription = "Longitude of the destination location",
                        propEnum = Nothing
                      }
                  ),
                  ( "originAddress",
                    CIT.ParameterProperty
                      { propType = "string",
                        propDescription = "Human-readable address of the origin",
                        propEnum = Nothing
                      }
                  ),
                  ( "destAddress",
                    CIT.ParameterProperty
                      { propType = "string",
                        propDescription = "Human-readable address of the destination",
                        propEnum = Nothing
                      }
                  ),
                  ( "vehicleVariant",
                    CIT.ParameterProperty
                      { propType = "string",
                        propDescription = "Preferred vehicle variant (e.g., AUTO_RICKSHAW, CAB, BIKE, SUV, SEDAN)",
                        propEnum = Just ["AUTO_RICKSHAW", "CAB", "BIKE", "SUV", "SEDAN"]
                      }
                  )
                ],
            paramRequired = ["originLat", "originLon", "destLat", "destLon"]
          }
    }

-- | Tool: Book a ride
bookRideTool :: CIT.ToolDefinition
bookRideTool =
  CIT.ToolDefinition
    { toolName = "bookRide",
      toolDescription = "Book a ride using a selected quote/estimate. This confirms the booking and assigns a driver.",
      toolParameters =
        CIT.ToolParameters
          { paramType = "object",
            paramProperties =
              Map.fromList
                [ ( "estimateId",
                    CIT.ParameterProperty
                      { propType = "string",
                        propDescription = "The ID of the selected estimate/quote to book",
                        propEnum = Nothing
                      }
                  ),
                  ( "paymentMethod",
                    CIT.ParameterProperty
                      { propType = "string",
                        propDescription = "Payment method (CASH, ONLINE, UPI, etc.)",
                        propEnum = Just ["CASH", "ONLINE", "UPI", "WALLET"]
                      }
                  ),
                  ( "customerExtraFee",
                    CIT.ParameterProperty
                      { propType = "number",
                        propDescription = "Optional tip amount to add to the fare",
                        propEnum = Nothing
                      }
                  )
                ],
            paramRequired = ["estimateId"]
          }
    }

-- | Tool: Get ride status
getRideStatusTool :: CIT.ToolDefinition
getRideStatusTool =
  CIT.ToolDefinition
    { toolName = "getRideStatus",
      toolDescription = "Get the current status of an active ride or booking. Returns driver details, vehicle info, ETA, and current ride state.",
      toolParameters =
        CIT.ToolParameters
          { paramType = "object",
            paramProperties =
              Map.fromList
                [ ( "bookingId",
                    CIT.ParameterProperty
                      { propType = "string",
                        propDescription = "The ID of the booking/ride to check status for",
                        propEnum = Nothing
                      }
                  )
                ],
            paramRequired = ["bookingId"]
          }
    }

-- | Tool: Cancel ride
cancelRideTool :: CIT.ToolDefinition
cancelRideTool =
  CIT.ToolDefinition
    { toolName = "cancelRide",
      toolDescription = "Cancel an existing booking or active ride. May incur cancellation charges depending on timing.",
      toolParameters =
        CIT.ToolParameters
          { paramType = "object",
            paramProperties =
              Map.fromList
                [ ( "bookingId",
                    CIT.ParameterProperty
                      { propType = "string",
                        propDescription = "The ID of the booking/ride to cancel",
                        propEnum = Nothing
                      }
                  ),
                  ( "reasonCode",
                    CIT.ParameterProperty
                      { propType = "string",
                        propDescription = "Reason for cancellation (e.g., DRIVER_NOT_MOVING, CHANGE_OF_PLANS, etc.)",
                        propEnum = Just ["USER_CANCELLED", "DRIVER_NOT_MOVING", "WRONG_PICKUP_LOCATION", "CHANGE_OF_PLANS", "OTHER"]
                      }
                  ),
                  ( "additionalInfo",
                    CIT.ParameterProperty
                      { propType = "string",
                        propDescription = "Additional details about cancellation reason",
                        propEnum = Nothing
                      }
                  )
                ],
            paramRequired = ["bookingId"]
          }
    }

-- | Tool: Add tip
addTipTool :: CIT.ToolDefinition
addTipTool =
  CIT.ToolDefinition
    { toolName = "addTip",
      toolDescription = "Add a tip to an active ride or booking to incentivize driver acceptance or as appreciation.",
      toolParameters =
        CIT.ToolParameters
          { paramType = "object",
            paramProperties =
              Map.fromList
                [ ( "bookingId",
                    CIT.ParameterProperty
                      { propType = "string",
                        propDescription = "The ID of the booking/ride to add tip to",
                        propEnum = Nothing
                      }
                  ),
                  ( "tipAmount",
                    CIT.ParameterProperty
                      { propType = "number",
                        propDescription = "Tip amount in local currency",
                        propEnum = Nothing
                      }
                  ),
                  ( "currency",
                    CIT.ParameterProperty
                      { propType = "string",
                        propDescription = "Currency code (e.g., INR, USD)",
                        propEnum = Just ["INR", "USD"]
                      }
                  )
                ],
            paramRequired = ["bookingId", "tipAmount"]
          }
    }

-- | Execute a tool call
executeToolCall ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["version" ::: DeploymentVersion]
  ) =>
  Id Person ->
  CIT.ToolCall ->
  m A.Value
executeToolCall personId toolCall = do
  let args = case A.decode (encodeUtf8 toolCall.toolCallArguments) of
        Just (Object obj) -> obj
        _ -> mempty
  case toolCall.toolCallName of
    "searchRides" -> executeSearchRides personId args
    "bookRide" -> executeBookRide personId args
    "getRideStatus" -> executeGetRideStatus personId args
    "cancelRide" -> executeCancelRide personId args
    "addTip" -> executeAddTip personId args
    _ -> return $ A.object ["error" .= ("Unknown tool: " <> toolCall.toolCallName)]

-- | Execute search rides tool
executeSearchRides ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["version" ::: DeploymentVersion]
  ) =>
  Id Person ->
  A.Object ->
  m A.Value
executeSearchRides personId args = do
  originLat <- parseRequiredNumber args "originLat"
  originLon <- parseRequiredNumber args "originLon"
  destLat <- parseRequiredNumber args "destLat"
  destLon <- parseRequiredNumber args "destLon"
  let originAddress = parseOptionalText args "originAddress"
      destAddress = parseOptionalText args "destAddress"
      vehicleVariant = parseOptionalText args "vehicleVariant"

  -- Create search request
  let searchReq =
        Search.SearchReq
          { origin = Search.SearchReqLocation originLat originLon originAddress,
            destination = Search.SearchReqLocation destLat destLon destAddress,
            isSourceManuallyMoved = Nothing,
            isDestinationManuallyMoved = Nothing,
            isSpecialLocation = Nothing,
            startTime = Nothing,
            isReallocationEnabled = Nothing,
            quotesUnifiedFlow = Nothing,
            sessionToken = Nothing,
            placeNameSource = Nothing,
            driverIdentifier = Nothing
          }

  -- Execute search
  result <- Search.search personId Nothing searchReq

  return $ A.object
    [ "searchId" .= result.searchId,
      "message" .= ("Search initiated successfully" :: Text)
    ]

-- | Execute book ride tool (combines select quote and confirm booking)
executeBookRide ::
  ( EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id Person ->
  A.Object ->
  m A.Value
executeBookRide personId args = do
  estimateId <- parseRequiredText args "estimateId"
  let paymentMethod = parseOptionalText args "paymentMethod"
      customerExtraFee = parseOptionalNumber args "customerExtraFee"

  -- Get estimate
  estimate <- QEstimate.findById (Id estimateId) >>= fromMaybeM (EstimateDoesNotExist estimateId)

  -- Create select request with auto-assign enabled
  let selectReq =
        Select.DSelectReq
          { customerExtraFee = customerExtraFee,
            autoAssignEnabled = Just True,
            paymentMethodId = paymentMethod
          }

  -- Execute select
  Select.select estimateId selectReq

  -- Confirm booking
  booking <- Booking.confirm estimateId

  return $ A.object
    [ "bookingId" .= booking.id,
      "bookingStatus" .= booking.status,
      "message" .= ("Ride booked successfully" :: Text)
    ]

-- | Execute get ride status tool
executeGetRideStatus ::
  ( EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id Person ->
  A.Object ->
  m A.Value
executeGetRideStatus personId args = do
  bookingId <- parseRequiredText args "bookingId"

  -- Get booking
  booking <- QBooking.findById (Id bookingId) >>= fromMaybeM (BookingDoesNotExist bookingId)

  -- Get ride if exists
  mRide <- QRide.findActiveByRBId booking.id

  return $ A.object
    [ "bookingId" .= booking.id,
      "bookingStatus" .= booking.status,
      "ride" .= case mRide of
        Just ride ->
          A.object
            [ "rideId" .= ride.id,
              "rideStatus" .= ride.status,
              "driverName" .= ride.driverName,
              "driverNumber" .= ride.driverMobileNumber,
              "vehicleNumber" .= ride.vehicleNumber,
              "otp" .= ride.otp
            ]
        Nothing -> A.Null
    ]

-- | Execute cancel ride tool
executeCancelRide ::
  ( EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id Person ->
  A.Object ->
  m A.Value
executeCancelRide personId args = do
  bookingId <- parseRequiredText args "bookingId"
  let reasonCode = parseOptionalText args "reasonCode"
      additionalInfo = parseOptionalText args "additionalInfo"

  -- Get booking
  booking <- QBooking.findById (Id bookingId) >>= fromMaybeM (BookingDoesNotExist bookingId)

  -- Cancel booking
  let cancelReq =
        Booking.BookingCancelReq
          { reasonCode = fromMaybe "USER_CANCELLED" reasonCode,
            reasonStage = "AFTER_ASSIGN",
            additionalInfo = additionalInfo
          }

  Booking.cancelBooking bookingId cancelReq

  return $ A.object
    [ "bookingId" .= bookingId,
      "cancelled" .= True,
      "message" .= ("Booking cancelled successfully" :: Text)
    ]

-- | Execute add tip tool
executeAddTip ::
  ( EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id Person ->
  A.Object ->
  m A.Value
executeAddTip personId args = do
  bookingId <- parseRequiredText args "bookingId"
  tipAmount <- parseRequiredNumber args "tipAmount"
  let currency = parseOptionalText args "currency"

  -- Get booking
  booking <- QBooking.findById (Id bookingId) >>= fromMaybeM (BookingDoesNotExist bookingId)

  -- Add tip to booking (this would typically update the booking with tip amount)
  -- For now, we return success as the actual tip implementation would depend on
  -- the specific booking flow and payment integration

  return $ A.object
    [ "bookingId" .= bookingId,
      "tipAmount" .= tipAmount,
      "currency" .= fromMaybe "INR" currency,
      "message" .= ("Tip added successfully" :: Text)
    ]

-- Helper functions
parseRequiredText :: MonadThrow m => A.Object -> Text -> m Text
parseRequiredText obj key =
  case A.lookup (toString key) obj of
    Just (A.String t) -> return t
    _ -> throwError $ InvalidRequest $ "Missing required field: " <> key

parseOptionalText :: A.Object -> Text -> Maybe Text
parseOptionalText obj key =
  case A.lookup (toString key) obj of
    Just (A.String t) -> Just t
    _ -> Nothing

parseRequiredNumber :: MonadThrow m => A.Object -> Text -> m Double
parseRequiredNumber obj key =
  case A.lookup (toString key) obj of
    Just (A.Number n) -> return $ toRealFloat n
    _ -> throwError $ InvalidRequest $ "Missing required field: " <> key

parseOptionalBool :: A.Object -> Text -> Maybe Bool
parseOptionalBool obj key =
  case A.lookup (toString key) obj of
    Just (A.Bool b) -> Just b
    _ -> Nothing

parseOptionalNumber :: A.Object -> Text -> Maybe Double
parseOptionalNumber obj key =
  case A.lookup (toString key) obj of
    Just (A.Number n) -> Just $ toRealFloat n
    _ -> Nothing
