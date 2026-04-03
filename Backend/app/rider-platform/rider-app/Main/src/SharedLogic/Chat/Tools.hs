{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Chat.Tools
  ( -- * Tool Types
    Tool (..),
    ToolParameter (..),
    ToolParameterType (..),
    ToolResult (..),
    ToolError (..),

    -- * Tool Definitions
    allTools,
    getToolByName,

    -- * Tool Execution
    executeTool,
    ExecuteToolContext (..),

    -- * Individual Tool Handlers
    executeSearch,
    executeSelect,
    executeConfirm,
    executeCancel,
    executeGetRideStatus,
    executeGetProfile,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Types as AT
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.Confirm as DConfirm
import qualified Domain.Action.UI.Profile as DProfile
import qualified Domain.Action.UI.Ride as DRide
import qualified Domain.Action.UI.Search as DSearch
import qualified Domain.Action.UI.Select as DSelect
import qualified SharedLogic.Search as SSearch
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.LocationAddress as DLA
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSearchRequest
import Environment (Flow)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Servant (NoContent (..))

-- | Type of tool parameter
data ToolParameterType
  = StringType
  | NumberType
  | IntegerType
  | BooleanType
  | ArrayType ToolParameterType
  | ObjectType
  deriving (Show, Eq, Generic)

instance ToJSON ToolParameterType where
  toJSON StringType = A.String "string"
  toJSON NumberType = A.String "number"
  toJSON IntegerType = A.String "integer"
  toJSON BooleanType = A.String "boolean"
  toJSON (ArrayType inner) = A.object ["type" A..= ("array" :: Text), "items" A..= inner]
  toJSON ObjectType = A.String "object"

-- | Definition of a tool parameter
data ToolParameter = ToolParameter
  { name :: !Text,
    description :: !Text,
    paramType :: !ToolParameterType,
    required :: !Bool,
    defaultValue :: !(Maybe A.Value)
  }
  deriving (Show, Eq, Generic)

instance ToJSON ToolParameter where
  toJSON ToolParameter {..} =
    A.object
      [ "name" A..= name,
        "description" A..= description,
        "type" A..= paramType,
        "required" A..= required
      ]
        <> maybe mempty ("default" A..=) defaultValue

-- | Tool definition for LLM function calling
data Tool = Tool
  { toolName :: !Text,
    toolDescription :: !Text,
    parameters :: ![ToolParameter]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Tool where
  toJSON Tool {..} =
    A.object
      [ "name" A..= toolName,
        "description" A..= toolDescription,
        "parameters" A..= parameters
      ]

-- | Result of tool execution
data ToolResult
  = ToolSuccess A.Value
  | ToolError ToolError
  deriving (Show, Eq, Generic)

instance ToJSON ToolResult where
  toJSON (ToolSuccess value) =
    A.object
      [ "status" A..= ("success" :: Text),
        "data" A..= value
      ]
  toJSON (ToolError err) =
    A.object
      [ "status" A..= ("error" :: Text),
        "error" A..= err
      ]

-- | Tool execution error
data ToolError
  = ToolNotFound Text
  | MissingRequiredParameter Text
  | InvalidParameterType Text Text
  | ToolExecutionError Text
  | SearchNotFound Text
  | EstimateNotFound Text
  | BookingNotFound Text
  | RideNotFound Text
  | PersonNotFound Text
  | InvalidRequestError Text
  deriving (Show, Eq, Generic)

instance ToJSON ToolError where
  toJSON (ToolNotFound name) = A.object ["code" A..= ("TOOL_NOT_FOUND" :: Text), "message" A..= ("Tool not found: " <> name)]
  toJSON (MissingRequiredParameter name) = A.object ["code" A..= ("MISSING_REQUIRED_PARAMETER" :: Text), "message" A..= ("Missing required parameter: " <> name)]
  toJSON (InvalidParameterType name expected) = A.object ["code" A..= ("INVALID_PARAMETER_TYPE" :: Text), "message" A..= ("Invalid type for parameter " <> name <> ", expected: " <> expected)]
  toJSON (ToolExecutionError msg) = A.object ["code" A..= ("EXECUTION_ERROR" :: Text), "message" A..= msg]
  toJSON (SearchNotFound id_) = A.object ["code" A..= ("SEARCH_NOT_FOUND" :: Text), "message" A..= ("Search request not found: " <> id_)]
  toJSON (EstimateNotFound id_) = A.object ["code" A..= ("ESTIMATE_NOT_FOUND" :: Text), "message" A..= ("Estimate not found: " <> id_)]
  toJSON (BookingNotFound id_) = A.object ["code" A..= ("BOOKING_NOT_FOUND" :: Text), "message" A..= ("Booking not found: " <> id_)]
  toJSON (RideNotFound id_) = A.object ["code" A..= ("RIDE_NOT_FOUND" :: Text), "message" A..= ("Ride not found: " <> id_)]
  toJSON (PersonNotFound id_) = A.object ["code" A..= ("PERSON_NOT_FOUND" :: Text), "message" A..= ("Person not found: " <> id_)]
  toJSON (InvalidRequestError msg) = A.object ["code" A..= ("INVALID_REQUEST" :: Text), "message" A..= msg]

-- | Context required for tool execution
data ExecuteToolContext = ExecuteToolContext
  { personId :: Id DPerson.Person,
    merchantId :: Id DMerchant.Merchant,
    mbBundleVersion :: Maybe Version,
    mbClientVersion :: Maybe Version,
    mbClientConfigVersion :: Maybe Version,
    mbRnVersion :: Maybe Text,
    mbClientId :: Maybe Text,
    mbDevice :: Maybe Text
  }
  deriving (Show, Generic)

-- | All available tools for LLM function calling
allTools :: [Tool]
allTools =
  [ searchTool,
    selectTool,
    confirmTool,
    cancelTool,
    getRideStatusTool,
    getProfileTool
  ]

-- | Get a tool by name
getToolByName :: Text -> Maybe Tool
getToolByName name = find ((== name) . toolName) allTools

-- ============================================================================
-- Tool Definitions
-- ============================================================================

searchTool :: Tool
searchTool =
  Tool
    { toolName = "search",
      toolDescription = "Search for available rides between origin and destination. Returns a search request ID that can be used to get quotes.",
      parameters =
        [ ToolParameter
            { name = "originLat",
              description = "Latitude of the pickup location",
              paramType = NumberType,
              required = True,
              defaultValue = Nothing
            },
          ToolParameter
            { name = "originLon",
              description = "Longitude of the pickup location",
              paramType = NumberType,
              required = True,
              defaultValue = Nothing
            },
          ToolParameter
            { name = "originAddress",
              description = "Address of the pickup location",
              paramType = StringType,
              required = True,
              defaultValue = Nothing
            },
          ToolParameter
            { name = "destinationLat",
              description = "Latitude of the drop location",
              paramType = NumberType,
              required = True,
              defaultValue = Nothing
            },
          ToolParameter
            { name = "destinationLon",
              description = "Longitude of the drop location",
              paramType = NumberType,
              required = True,
              defaultValue = Nothing
            },
          ToolParameter
            { name = "destinationAddress",
              description = "Address of the drop location",
              paramType = StringType,
              required = True,
              defaultValue = Nothing
            },
          ToolParameter
            { name = "rideType",
              description = "Type of ride: OneWay, Rental, InterCity, Ambulance, Delivery",
              paramType = StringType,
              required = False,
              defaultValue = Just (A.String "OneWay")
            }
        ]
    }

selectTool :: Tool
selectTool =
  Tool
    { toolName = "select",
      toolDescription = "Select an estimate to get quotes from drivers. Call this after search to proceed with booking.",
      parameters =
        [ ToolParameter
            { name = "estimateId",
              description = "ID of the estimate to select (from search results)",
              paramType = StringType,
              required = True,
              defaultValue = Nothing
            },
          ToolParameter
            { name = "autoAssignEnabled",
              description = "Whether to auto-assign the first available driver",
              paramType = BooleanType,
              required = False,
              defaultValue = Just (A.Bool True)
            }
        ]
    }

confirmTool :: Tool
confirmTool =
  Tool
    { toolName = "confirm",
      toolDescription = "Confirm a booking to finalize the ride. Call this after selecting an estimate.",
      parameters =
        [ ToolParameter
            { name = "estimateId",
              description = "ID of the estimate to confirm",
              paramType = StringType,
              required = True,
              defaultValue = Nothing
            }
        ]
    }

cancelTool :: Tool
cancelTool =
  Tool
    { toolName = "cancel",
      toolDescription = "Cancel an existing booking or search request.",
      parameters =
        [ ToolParameter
            { name = "bookingId",
              description = "ID of the booking to cancel",
              paramType = StringType,
              required = False,
              defaultValue = Nothing
            },
          ToolParameter
            { name = "searchRequestId",
              description = "ID of the search request to cancel (if no booking yet)",
              paramType = StringType,
              required = False,
              defaultValue = Nothing
            },
          ToolParameter
            { name = "reasonCode",
              description = "Reason for cancellation",
              paramType = StringType,
              required = False,
              defaultValue = Just (A.String "User requested")
            }
        ]
    }

getRideStatusTool :: Tool
getRideStatusTool =
  Tool
    { toolName = "getRideStatus",
      toolDescription = "Get the current status of a ride by ride ID or booking ID.",
      parameters =
        [ ToolParameter
            { name = "rideId",
              description = "ID of the ride (optional if bookingId provided)",
              paramType = StringType,
              required = False,
              defaultValue = Nothing
            },
          ToolParameter
            { name = "bookingId",
              description = "ID of the booking (optional if rideId provided)",
              paramType = StringType,
              required = False,
              defaultValue = Nothing
            }
        ]
    }

getProfileTool :: Tool
getProfileTool =
  Tool
    { toolName = "getProfile",
      toolDescription = "Get the user's profile information including name, phone, email, and preferences.",
      parameters = []
    }

-- ============================================================================
-- Tool Execution
-- ============================================================================

-- | Execute a tool with the given name and parameters
executeTool :: ExecuteToolContext -> Text -> A.Object -> Flow ToolResult
executeTool ctx toolName params =
  case toolName of
    "search" -> executeSearch ctx params
    "select" -> executeSelect ctx params
    "confirm" -> executeConfirm ctx params
    "cancel" -> executeCancel ctx params
    "getRideStatus" -> executeGetRideStatus ctx params
    "getProfile" -> executeGetProfile ctx params
    _ -> pure $ ToolError $ ToolNotFound toolName

-- | Helper to get a required parameter
getRequiredParam :: A.Object -> Text -> (A.Value -> Maybe a) -> Text -> Either ToolError a
getRequiredParam params key parser expectedType =
  case AKM.lookup (A.fromText key) params of
    Nothing -> Left $ MissingRequiredParameter key
    Just value -> case parser value of
      Nothing -> Left $ InvalidParameterType key expectedType
      Just val -> Right val

-- | Helper to get an optional parameter
getOptionalParam :: A.Object -> Text -> (A.Value -> Maybe a) -> Maybe a -> Maybe a
getOptionalParam params key parser defaultVal =
  case AKM.lookup (A.fromText key) params of
    Nothing -> defaultVal
    Just value -> parser value <|> defaultVal

-- | Parse text from JSON value
parseText :: A.Value -> Maybe Text
parseText (A.String t) = Just t
parseText _ = Nothing

-- | Parse double from JSON value
parseDouble :: A.Value -> Maybe Double
parseDouble (A.Number n) = Just $ realToFrac n
parseDouble _ = Nothing

-- | Parse bool from JSON value
parseBool :: A.Value -> Maybe Bool
parseBool (A.Bool b) = Just b
parseBool _ = Nothing

-- ============================================================================
-- Individual Tool Handlers
-- ============================================================================

-- | Execute search tool
executeSearch :: ExecuteToolContext -> A.Object -> Flow ToolResult
executeSearch ctx params = do
  -- Parse required parameters
  originLat <- case getRequiredParam params "originLat" parseDouble "number" of
    Left err -> return $ ToolError err
    Right val -> pure val
  originLon <- case getRequiredParam params "originLon" parseDouble "number" of
    Left err -> return $ ToolError err
    Right val -> pure val
  originAddress <- case getRequiredParam params "originAddress" parseText "string" of
    Left err -> return $ ToolError err
    Right val -> pure val
  destLat <- case getRequiredParam params "destinationLat" parseDouble "number" of
    Left err -> return $ ToolError err
    Right val -> pure val
  destLon <- case getRequiredParam params "destinationLon" parseDouble "number" of
    Left err -> return $ ToolError err
    Right val -> pure val
  destAddress <- case getRequiredParam params "destinationAddress" parseText "string" of
    Left err -> return $ ToolError err
    Right val -> pure val

  let rideType = fromMaybe "OneWay" $ getOptionalParam params "rideType" parseText Nothing

  -- Build search request based on ride type
  let originLoc =
        SSearch.SearchReqLocation
          { gps = LatLong {lat = originLat, lon = originLon},
            address = DLA.LocationAddress {area = Just originAddress, areaCode = Nothing, building = Nothing, city = Nothing, country = Nothing, door = Nothing, state = Nothing, street = Nothing, ward = Nothing, placeId = Nothing, instructions = Nothing, title = Nothing, extras = Nothing}
          }
  let destLoc =
        SSearch.SearchReqLocation
          { gps = LatLong {lat = destLat, lon = destLon},
            address = DLA.LocationAddress {area = Just destAddress, areaCode = Nothing, building = Nothing, city = Nothing, country = Nothing, door = Nothing, state = Nothing, street = Nothing, ward = Nothing, placeId = Nothing, instructions = Nothing, title = Nothing, extras = Nothing}
          }

  let searchReq =
        DSearch.OneWaySearch
          DSearch.OneWaySearchReq
            { origin = originLoc,
              destination = Just destLoc,
              stops = Nothing,
              startTime = Nothing,
              isSourceManuallyMoved = Nothing,
              isDestinationManuallyMoved = Nothing,
              isSpecialLocation = Nothing,
              driverIdentifier = Nothing,
              quotesUnifiedFlow = Just True,
              placeNameSource = Nothing,
              isReallocationEnabled = Nothing,
              fareParametersInRateCard = Nothing,
              sessionToken = Nothing,
              isMeterRideSearch = Nothing,
              recentLocationId = Nothing,
              platformType = Nothing,
              isReserveRide = Nothing,
              subscriptionId = Nothing,
              verifyBeforeCancellingOldBooking = Nothing,
              numberOfLuggages = Nothing,
              doMultimodalSearch = Nothing
            }

  -- Call domain action
  result <-
    try @_ @SomeException $
      DSearch.search
        ctx.personId
        searchReq
        ctx.mbBundleVersion
        ctx.mbClientVersion
        ctx.mbClientConfigVersion
        ctx.mbRnVersion
        (Id <$> ctx.mbClientId)
        ctx.mbDevice
        False
        False
        Nothing

  case result of
    Left ex -> pure $ ToolError $ ToolExecutionError $ T.pack $ show ex
    Right searchRes ->
      pure $ ToolSuccess $ A.object ["searchId" A..= searchRes.searchRequest.id, "searchExpiry" A..= searchRes.searchRequestExpiry]

-- | Execute select tool
executeSelect :: ExecuteToolContext -> A.Object -> Flow ToolResult
executeSelect ctx params = do
  estimateIdText <- case getRequiredParam params "estimateId" parseText "string" of
    Left err -> return $ ToolError err
    Right val -> pure val

  let estimateId = Id @DEstimate.Estimate estimateIdText
  let autoAssign = fromMaybe True $ getOptionalParam params "autoAssignEnabled" parseBool Nothing

  let selectReq =
        DSelect.DSelectReq
          { customerExtraFee = Nothing,
            autoAssignEnabled = Just autoAssign,
            paymentMethodId = Nothing,
            otherSelectedEstimates = Nothing,
            otherSelectedEstimatesData = Nothing,
            deliveryDetails = Nothing
          }

  -- Call domain action
  result <-
    try @_ @SomeException $
      DSelect.select ctx.personId estimateId selectReq

  case result of
    Left ex -> pure $ ToolError $ ToolExecutionError $ T.pack $ show ex
    Right selectRes ->
      pure $ ToolSuccess $ A.object ["selectTtl" A..= selectRes.selectTtl]

-- | Execute confirm tool
executeConfirm :: ExecuteToolContext -> A.Object -> Flow ToolResult
executeConfirm ctx params = do
  estimateIdText <- case getRequiredParam params "estimateId" parseText "string" of
    Left err -> return $ ToolError err
    Right val -> pure val

  let estimateId = Id @DEstimate.Estimate estimateIdText

  -- Call domain action
  result <-
    try @_ @SomeException $
      DConfirm.confirm ctx.personId estimateId Nothing Nothing

  case result of
    Left ex -> pure $ ToolError $ ToolExecutionError $ T.pack $ show ex
    Right confirmRes ->
      pure $
        ToolSuccess $
          A.object
            [ "bookingId" A..= confirmRes.bookingId,
              "rideId" A..= confirmRes.rideId,
              "bookingStatus" A..= confirmRes.bookingStatus,
              "rideStatus" A..= confirmRes.rideStatus
            ]

-- | Execute cancel tool
executeCancel :: ExecuteToolContext -> A.Object -> Flow ToolResult
executeCancel ctx params = do
  let mbBookingId = Id @DBooking.Booking <$> getOptionalParam params "bookingId" parseText Nothing
  let mbSearchReqId = Id @DSearchRequest.SearchRequest <$> getOptionalParam params "searchRequestId" parseText Nothing
  let reasonCode = fromMaybe "User requested" $ getOptionalParam params "reasonCode" parseText Nothing

  case (mbBookingId, mbSearchReqId) of
    (Nothing, Nothing) -> pure $ ToolError $ InvalidRequestError "Either bookingId or searchRequestId must be provided"
    (Just bookingId, _) -> do
      let cancelReq =
            DCancel.CancelReq
              { reasonCode = Nothing,
                additionalInfo = Just reasonCode,
                reallocate = Nothing,
                blockOnCancellationRate = Nothing
              }
      result <-
        try @_ @SomeException $
          DCancel.cancelBooking ctx.personId bookingId cancelReq
      case result of
        Left ex -> pure $ ToolError $ ToolExecutionError $ T.pack $ show ex
        Right _ -> pure $ ToolSuccess $ A.object ["cancelled" A..= True, "bookingId" A..= bookingId.getId]
    (_, Just searchReqId) -> do
      result <-
        try @_ @SomeException $
          DCancel.cancelSearch ctx.personId searchReqId
      case result of
        Left ex -> pure $ ToolError $ ToolExecutionError $ T.pack $ show ex
        Right _ -> pure $ ToolSuccess $ A.object ["cancelled" A..= True, "searchRequestId" A..= searchReqId.getId]

-- | Execute get ride status tool
executeGetRideStatus :: ExecuteToolContext -> A.Object -> Flow ToolResult
executeGetRideStatus ctx params = do
  let mbRideId = Id @DRide.Ride <$> getOptionalParam params "rideId" parseText Nothing
  let mbBookingId = Id @DBooking.Booking <$> getOptionalParam params "bookingId" parseText Nothing

  case (mbRideId, mbBookingId) of
    (Nothing, Nothing) -> pure $ ToolError $ InvalidRequestError "Either rideId or bookingId must be provided"
    (Just rideId, _) -> do
      result <-
        try @_ @SomeException $
          DRide.getRideStatus rideId ctx.personId
      case result of
        Left ex -> pure $ ToolError $ ToolExecutionError $ T.pack $ show ex
        Right rideStatus ->
          pure $ ToolSuccess $ A.toJSON rideStatus
    (_, Just bookingId) -> do
      -- For booking status, we need to find the ride first
      mbRide <- QRide.findActiveByRBId bookingId
      case mbRide of
        Just ride -> do
          result <-
            try @_ @SomeException $
              DRide.getRideStatus ride.id ctx.personId
          case result of
            Left ex -> pure $ ToolError $ ToolExecutionError $ T.pack $ show ex
            Right rideStatus ->
              pure $ ToolSuccess $ A.toJSON rideStatus
        Nothing -> do
          -- No active ride, return booking info
          booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
          pure $ ToolSuccess $ A.object
            [ "bookingId" A..= booking.id,
              "status" A..= booking.status,
              "fromLocation" A..= booking.fromLocation,
              "toLocation" A..= (case booking.bookingDetails of
                DB.OneWayDetails details -> Just details.toLocation
                DB.RentalDetails _ -> Nothing
                DB.OneWaySpecialZoneDetails details -> Just details.toLocation
                DB.InterCityDetails details -> Just details.toLocation
                DB.DriverOfferDetails details -> Just details.toLocation
                DB.AmbulanceDetails details -> Just details.toLocation
                DB.DeliveryDetails details -> Just details.toLocation
                DB.MeterRideDetails details -> details.toLocation)
            ]

-- | Execute get profile tool
executeGetProfile :: ExecuteToolContext -> A.Object -> Flow ToolResult
executeGetProfile ctx _params = do
  result <-
    try @_ @SomeException $
      DProfile.getPersonDetails (ctx.personId, ctx.merchantId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

  case result of
    Left ex -> pure $ ToolError $ ToolExecutionError $ T.pack $ show ex
    Right profile ->
      pure $ ToolSuccess $ A.toJSON profile
