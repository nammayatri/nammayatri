{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Error (module Tools.Error, SearchCancelErrors (..)) where

import EulerHS.Prelude
import Kernel.Types.Error as Tools.Error
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse
import Network.HTTP.Types (Status (statusCode))
import Servant.Client (ResponseF (responseStatusCode))

data CustomerError = PersonMobileAlreadyExists Text | DeviceTokenNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''CustomerError

instance IsBaseError CustomerError where
  toMessage (PersonMobileAlreadyExists phoneNo) = Just $ "Mobile number " <> phoneNo <> " already exists with another user."
  toMessage DeviceTokenNotFound = Just "Device Token does not exist."

instance IsHTTPError CustomerError where
  toErrorCode = \case
    PersonMobileAlreadyExists _ -> "PERSON_MOBILE_ALREADY_EXISTS"
    DeviceTokenNotFound -> "DEVICE_TOKEN_NOT_FOUND"
  toHttpCode = \case
    PersonMobileAlreadyExists _ -> E400
    DeviceTokenNotFound -> E400

instance IsAPIError CustomerError

data RatingError
  = InvalidRatingValue
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RatingError

instance IsBaseError RatingError

instance IsHTTPError RatingError where
  toErrorCode InvalidRatingValue = "INVALID_RATING_VALUE"
  toHttpCode InvalidRatingValue = E400

instance IsAPIError RatingError

data EstimateError = EstimateDoesNotExist Text | EstimateStatusDoesNotExist Text | EstimateCancelled Text | EstimateNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''EstimateError

instance IsBaseError EstimateError where
  toMessage (EstimateDoesNotExist estimateId) = Just $ "No estimate matches passed data \"" <> show estimateId <> "\" not exist. "
  toMessage (EstimateStatusDoesNotExist estimateId) = Just $ "Estimate status not found with estimate id : \"" <> show estimateId
  toMessage (EstimateCancelled estimateId) = Just $ "Estimate for the estimate id : \"" <> show estimateId <> "\" has been cancelled. "
  toMessage EstimateNotFound = Just "Estimate not found. "

instance IsHTTPError EstimateError where
  toErrorCode = \case
    EstimateNotFound -> "ESTIMATE_NOT_FOUND"
    EstimateDoesNotExist _ -> "ESTIMATE_DOES_NOT_EXIST"
    EstimateCancelled _ -> "ESTIMATE_CANCELLED"
    EstimateStatusDoesNotExist _ -> "ESTIMATE_STATUS_DOES_NOT_EXIST"
  toHttpCode = \case
    EstimateNotFound -> E400
    EstimateDoesNotExist _ -> E400
    EstimateCancelled _ -> E403
    EstimateStatusDoesNotExist _ -> E400

instance IsAPIError EstimateError

data TrackUrlError
  = InvalidRideRequest
  | TrackingUrlFailed
  | BPPServerUnavailable
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''TrackUrlError

instance FromResponse TrackUrlError where
  fromResponse resp = case statusCode $ responseStatusCode resp of
    400 -> Just InvalidRideRequest
    503 -> Just BPPServerUnavailable
    _ -> Just TrackingUrlFailed

instance IsBaseError TrackUrlError where
  toMessage = \case
    InvalidRideRequest -> Just "Tracking not available for provided ride."
    TrackingUrlFailed -> Just "Can't call tracking url"
    BPPServerUnavailable -> Just "BPP server is not available to fetch the driver location"

instance IsHTTPError TrackUrlError where
  toErrorCode = \case
    InvalidRideRequest -> "INVALID_RIDE_REQUEST"
    TrackingUrlFailed -> "TRACKING_URL_FAILED"
    BPPServerUnavailable -> "BPP_SERVER_UNAVAILABLE"

  toHttpCode = \case
    InvalidRideRequest -> E412
    TrackingUrlFailed -> E500
    BPPServerUnavailable -> E503

instance IsAPIError TrackUrlError

-- TODO move to lib
data MerchantPaymentMethodError
  = MerchantPaymentMethodNotFound Text
  | MerchantPaymentMethodDoesNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''MerchantPaymentMethodError

instance IsBaseError MerchantPaymentMethodError where
  toMessage = \case
    MerchantPaymentMethodNotFound merchantPaymentMethodId -> Just $ "Merchant payment method with id \"" <> show merchantPaymentMethodId <> "\" not found."
    MerchantPaymentMethodDoesNotExist merchantPaymentMethodId -> Just $ "No merchant payment method matches passed data \"<>" <> show merchantPaymentMethodId <> "\"."

instance IsHTTPError MerchantPaymentMethodError where
  toErrorCode = \case
    MerchantPaymentMethodNotFound _ -> "MERCHANT_PAYMENT_METHOD_NOT_FOUND"
    MerchantPaymentMethodDoesNotExist _ -> "MERCHANT_PAYMENT_METHOD_DOES_NOT_EXIST"
  toHttpCode = \case
    MerchantPaymentMethodNotFound _ -> E500
    MerchantPaymentMethodDoesNotExist _ -> E400

instance IsAPIError MerchantPaymentMethodError

newtype PersonStatsError
  = PersonStatsNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''PersonStatsError

instance IsBaseError PersonStatsError where
  toMessage = \case
    PersonStatsNotFound personId -> Just $ "Person stats with personId \"" <> show personId <> "\" not found."

instance IsHTTPError PersonStatsError where
  toErrorCode = \case
    PersonStatsNotFound _ -> "PERSON_STATS_NOT_FOUND"
  toHttpCode = \case
    PersonStatsNotFound _ -> E500

instance IsAPIError PersonStatsError

data MediaFileError
  = FileSizeExceededError Text
  | FileDoNotExist Text
  | FileFormatNotSupported Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''MediaFileError

instance IsHTTPError MediaFileError where
  toErrorCode = \case
    FileSizeExceededError _ -> "FILE_SIZE_EXCEEDED"
    FileDoNotExist _ -> "FILE_DO_NOT_EXIST"
    FileFormatNotSupported _ -> "FILE_FORMAT_NOT_SUPPORTED"
  toHttpCode = \case
    FileSizeExceededError _ -> E413
    FileDoNotExist _ -> E400
    FileFormatNotSupported _ -> E415

instance IsAPIError MediaFileError

instance IsBaseError MediaFileError where
  toMessage = \case
    FileSizeExceededError fileSize -> Just $ "Filesize is " <> fileSize <> " Bytes, which is more than the allowed 10MB limit."
    FileDoNotExist fileId -> Just $ "MediaFile with fileId \"" <> show fileId <> "\" do not exist."
    FileFormatNotSupported fileFormat -> Just $ "MediaFile with fileFormat \"" <> show fileFormat <> "\" not supported."

newtype DisabilityError
  = DisabilityDoesNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DisabilityError

instance IsBaseError DisabilityError where
  toMessage = \case
    DisabilityDoesNotExist personId -> Just $ "Disability with disabilityId \"" <> show personId <> "\"not found. "

instance IsHTTPError DisabilityError where
  toErrorCode = \case
    DisabilityDoesNotExist _ -> "DISABILITY_DOES_NOT_EXIST"
  toHttpCode = \case
    DisabilityDoesNotExist _ -> E400

instance IsAPIError DisabilityError

data AadhaarError
  = AadhaarAlreadyVerified
  | TransactionIdNotFound
  | AadhaarAlreadyLinked
  | AadhaarDataAlreadyPresent
  | GenerateAadhaarOtpExceedLimit Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''AadhaarError

instance IsBaseError AadhaarError where
  toMessage AadhaarAlreadyVerified = Just " aadhaar is already verified."
  toMessage TransactionIdNotFound = Just " transaction id not found for this verification"
  toMessage AadhaarAlreadyLinked = Just "aadhaar number is already linked"
  toMessage AadhaarDataAlreadyPresent = Just "aadhaar data is already present for this person"
  toMessage (GenerateAadhaarOtpExceedLimit id_) = Just $ "Generate Aadhaar otp  try limit exceeded for person \"" <> id_ <> "\"."

instance IsHTTPError AadhaarError where
  toErrorCode = \case
    AadhaarAlreadyVerified -> "AADHAAR_ALREADY_VERIFIED"
    TransactionIdNotFound -> "TRANSACTION_ID_NOT_FOUND"
    AadhaarAlreadyLinked -> "AADHAAR_ALREADY_LINKED"
    AadhaarDataAlreadyPresent -> "AADHAAR_DATA_ALREADY_PRESENT"
    GenerateAadhaarOtpExceedLimit _ -> "GENERATE_AADHAAR_OTP_EXCEED_LIMIT"
  toHttpCode = \case
    AadhaarAlreadyVerified -> E400
    TransactionIdNotFound -> E400
    AadhaarAlreadyLinked -> E400
    AadhaarDataAlreadyPresent -> E400
    GenerateAadhaarOtpExceedLimit _ -> E429

instance IsAPIError AadhaarError

data TicketBookingError
  = TicketServiceNotFound Text
  | TicketBookingNotFound Text
  | TicketBookingServiceNotFound Text
  | TicketPlaceNotFound Text
  | BusinessHourNotFound Text
  | ServiceCategoryNotFound Text
  | TicketSeatManagementNotFound Text Text
  | PeopleCategoryNotFound Text
  | TicketBookingNotConfirmed Text
  | TicketBookingServiceNotConfirmed Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''TicketBookingError

instance IsBaseError TicketBookingError where
  toMessage (TicketServiceNotFound serviceId) = Just $ "Ticker service not found: " <> show serviceId
  toMessage (TicketBookingNotFound bookingId) = Just $ "Ticket booking not found: " <> show bookingId
  toMessage (TicketBookingServiceNotFound bookingServiceId) = Just $ "Ticket booking service not found: " <> show bookingServiceId
  toMessage (TicketBookingNotConfirmed bookingId) = Just $ "Ticket booking not confirmed: " <> show bookingId
  toMessage (TicketBookingServiceNotConfirmed bookingServiceId) = Just $ "Ticket booking service not confirmed: " <> show bookingServiceId
  toMessage (TicketPlaceNotFound placeId) = Just $ "Ticket place not found: " <> show placeId
  toMessage (BusinessHourNotFound businessHourId) = Just $ "Business hour not found: " <> show businessHourId
  toMessage (ServiceCategoryNotFound sCategoryId) = Just $ "Service category not found: " <> show sCategoryId
  toMessage (TicketSeatManagementNotFound seatMId curDate) = Just $ "Seat management details not found for seat management id: " <> show seatMId <> " and date: " <> show curDate
  toMessage (PeopleCategoryNotFound pCatId) = Just $ "People category not found: " <> show pCatId

instance IsHTTPError TicketBookingError where
  toErrorCode = \case
    TicketServiceNotFound _ -> "TICKET_SERVICE_NOT_FOUND"
    TicketBookingNotFound _ -> "TICKET_BOOKING_NOT_FOUND"
    TicketBookingNotConfirmed _ -> "TICKET_BOOKING_NOT_CONFIRMED"
    TicketBookingServiceNotFound _ -> "TICKET_BOOKING_SERVICE_NOT_FOUND"
    TicketBookingServiceNotConfirmed _ -> "TICKET_BOOKING_SERVICE_NOT_CONFIRMED"
    TicketPlaceNotFound _ -> "TICKET_PLACE_NOT_FOUND"
    BusinessHourNotFound _ -> "BUSINESS_HOUR_NOT_FOUND"
    ServiceCategoryNotFound _ -> "SERVICE_CATEGORY_NOT_FOUND"
    TicketSeatManagementNotFound _ _ -> "TICKET_SEAT_MANAGEMENT_NOT_FOUND"
    PeopleCategoryNotFound _ -> "PEOPLE_CATEGORY_NOT_FOUND"
  toHttpCode = \case
    TicketServiceNotFound _ -> E500
    TicketBookingNotFound _ -> E500
    TicketBookingServiceNotFound _ -> E400
    TicketBookingNotConfirmed _ -> E400
    TicketBookingServiceNotConfirmed _ -> E400
    TicketPlaceNotFound _ -> E500
    BusinessHourNotFound _ -> E500
    ServiceCategoryNotFound _ -> E500
    TicketSeatManagementNotFound _ _ -> E500
    PeopleCategoryNotFound _ -> E500

instance IsAPIError TicketBookingError

data PassError
  = PassNotFound Text
  | PassCategoryNotFound Text
  | PassTypeNotFound Text
  | PurchasedPassNotFound Text
  | PurchasedPassPaymentNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''PassError

instance IsBaseError PassError where
  toMessage (PassNotFound passId) = Just $ "Pass not found: " <> show passId
  toMessage (PassCategoryNotFound categoryId) = Just $ "Pass category not found: " <> show categoryId
  toMessage (PassTypeNotFound typeId) = Just $ "Pass type not found: " <> show typeId
  toMessage (PurchasedPassNotFound purchasedPassId) = Just $ "Purchased pass not found: " <> show purchasedPassId
  toMessage (PurchasedPassPaymentNotFound purchasedPassPaymentId) = Just $ "Purchased pass payment not found: " <> show purchasedPassPaymentId

instance IsHTTPError PassError where
  toErrorCode = \case
    PassNotFound _ -> "PASS_NOT_FOUND"
    PassCategoryNotFound _ -> "PASS_CATEGORY_NOT_FOUND"
    PassTypeNotFound _ -> "PASS_TYPE_NOT_FOUND"
    PurchasedPassNotFound _ -> "PURCHASED_PASS_NOT_FOUND"
    PurchasedPassPaymentNotFound _ -> "PURCHASED_PASS_PAYMENT_NOT_FOUND"
  toHttpCode = \case
    PassNotFound _ -> E500
    PassCategoryNotFound _ -> E500
    PassTypeNotFound _ -> E500
    PurchasedPassNotFound _ -> E500
    PurchasedPassPaymentNotFound _ -> E500

instance IsAPIError PassError

data RiderError
  = RiderConfigNotFound Text
  | RiderConfigDoesNotExist Text
  | RiderConfigFieldIsEmpty Text Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RiderError

instance IsBaseError RiderError where
  toMessage (RiderConfigNotFound merchantOperatingCityId) = Just $ "Rider with merchantOperatingCityId \"" <> show merchantOperatingCityId <> "\" not found."
  toMessage (RiderConfigDoesNotExist merchantOperatingCityId) = Just $ "Rider with merchantOperatingCityId \"" <> show merchantOperatingCityId <> "\" does not exist."
  toMessage (RiderConfigFieldIsEmpty field merchantOperatingCityId) = Just $ "RiderConfig with merchantOperatingCityId \"" <> merchantOperatingCityId <> "\" , field \"" <> field <> "\" is empty."

instance IsHTTPError RiderError where
  toErrorCode = \case
    RiderConfigNotFound _ -> "RIDER_NOT_FOUND"
    RiderConfigDoesNotExist _ -> "RIDER_NOT_EXISTS"
    RiderConfigFieldIsEmpty _ _ -> "RIDER_CONFIG_FIELD_IS_EMPTY"
  toHttpCode = \case
    RiderConfigNotFound _ -> E500
    RiderConfigDoesNotExist _ -> E400
    RiderConfigFieldIsEmpty _ _ -> E400

instance IsAPIError RiderError

data LocationMappingError
  = FromLocationMappingNotFound Text
  | FromLocationNotFound Text
  | StopsLocationMappingNotFound Text
  | StopsLocationNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''LocationMappingError

instance IsBaseError LocationMappingError where
  toMessage = \case
    FromLocationMappingNotFound id_ -> Just $ "From location mapping not found for entity id: " <> id_ <> "."
    FromLocationNotFound id_ -> Just $ "From location not found for locationId: " <> id_ <> "."
    StopsLocationMappingNotFound id_ -> Just $ "Stops location mapping not found for entity id: " <> id_ <> "."
    StopsLocationNotFound id_ -> Just $ "Stops location not found for locationId: " <> id_ <> "."

instance IsHTTPError LocationMappingError where
  toErrorCode = \case
    FromLocationMappingNotFound _ -> "FROM_LOCATION_MAPPING_NOT_FOUND"
    FromLocationNotFound _ -> "FROM_LOCATION_NOT_FOUND"
    StopsLocationMappingNotFound _ -> "STOPS_LOCATION_MAPPING_NOT_FOUND"
    StopsLocationNotFound _ -> "STOPS_LOCATION_NOT_FOUND"

  toHttpCode _ = E500

instance IsAPIError LocationMappingError

newtype BecknSchemaError
  = InvalidBecknSchema Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''BecknSchemaError

instance IsBaseError BecknSchemaError where
  toMessage (InvalidBecknSchema msg) = Just $ "Invalid Beckn Schema:-" <> msg

instance IsHTTPError BecknSchemaError where
  toErrorCode (InvalidBecknSchema _) = "INVALID_BECKN_SCHEMA"
  toHttpCode (InvalidBecknSchema _) = E400

instance IsAPIError BecknSchemaError

newtype APIKeyError
  = InvalidAPIKey Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''APIKeyError

instance IsBaseError APIKeyError where
  toMessage = \case
    InvalidAPIKey "" -> Just "Invalid API Key"
    InvalidAPIKey msg -> Just $ "Invalid API Key:-" <> msg

instance IsHTTPError APIKeyError where
  toErrorCode = \case
    InvalidAPIKey _ -> "INVALID_API_KEY"

  toHttpCode = \case
    InvalidAPIKey _ -> E401

instance IsAPIError APIKeyError

data PartnerOrgConfigError
  = PartnerOrgConfigNotFound Text Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''PartnerOrgConfigError

instance IsBaseError PartnerOrgConfigError where
  toMessage = \case
    PartnerOrgConfigNotFound partnerOrgId cfgType -> Just $ "PartnerOrg Config with partnerOrgId:" +|| partnerOrgId ||+ " and configType:" +|| cfgType ||+ " not found."

instance IsHTTPError PartnerOrgConfigError where
  toErrorCode = \case
    PartnerOrgConfigNotFound _ _ -> "PARTNER_ORG_CONFIG_NOT_FOUND"

  toHttpCode = \case
    PartnerOrgConfigNotFound _ _ -> E500

instance IsAPIError PartnerOrgConfigError

data RouteError
  = RouteNotFound Text
  | RouteDoesNotExist Text
  | RouteFareNotFound Text Text Text
  | RouteMappingNotFound Text
  | RouteMappingDoesNotExist Text Text Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RouteError

instance IsBaseError RouteError where
  toMessage = \case
    RouteNotFound msg -> Just $ "Route Not Found:-" <> msg
    RouteDoesNotExist msg -> Just $ "Route Does Not Exist:-" <> msg
    RouteMappingNotFound msg -> Just $ "Route Mapping Not Found:-" <> msg
    RouteMappingDoesNotExist routeCode startStop integratedBppConfigId -> Just $ "Route Mapping Does Not Exist:-" <> routeCode <> ", " <> startStop <> ", " <> integratedBppConfigId
    RouteFareNotFound routeCode startStop endStop -> Just $ "Route Fare Not Found:-" <> routeCode <> ", " <> startStop <> ", " <> endStop

instance IsHTTPError RouteError where
  toErrorCode = \case
    RouteNotFound _ -> "ROUTE_NOT_FOUND"
    RouteDoesNotExist _ -> "ROUTE_DOES_NOT_EXIST"
    RouteMappingNotFound _ -> "ROUTE_MAPPING_NOT_FOUND"
    RouteMappingDoesNotExist _ _ _ -> "ROUTE_MAPPING_DOES_NOT_EXIST"
    RouteFareNotFound _ _ _ -> "ROUTE_FARE_NOT_FOUND"

  toHttpCode = \case
    RouteNotFound _ -> E500
    RouteDoesNotExist _ -> E400
    RouteFareNotFound _ _ _ -> E500
    RouteMappingDoesNotExist _ _ _ -> E500
    RouteMappingNotFound _ -> E500

instance IsAPIError RouteError

data StationError
  = StationNotFound Text
  | StationDoesNotExist Text
  | StationsNotFound Text Text
  | InvalidStationData Text -- station data issue
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''StationError

instance IsBaseError StationError where
  toMessage = \case
    StationNotFound msg -> Just $ "Station Not Found:-" <> msg
    StationDoesNotExist msg -> Just $ "Station Does Not Exist:-" <> msg
    StationsNotFound start end -> Just $ "Station Not Found:-" <> start <> ", " <> end
    InvalidStationData reason -> Just $ "Invalid station data: " <> reason

instance IsHTTPError StationError where
  toErrorCode = \case
    StationNotFound _ -> "STATION_NOT_FOUND"
    StationDoesNotExist _ -> "STATION_DOES_NOT_EXIST"
    StationsNotFound _ _ -> "STATIONS_NOT_FOUND"
    InvalidStationData _ -> "INVALID_STATION_DATA"

  toHttpCode = \case
    StationNotFound _ -> E500
    StationDoesNotExist _ -> E400
    StationsNotFound _ _ -> E500
    InvalidStationData _ -> E400

instance IsAPIError StationError

data GoogleWalletError
  = JWTSignError Text
  | FailedToCallWalletAPI Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''GoogleWalletError

instance IsBaseError GoogleWalletError where
  toMessage = \case
    JWTSignError msg -> Just $ "JWT Sign Error:-" <> msg
    FailedToCallWalletAPI msg -> Just $ "Failed To Call Wallet API:-" <> msg

instance IsHTTPError GoogleWalletError where
  toErrorCode = \case
    JWTSignError _ -> "JWT_SIGN_ERROR"
    FailedToCallWalletAPI _ -> "FAILED_TO_CALL_WALLET_API"

  toHttpCode = \case
    JWTSignError _ -> E500
    FailedToCallWalletAPI _ -> E400

instance IsAPIError GoogleWalletError

data PartnerOrgStationError
  = PartnerOrgStationNotFound Text Text
  | PartnerOrgStationNotFoundForStationId Text Text
  | PartnerOrgStationDoesNotExist Text Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''PartnerOrgStationError

instance IsBaseError PartnerOrgStationError where
  toMessage = \case
    PartnerOrgStationNotFound pOrgId pOrgStationId -> Just $ "PartnerOrg Station for partnerOrgId:" +|| pOrgId ||+ " and partnerOrgStationId:" +|| pOrgStationId ||+ " not found."
    PartnerOrgStationNotFoundForStationId pOrgId stationId -> Just $ "PartnerOrg Station for partnerOrgId:" +|| pOrgId ||+ " and stationId:" +|| stationId ||+ " not found."
    PartnerOrgStationDoesNotExist pOrgId pOrgStationId -> Just $ "PartnerOrg Station for partnerOrgId:" +|| pOrgId ||+ " and partnerOrgStationId:" +|| pOrgStationId ||+ " does not exist."

instance IsHTTPError PartnerOrgStationError where
  toErrorCode = \case
    PartnerOrgStationNotFound _ _ -> "PARTNER_ORG_STATION_NOT_FOUND"
    PartnerOrgStationNotFoundForStationId _ _ -> "PARTNER_ORG_STATION_NOT_FOUND_FOR_STATION_ID"
    PartnerOrgStationDoesNotExist _ _ -> "PARTNER_ORG_STATION_DOES_NOT_EXIST"

  toHttpCode = \case
    PartnerOrgStationNotFound _ _ -> E500
    PartnerOrgStationNotFoundForStationId _ _ -> E500
    PartnerOrgStationDoesNotExist _ _ -> E400

instance IsAPIError PartnerOrgStationError

data FRFSConfigError
  = FRFSConfigNotFound Text
  | FRFSConfigDoesNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FRFSConfigError

instance IsBaseError FRFSConfigError where
  toMessage = \case
    FRFSConfigNotFound mocId -> Just $ "FRFS Config with merchantOperatingCityId:" +|| mocId ||+ " not found."
    FRFSConfigDoesNotExist mocId -> Just $ "FRFS Config with merchantOperatingCityId:" +|| mocId ||+ " does not exist."

instance IsHTTPError FRFSConfigError where
  toErrorCode = \case
    FRFSConfigNotFound _ -> "FRFS_CONFIG_NOT_FOUND"
    FRFSConfigDoesNotExist _ -> "FRFS_CONFIG_DOES_NOT_EXIST"

  toHttpCode = \case
    FRFSConfigNotFound _ -> E500
    FRFSConfigDoesNotExist _ -> E400

instance IsAPIError FRFSConfigError

data FRFSTicketError
  = FRFSTicketDoesNotExist Text
  | FRFSTicketExpired Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FRFSTicketError

instance IsBaseError FRFSTicketError where
  toMessage = \case
    FRFSTicketDoesNotExist ticketId -> Just $ "FRFS Ticket with ticketId:" +|| ticketId ||+ " does not exist."
    FRFSTicketExpired ticketId -> Just $ "FRFS Ticket with ticketId:" +|| ticketId ||+ " has expired."

instance IsHTTPError FRFSTicketError where
  toErrorCode = \case
    FRFSTicketDoesNotExist _ -> "FRFS_TICKET_DOES_NOT_EXIST"
    FRFSTicketExpired _ -> "FRFS_TICKET_EXPIRED"

  toHttpCode = \case
    FRFSTicketDoesNotExist _ -> E400
    FRFSTicketExpired _ -> E400

instance IsAPIError FRFSTicketError

data FRFSTicketBookingError
  = FRFSTicketBookingNotFound Text
  | FRFSTicketBookingDoesNotExist Text
  | FRFSTicketsForBookingExpired Text
  | FRFSTicketsForBookingDoesNotExist Text
  | FRFSQuoteExpired Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FRFSTicketBookingError

instance IsBaseError FRFSTicketBookingError where
  toMessage = \case
    FRFSTicketBookingNotFound bookingId -> Just $ "FRFS Ticket Booking with bookingId:" +|| bookingId ||+ " not found."
    FRFSTicketBookingDoesNotExist bookingId -> Just $ "FRFS Ticket Booking with bookingId:" +|| bookingId ||+ " does not exist."
    FRFSTicketsForBookingExpired bookingId -> Just $ "FRFS Tickets for booking with bookingId:" +|| bookingId ||+ " has expired."
    FRFSTicketsForBookingDoesNotExist bookingId -> Just $ "FRFS Tickets for booking with bookingId:" +|| bookingId ||+ " does not exist."
    FRFSQuoteExpired _ -> Just $ "Quote expired"

instance IsHTTPError FRFSTicketBookingError where
  toErrorCode = \case
    FRFSTicketBookingNotFound _ -> "FRFS_TICKET_BOOKING_NOT_FOUND"
    FRFSTicketBookingDoesNotExist _ -> "FRFS_TICKET_BOOKING_DOES_NOT_EXIST"
    FRFSTicketsForBookingExpired _ -> "FRFS_TICKETS_FOR_BOOKING_EXPIRED"
    FRFSTicketsForBookingDoesNotExist _ -> "FRFS_TICKETS_FOR_BOOKING_DOES_NOT_EXIST"
    FRFSQuoteExpired _ -> "FRFS_QUOTE_EXPIRED"

  toHttpCode = \case
    FRFSTicketBookingNotFound _ -> E500
    FRFSTicketBookingDoesNotExist _ -> E400
    FRFSTicketsForBookingExpired _ -> E400
    FRFSTicketsForBookingDoesNotExist _ -> E400
    FRFSQuoteExpired _ -> E400

instance IsAPIError FRFSTicketBookingError

newtype BecknConfigError
  = BecknConfigNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''BecknConfigError

instance IsBaseError BecknConfigError where
  toMessage = \case
    BecknConfigNotFound msg -> Just $ "Beckn Config not found:-" <> msg

instance IsHTTPError BecknConfigError where
  toErrorCode = \case
    BecknConfigNotFound _ -> "BECKN_CONFIG_NOT_FOUND"

  toHttpCode = \case
    BecknConfigNotFound _ -> E500

instance IsAPIError BecknConfigError

data IntegratedBPPConfigError
  = IntegratedBPPConfigNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''IntegratedBPPConfigError

instance IsBaseError IntegratedBPPConfigError where
  toMessage = \case
    IntegratedBPPConfigNotFound -> Just "IntegratedBPPConfig not found"

instance IsHTTPError IntegratedBPPConfigError where
  toErrorCode = \case
    IntegratedBPPConfigNotFound -> "INTEGRATED_BPP_CONFIG_NOT_FOUND"

  toHttpCode = \case
    IntegratedBPPConfigNotFound -> E500

instance IsAPIError IntegratedBPPConfigError

data PaymentError
  = PaymentMethodRequired
  | CustomerPaymentIdNotFound Text
  | PaymentMethodIdNotFound Text
  | DriverAccountIdNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''PaymentError

instance IsBaseError PaymentError where
  toMessage = \case
    PaymentMethodRequired -> Just "Payment method is required to book a ride"
    CustomerPaymentIdNotFound customerId -> Just $ "Customer payment id with id \"" <> show customerId <> "\" not found."
    PaymentMethodIdNotFound bookingId -> Just $ "Payment method for booking with id \"" <> show bookingId <> "\" not found."
    DriverAccountIdNotFound bookingId -> Just $ "Driver account for booking with id \"" <> show bookingId <> "\" not found."

instance IsHTTPError PaymentError where
  toErrorCode = \case
    PaymentMethodRequired -> "PAYMENT_METHOD_REQUIRED"
    CustomerPaymentIdNotFound _ -> "CUSTOMER_PAYMENT_ID_NOT_FOUND"
    PaymentMethodIdNotFound _ -> "PAYMENT_METHOD_ID_NOT_FOUND"
    DriverAccountIdNotFound _ -> "DRIVER_ACCOUNT_ID_NOT_FOUND"

  toHttpCode = \case
    PaymentMethodRequired -> E400
    CustomerPaymentIdNotFound _ -> E500
    PaymentMethodIdNotFound _ -> E500
    DriverAccountIdNotFound _ -> E500

instance IsAPIError PaymentError

data FRFSTicketBookingPaymentError
  = FRFSTicketBookingPaymentNotFound Text
  | FRFSTicketBookingPaymentDoesNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FRFSTicketBookingPaymentError

instance IsBaseError FRFSTicketBookingPaymentError where
  toMessage = \case
    FRFSTicketBookingPaymentNotFound ticketBookingId -> Just $ "FRFS Ticket Booking Payment with ticketBookingId:" +|| ticketBookingId ||+ " not found."
    FRFSTicketBookingPaymentDoesNotExist ticketBookingId -> Just $ "FRFS Ticket Booking Payment with ticketBookingId:" +|| ticketBookingId ||+ " does not exist."

instance IsHTTPError FRFSTicketBookingPaymentError where
  toErrorCode = \case
    FRFSTicketBookingPaymentNotFound _ -> "FRFS_TICKET_BOOKING_PAYMENT_NOT_FOUND"
    FRFSTicketBookingPaymentDoesNotExist _ -> "FRFS_TICKET_BOOKING_PAYMENT_DOES_NOT_EXIST"

  toHttpCode = \case
    FRFSTicketBookingPaymentNotFound _ -> E500
    FRFSTicketBookingPaymentDoesNotExist _ -> E400

instance IsAPIError FRFSTicketBookingPaymentError

------------------ CAC ---------------------
-- This is for temporary implementation of the CAC auth API. This will be depcricated once we have SSO for CAC.
data CacAuthError = CacAuthError | CacInvalidToken
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''CacAuthError

instance IsBaseError CacAuthError where
  toMessage = \case
    CacAuthError -> Just "Auth Token Missing !!!!!!!!"
    CacInvalidToken -> Just "Invalid Auth Token !!!!!!!!"

instance IsHTTPError CacAuthError where
  toErrorCode = \case
    CacAuthError -> "CAC_AUTH_ERROR"
    CacInvalidToken -> "CAC_INVALID_TOKEN"
  toHttpCode = \case
    CacAuthError -> E401
    CacInvalidToken -> E401

instance IsAPIError CacAuthError

data SafetyError
  = IncidentReportServiceUnavailable Text
  | CallSidNullError
  | CallRecordNotFoundError Text
  | RideIdEmptyInCallRecord Text
  | PoliceCallNotAllowed Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SafetyError

instance IsBaseError SafetyError where
  toMessage = \case
    IncidentReportServiceUnavailable merchantId -> Just ("Incident Report Service not available for merchantOperatingCityId : " <> show merchantId <> ".")
    CallSidNullError -> Just "CallSid not found in IVR response from Exotel."
    CallRecordNotFoundError sid -> Just $ "Call Record not found in DB against callSid : " <> show sid
    RideIdEmptyInCallRecord sid -> Just $ "RideId is empty in Call Record against callSid : " <> show sid
    PoliceCallNotAllowed personId -> Just $ "Police call not allowed by personId : " <> show personId

instance IsHTTPError SafetyError where
  toErrorCode = \case
    IncidentReportServiceUnavailable _ -> "INCIDENT_REPORT_SERVICE_UNAVAILABLE"
    CallSidNullError -> "CALL_SID_NULL_ERROR"
    CallRecordNotFoundError _ -> "CALL_RECORD_NOT_FOUND_ERROR"
    RideIdEmptyInCallRecord _ -> "RIDE_ID_EMPTY_IN_CALL_RECORD"
    PoliceCallNotAllowed _ -> "POLICE_CALL_NOT_ALLOWED"
  toHttpCode = \case
    IncidentReportServiceUnavailable _ -> E400
    CallSidNullError -> E500
    CallRecordNotFoundError _ -> E500
    RideIdEmptyInCallRecord _ -> E500
    PoliceCallNotAllowed _ -> E400

instance IsAPIError SafetyError

data JourneyError
  = JourneyNotFound Text
  | JourneyLegReqDataNotFound Int
  | JourneyLegSearchIdNotFound Text Int
  | JourneyLegNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''JourneyError

instance IsBaseError JourneyError where
  toMessage = \case
    JourneyNotFound journeyId -> Just ("Journey with id: " <> journeyId <> " not found.")
    JourneyLegReqDataNotFound sequenceNumber -> Just ("Request data for journey leg number: " <> show sequenceNumber <> " not found!")
    JourneyLegSearchIdNotFound journeyId legNumber -> Just ("SearchId for JourneyLeg with id: " <> journeyId <> " and legNumber: " <> show legNumber <> " not found.")
    JourneyLegNotFound legId -> Just ("JourneyLeg with id: " <> legId <> " not found.")

instance IsHTTPError JourneyError where
  toErrorCode = \case
    JourneyNotFound _ -> "JOURNEY_NOT_FOUND"
    JourneyLegReqDataNotFound _ -> "JOURNEY_LEG_REQ_DATA_NOT_FOUND"
    JourneyLegSearchIdNotFound _ _ -> "JOURNEY_LEG_SEARCH_ID_NOT_FOUND"
    JourneyLegNotFound _ -> "JOURNEY_LEG_NOT_FOUND"
  toHttpCode = \case
    JourneyNotFound _ -> E400
    JourneyLegReqDataNotFound _ -> E400
    JourneyLegSearchIdNotFound _ _ -> E400
    JourneyLegNotFound _ -> E400

instance IsAPIError JourneyError

data PTCircuitBreakerError
  = PTBookingTemporarilyDisabled Text
  | PTFareCachingTemporarilyDisabled Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''PTCircuitBreakerError

instance IsBaseError PTCircuitBreakerError where
  toMessage = \case
    PTBookingTemporarilyDisabled mode -> Just $ "Public transport booking for " <> mode <> " is temporarily disabled due to provider issues."
    PTFareCachingTemporarilyDisabled mode -> Just $ "Public transport fare service for " <> mode <> " is temporarily unavailable."

instance IsHTTPError PTCircuitBreakerError where
  toErrorCode = \case
    PTBookingTemporarilyDisabled _ -> "PT_BOOKING_TEMPORARILY_DISABLED"
    PTFareCachingTemporarilyDisabled _ -> "PT_FARE_CACHING_TEMPORARILY_DISABLED"
  toHttpCode = \case
    PTBookingTemporarilyDisabled _ -> E503
    PTFareCachingTemporarilyDisabled _ -> E503

instance IsAPIError PTCircuitBreakerError

data CancellationError
  = CancellationNotSupported
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''CancellationError

instance IsBaseError CancellationError where
  toMessage = \case
    CancellationNotSupported -> Just $ "Cancellation Not Allowed"

instance IsHTTPError CancellationError where
  toErrorCode = \case
    CancellationNotSupported -> "CANCELLATION_NOT_SUPPORTED"

  toHttpCode = \case
    CancellationNotSupported -> E500

instance IsAPIError CancellationError

data CategoryError
  = CategoriesIneligible
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''CategoryError

instance IsBaseError CategoryError where
  toMessage = \case
    CategoriesIneligible -> Just $ "Category not eligible"

instance IsHTTPError CategoryError where
  toErrorCode = \case
    CategoriesIneligible -> "CATEGORIES_INELIGIBLE"

  toHttpCode = \case
    CategoriesIneligible -> E400

instance IsAPIError CategoryError

data JourneyLegError
  = JourneyLegCannotBeSwitched Text
  | JourneyLegCannotBeCancelled Text
  | JourneyLegCannotBeSkippedForMode Text
  | JourneyLegCannotBeSkippedForStatus Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''JourneyLegError

instance IsBaseError JourneyLegError where
  toMessage = \case
    JourneyLegCannotBeSwitched journeyLegId -> Just ("JourneyLeg with id: " <> journeyLegId <> " can not be switched.")
    JourneyLegCannotBeCancelled journeyLegId -> Just ("Request data for journey leg number: " <> journeyLegId <> " can not be cancelled!")
    JourneyLegCannotBeSkippedForMode journeyLegMode -> Just ("JourneyLeg cannot be switched for mode: " <> journeyLegMode)
    JourneyLegCannotBeSkippedForStatus journeyLegStatus -> Just ("JourneyLeg cannot be switched for status: " <> journeyLegStatus)

instance IsHTTPError JourneyLegError where
  toErrorCode = \case
    JourneyLegCannotBeSwitched _ -> "JOURNEY_LEG_CANNOT_SWITCH"
    JourneyLegCannotBeCancelled _ -> "JOURNEY_LEG_CANNOT_CANCELLED"
    JourneyLegCannotBeSkippedForMode _ -> "JOURNEY_LEG_CANNOT_SKIP"
    JourneyLegCannotBeSkippedForStatus _ -> "JOURNEY_LEG_CANNOT_SKIP"
  toHttpCode = \case
    JourneyLegCannotBeSwitched _ -> E400
    JourneyLegCannotBeCancelled _ -> E400
    JourneyLegCannotBeSkippedForMode _ -> E400
    JourneyLegCannotBeSkippedForStatus _ -> E400

instance IsAPIError JourneyLegError

data FRFSQuoteError
  = CachedFRFSQuoteAnomaly Text Text
  | FRFSQuoteNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FRFSQuoteError

instance IsBaseError FRFSQuoteError where
  toMessage = \case
    CachedFRFSQuoteAnomaly quotesCreatedByCache quotesCreatedByOnSearch -> Just $ "Quotes created by cache and quotes from on_search does not match. Quotes created by Cache- " <> quotesCreatedByCache <> " Quotes created by on_search- " <> quotesCreatedByOnSearch
    FRFSQuoteNotFound quoteId -> Just $ "FRFS Quote with quoteId " <> quoteId <> " not found."

instance IsHTTPError FRFSQuoteError where
  toErrorCode = \case
    CachedFRFSQuoteAnomaly _ _ -> "FRFS_QUOTE_MISMATCH"
    FRFSQuoteNotFound _ -> "FRFS_QUOTE_NOT_FOUND"
  toHttpCode = \case
    CachedFRFSQuoteAnomaly _ _ -> E500
    FRFSQuoteNotFound _ -> E500

instance IsAPIError FRFSQuoteError

data RedisLockError
  = RedisLockStillProcessing Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RedisLockError

instance IsBaseError RedisLockError where
  toMessage = \case
    RedisLockStillProcessing lockKey -> Just $ "Thread is still processing. Redis Lock Key:- " <> lockKey

instance IsHTTPError RedisLockError where
  toErrorCode = \case
    RedisLockStillProcessing _ -> "REDIS_LOCK_STILL_PROCESSING"

  toHttpCode = \case
    RedisLockStillProcessing _ -> E500

instance IsAPIError RedisLockError

data InvalidFormatError
  = InvalidStationJson Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''InvalidFormatError

instance IsBaseError InvalidFormatError where
  toMessage = \case
    InvalidStationJson stationJson -> Just $ "Invalid stations json from db . Station Json:- " <> stationJson

instance IsHTTPError InvalidFormatError where
  toErrorCode = \case
    InvalidStationJson _ -> "INVALID_STATION_JSON"

  toHttpCode = \case
    InvalidStationJson _ -> E500

instance IsAPIError InvalidFormatError

data FRFSSearchError
  = FRFSSearchNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FRFSSearchError

instance IsBaseError FRFSSearchError where
  toMessage = \case
    FRFSSearchNotFound searchId -> Just $ "FRFS Search with searchId:- " <> searchId <> " not found."

instance IsHTTPError FRFSSearchError where
  toErrorCode = \case
    FRFSSearchNotFound _ -> "FRFS_SEARCH_NOT_FOUND"

  toHttpCode = \case
    FRFSSearchNotFound _ -> E500

instance IsAPIError FRFSSearchError

data FRFSSBookingError
  = FRFSBookingNotMadeThroughPartnerOrg Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FRFSSBookingError

instance IsBaseError FRFSSBookingError where
  toMessage = \case
    FRFSBookingNotMadeThroughPartnerOrg bookingId -> Just $ "This booking is not made by partnerOrg. TicketBookingId :- " <> bookingId

instance IsHTTPError FRFSSBookingError where
  toErrorCode = \case
    FRFSBookingNotMadeThroughPartnerOrg _ -> "BOOKING_NOT_MADE_THROUGH_PARTNERORG"

  toHttpCode = \case
    FRFSBookingNotMadeThroughPartnerOrg _ -> E500

instance IsAPIError FRFSSBookingError

data CustomAuthError = IpHitsLimitExceeded deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''CustomAuthError

instance IsBaseError CustomAuthError where
  toMessage = \case
    IpHitsLimitExceeded -> Just "IP Rate Limit Exceed, Too Many Requests In Short Duration"

instance IsHTTPError CustomAuthError where
  toErrorCode = \case
    IpHitsLimitExceeded -> "IP_HITS_LIMIT_EXCEED"
  toHttpCode = \case
    IpHitsLimitExceeded -> E429

instance IsAPIError CustomAuthError

data SearchCancelErrors = ActiveBookingPresent Text | FailedToCancelSearch Text deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SearchCancelErrors

instance IsBaseError SearchCancelErrors where
  toMessage = \case
    ActiveBookingPresent searchId -> Just $ "Active Booking Present for searchId: " <> searchId
    FailedToCancelSearch searchId -> Just $ "Failed To Cancel for searchId: " <> searchId

instance IsHTTPError SearchCancelErrors where
  toErrorCode = \case
    ActiveBookingPresent _ -> "ACTIVE_BOOKING_PRESENT"
    FailedToCancelSearch _ -> "FAILED_TO_CANCEL"
  toHttpCode = \case
    ActiveBookingPresent _ -> E400
    FailedToCancelSearch _ -> E400

instance IsAPIError SearchCancelErrors

data MultimodalError
  = InvalidStationChange Text Text
  | NoValidMetroRoute Text Text -- source, destination
  | MetroLegNotFound Text -- reason
  | NoValidSubwayRoute Text Text -- source, destination
  | SubwayLegNotFound Text -- reason
  | InvalidLegOrder Int -- legOrder
  | OSRMFailure Text -- reason
  | OTPServiceUnavailable Text -- reason
  | UnsupportedVehicleType Text -- reason
  | VehicleUnserviceableOnRoute Text -- reason
  | VehicleServiceTierUnserviceable Text -- reason
  | InvalidVehicleNumber Text -- reason
  | FleetRouteMapMissing Text -- reason
  | PublicTransportDataUnavailable Text -- reason
  | StopNotFound Text
  | StopDoesNotHaveLocation Text
  | CategoriesAndTotalPriceMismatch Text Text
  | NoSelectedCategoryFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''MultimodalError

instance IsBaseError MultimodalError where
  toMessage = \case
    InvalidStationChange stopCode reason -> Just $ "Invalid station change for stop code " <> stopCode <> ": " <> reason
    NoValidMetroRoute source dest -> Just $ "No valid metro route found between " <> source <> " and " <> dest
    MetroLegNotFound reason -> Just $ "Metro leg not found: " <> reason
    NoValidSubwayRoute source dest -> Just $ "No valid subway route found between " <> source <> " and " <> dest
    SubwayLegNotFound reason -> Just $ "Subway leg not found: " <> reason
    InvalidLegOrder legOrder -> Just $ "Invalid leg order: " <> show legOrder
    OSRMFailure reason -> Just $ "OSRM service failure: " <> reason
    OTPServiceUnavailable reason -> Just $ "OTP service unavailable: " <> reason
    UnsupportedVehicleType reason -> Just $ "Unsupported vehicle type: " <> reason
    VehicleUnserviceableOnRoute reason -> Just $ "Vehicle unserviceable on route: " <> reason
    VehicleServiceTierUnserviceable reason -> Just $ "Vehicle service tier unserviceable: " <> reason
    InvalidVehicleNumber reason -> Just $ "Invalid vehicle number: " <> reason
    FleetRouteMapMissing reason -> Just $ "Fleet route map missing: " <> reason
    PublicTransportDataUnavailable reason -> Just $ "Public transport data unavailable: " <> reason
    StopNotFound reason -> Just $ "Stop not found: " <> reason
    StopDoesNotHaveLocation reason -> Just $ "Stop does not have location: " <> reason
    CategoriesAndTotalPriceMismatch categoriesTotalPrice totalPrice -> Just $ "Categories and total price mismatch: " <> categoriesTotalPrice <> " and " <> totalPrice
    NoSelectedCategoryFound quoteId -> Just $ "No selected category found in quote categories, quoteId : " <> quoteId

instance IsHTTPError MultimodalError where
  toErrorCode = \case
    InvalidStationChange _ _ -> "INVALID_STATION_CHANGE"
    NoValidMetroRoute _ _ -> "NO_VALID_METRO_ROUTE"
    MetroLegNotFound _ -> "METRO_LEG_NOT_FOUND"
    NoValidSubwayRoute _ _ -> "NO_VALID_SUBWAY_ROUTE"
    SubwayLegNotFound _ -> "SUBWAY_LEG_NOT_FOUND"
    InvalidLegOrder _ -> "INVALID_LEG_ORDER"
    OSRMFailure _ -> "OSRM_FAILURE"
    OTPServiceUnavailable _ -> "OTP_SERVICE_UNAVAILABLE"
    UnsupportedVehicleType _ -> "UNSUPPORTED_VEHICLE_TYPE"
    VehicleUnserviceableOnRoute _ -> "VEHICLE_UNSERVICEABLE_ON_ROUTE"
    VehicleServiceTierUnserviceable _ -> "VEHICLE_SERVICE_TIER_UNSERVICEABLE"
    InvalidVehicleNumber _ -> "INVALID_VEHICLE_NUMBER"
    FleetRouteMapMissing _ -> "FLEET_ROUTE_MAP_MISSING"
    PublicTransportDataUnavailable _ -> "PUBLIC_TRANSPORT_DATA_UNAVAILABLE"
    StopNotFound _ -> "STOP_NOT_FOUND"
    StopDoesNotHaveLocation _ -> "STOP_DOES_NOT_HAVE_LOCATION"
    CategoriesAndTotalPriceMismatch _ _ -> "CATEGORIES_AND_TOTAL_PRICE_MISMATCH"
    NoSelectedCategoryFound _ -> "NO_SELECTED_CATEGORY_FOUND"

  toHttpCode = \case
    InvalidStationChange _ _ -> E400
    NoValidMetroRoute _ _ -> E400
    MetroLegNotFound _ -> E400
    NoValidSubwayRoute _ _ -> E400
    SubwayLegNotFound _ -> E400
    InvalidLegOrder _ -> E400
    OSRMFailure _ -> E500
    OTPServiceUnavailable _ -> E503
    UnsupportedVehicleType _ -> E400
    VehicleUnserviceableOnRoute _ -> E400
    VehicleServiceTierUnserviceable _ -> E400
    InvalidVehicleNumber _ -> E400
    FleetRouteMapMissing _ -> E400
    PublicTransportDataUnavailable _ -> E500
    StopNotFound _ -> E400
    StopDoesNotHaveLocation _ -> E400
    CategoriesAndTotalPriceMismatch _ _ -> E500
    NoSelectedCategoryFound _ -> E500

instance IsAPIError MultimodalError

data DepotManagerError
  = DepotManagerNotFound Text
  | DepotFleetInfoNotFound Text
  | DepotManagerDoesNotHaveAccessToFleet Text Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DepotManagerError

instance IsBaseError DepotManagerError where
  toMessage = \case
    DepotManagerNotFound depotManagerId -> Just $ "Depot manager with id: " <> depotManagerId <> " not found."
    DepotManagerDoesNotHaveAccessToFleet depotManagerId fleetId -> Just $ "Depot manager with id: " <> depotManagerId <> " does not have access to fleet with id: " <> fleetId <> "."
    DepotFleetInfoNotFound fleetId -> Just $ "Depot fleet info with id: " <> fleetId <> " not found."

instance IsHTTPError DepotManagerError where
  toErrorCode = \case
    DepotManagerNotFound _ -> "DEPOT_MANAGER_NOT_FOUND"
    DepotManagerDoesNotHaveAccessToFleet _ _ -> "DEPOT_MANAGER_DOES_NOT_HAVE_ACCESS_TO_FLEET"
    DepotFleetInfoNotFound _ -> "DEPOT_FLEET_INFO_NOT_FOUND"
  toHttpCode = \case
    DepotManagerNotFound _ -> E400
    DepotManagerDoesNotHaveAccessToFleet _ _ -> E401
    DepotFleetInfoNotFound _ -> E400

instance IsAPIError DepotManagerError

data GetUserTokenError
  = GetUserIdError Text
  | UserNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''GetUserTokenError

instance IsBaseError GetUserTokenError where
  toMessage = \case
    GetUserIdError appSecretKey -> Just $ "Get user id error for app secret key: " <> appSecretKey
    UserNotFound mobileNumberHash -> Just $ "User with mobile number hash: " <> mobileNumberHash <> " not found."

instance IsHTTPError GetUserTokenError where
  toErrorCode = \case
    GetUserIdError _ -> "GET_USER_ID_ERROR"
    UserNotFound _ -> "USER_NOT_FOUND"
  toHttpCode = \case
    GetUserIdError _ -> E400
    UserNotFound _ -> E400

instance IsAPIError GetUserTokenError

data EmailError
  = InvalidEmailAddress
  | EmailServiceUnavailable
  | EmailRateLimitExceeded
  | EmailConfigurationError
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''EmailError

instance IsBaseError EmailError where
  toMessage = \case
    InvalidEmailAddress -> Just "The email address provided is invalid. Please check and try again."
    EmailServiceUnavailable -> Just "Email service is temporarily unavailable. Please try again later."
    EmailRateLimitExceeded -> Just "Too many email requests. Please wait a moment and try again."
    EmailConfigurationError -> Just "Unable to send email due to a configuration issue. Please contact support."

instance IsHTTPError EmailError where
  toErrorCode = \case
    InvalidEmailAddress -> "INVALID_EMAIL_ADDRESS"
    EmailServiceUnavailable -> "EMAIL_SERVICE_UNAVAILABLE"
    EmailRateLimitExceeded -> "EMAIL_RATE_LIMIT_EXCEEDED"
    EmailConfigurationError -> "EMAIL_CONFIGURATION_ERROR"
  toHttpCode = \case
    InvalidEmailAddress -> E400
    EmailServiceUnavailable -> E503
    EmailRateLimitExceeded -> E429
    EmailConfigurationError -> E500

instance IsAPIError EmailError

data RefundRequestError
  = RefundRequestDoesNotExist Text
  | RefundRequestAlreadyExists Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RefundRequestError

instance IsBaseError RefundRequestError where
  toMessage (RefundRequestDoesNotExist orderId) = Just $ "Refund request matches passed data \"" <> orderId <> "\" does not exist."
  toMessage (RefundRequestAlreadyExists orderId) = Just $ "Refund request \"" <> orderId <> "\" already exists."

instance IsHTTPError RefundRequestError where
  toErrorCode = \case
    RefundRequestDoesNotExist _ -> "REFUND_REQUEST_DOES_NOT_EXIST"
    RefundRequestAlreadyExists _ -> "REFUND_REQUEST_ALREADY_EXISTS"
  toHttpCode = \case
    RefundRequestDoesNotExist _ -> E400
    RefundRequestAlreadyExists _ -> E400

instance IsAPIError RefundRequestError

data GuestUserError
  = GuestUserAccessDenied Text Text
  | GuestLinkTokenMissing Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''GuestUserError

instance IsBaseError GuestUserError where
  toMessage = \case
    GuestUserAccessDenied guestUserId reason -> Just $ "Guest user with id: " <> guestUserId <> ": " <> reason
    GuestLinkTokenMissing linkToken -> Just $ "Guest link token with token: " <> linkToken <> " not found."

instance IsHTTPError GuestUserError where
  toErrorCode = \case
    GuestUserAccessDenied _ _ -> "GUEST_USER_ACCESS_DENIED"
    GuestLinkTokenMissing _ -> "GUEST_LINK_TOKEN_MISSING"
  toHttpCode = \case
    GuestUserAccessDenied _ _ -> E401
    GuestLinkTokenMissing _ -> E400

instance IsAPIError GuestUserError
