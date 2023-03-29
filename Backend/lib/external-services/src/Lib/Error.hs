{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Error where

import EulerHS.Prelude
import EulerHS.Types (KVDBReply)
import Kernel.Types.CommonImport
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Lib.SMS.MyValueFirst.Types (SubmitSmsRes, submitSmsResToText)
import Network.HTTP.Types (Status (statusCode))
import Servant.Client (ResponseF (responseStatusCode))

data VehicleError
  = VehicleNotFound Text
  | VehicleDoesNotExist Text
  | VehicleAlreadyLinked
  | VehicleRegistrationNumberAlreadyExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''VehicleError

instance IsBaseError VehicleError where
  toMessage = \case
    VehicleNotFound vehicleId -> Just $ "Vehicle with vehicleId \"" <> show vehicleId <> "\" not found."
    VehicleDoesNotExist vehicleId -> Just $ "Vehicle with vehicleId \"" <> show vehicleId <> "\" not exist."
    VehicleRegistrationNumberAlreadyExist registrationNo -> Just $ "Vehicle with registrationNumber \"" <> show registrationNo <> "\" already exist."
    _ -> Nothing

instance IsHTTPError VehicleError where
  toErrorCode = \case
    VehicleNotFound _ -> "VEHICLE_NOT_FOUND"
    VehicleDoesNotExist _ -> "VEHICLE_DOES_NOT_EXIST"
    VehicleAlreadyLinked -> "VEHICLE_ALREADY_LINKED"
    VehicleRegistrationNumberAlreadyExist _ -> "REGISTRATION_NUMBER_ALREADY_EXIST"
  toHttpCode = \case
    VehicleNotFound _ -> E500
    VehicleDoesNotExist _ -> E400
    VehicleAlreadyLinked -> E400
    VehicleRegistrationNumberAlreadyExist _ -> E400

instance IsAPIError VehicleError

data PersonError
  = PersonNotFound Text
  | PersonDoesNotExist Text
  | PersonFieldNotPresent Text
  | PersonWithPhoneNotFound Text
  | PersonEmailExists
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''PersonError

instance IsBaseError PersonError where
  toMessage = \case
    PersonNotFound personId -> Just $ "Person with personId \"" <> show personId <> "\" not found."
    PersonDoesNotExist personId -> Just $ "No person matches passed data \"" <> show personId <> "\" not exist."
    PersonFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this person."
    PersonWithPhoneNotFound phone -> Just $ "Person with mobile number \"" <> show phone <> "\" not found."
    PersonEmailExists -> Just "Email is already registered."

instance IsHTTPError PersonError where
  toErrorCode = \case
    PersonNotFound _ -> "PERSON_NOT_FOUND"
    PersonDoesNotExist _ -> "PERSON_DOES_NOT_EXIST"
    PersonFieldNotPresent _ -> "PERSON_FIELD_NOT_PRESENT"
    PersonWithPhoneNotFound _ -> "PERSON_NOT_FOUND"
    PersonEmailExists -> "PERSON_EMAIL_ALREADY_EXISTS"
  toHttpCode = \case
    PersonNotFound _ -> E500
    PersonDoesNotExist _ -> E400
    PersonFieldNotPresent _ -> E500
    PersonWithPhoneNotFound _ -> E422
    PersonEmailExists -> E400

instance IsAPIError PersonError

data MerchantError
  = MerchantNotFound Text
  | MerchantDoesNotExist Text
  | MerchantWithExoPhoneNotFound Text
  | MerchantServiceUsageConfigNotFound Text
  | MerchantServiceConfigNotFound Text MapsService
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''MerchantError

instance IsBaseError MerchantError where
  toMessage (MerchantNotFound merchantId) = Just $ "Merchant with merchantId \"" <> show merchantId <> "\" not found."
  toMessage (MerchantDoesNotExist merchantId) = Just $ "No merchant matches passed data " <> show merchantId <> "."
  toMessage (MerchantWithExoPhoneNotFound exoPhone) = Just $ "Merchant with ExoPhone \"" <> show exoPhone <> "\" not found."
  toMessage (MerchantServiceUsageConfigNotFound merchantId) = Just $ "MerchantServiceUsageConfig with merchantId \"" <> show merchantId <> "\" not found."
  toMessage (MerchantServiceConfigNotFound merchantId service) = Just $ "MerchantServiceConfig for service " <> show service <> " with merchantId \"" <> merchantId <> "\" not found."

instance IsHTTPError MerchantError where
  toErrorCode = \case
    MerchantNotFound _ -> "MERCHANT_NOT_FOUND"
    MerchantDoesNotExist _ -> "MERCHANT_DOES_NOT_EXIST"
    MerchantWithExoPhoneNotFound _ -> "MERCHANT_WITH_EXO_PHONE_NOT_FOUND"
    MerchantServiceUsageConfigNotFound _ -> "MERCHANT_SERVICE_USAGE_CONFIG_NOT_FOUND"
    MerchantServiceConfigNotFound _ _ -> "MERCHANT_SERVICE_CONFIG_NOT_FOUND"
  toHttpCode = \case
    MerchantNotFound _ -> E500
    MerchantDoesNotExist _ -> E400
    MerchantWithExoPhoneNotFound _ -> E500
    MerchantServiceUsageConfigNotFound _ -> E500
    MerchantServiceConfigNotFound _ _ -> E500

instance IsAPIError MerchantError

data LocationError = LocationNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''LocationError

instance IsBaseError LocationError where
  toMessage LocationNotFound = Just "Location not found."

instance IsHTTPError LocationError where
  toErrorCode LocationNotFound = "LOCATION_NOT_FOUND"
  toHttpCode LocationNotFound = E500

instance IsAPIError LocationError

data SearchRequestError
  = SearchRequestNotFound Text
  | SearchRequestDoesNotExist Text
  | SearchRequestExpired
  | SearchRequestCancelled Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SearchRequestError

instance IsBaseError SearchRequestError where
  toMessage = \case
    SearchRequestNotFound searchId -> Just $ "Search with searchId \"" <> show searchId <> "\"not found. "
    SearchRequestDoesNotExist searchId -> Just $ "No case matches passed data \"<>" <> show searchId <> "\" not exist"
    SearchRequestCancelled searchId -> Just $ "Search with searchId \"<>" <> show searchId <> "\" was cancelled. "
    _ -> Nothing

instance IsHTTPError SearchRequestError where
  toErrorCode = \case
    SearchRequestNotFound _ -> "SEARCH_REQUEST_NOT_FOUND"
    SearchRequestDoesNotExist _ -> "SEARCH_REQUEST_DOES_NOT_EXIST"
    SearchRequestExpired -> "SEARCH_REQUEST_EXPIRED"
    SearchRequestCancelled _ -> "SEARCH_REQUEST_CANCELLED"
  toHttpCode = \case
    SearchRequestNotFound _ -> E500
    SearchRequestDoesNotExist _ -> E400
    SearchRequestExpired -> E400
    SearchRequestCancelled _ -> E403

instance IsAPIError SearchRequestError

data QuoteError
  = QuoteNotFound Text
  | QuoteDoesNotExist Text
  | QuoteExpired Text
  | QuoteFieldNotPresent Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''QuoteError

instance IsBaseError QuoteError where
  toMessage = \case
    QuoteNotFound quoteId -> Just $ "Quote with quoteId \"" <> show quoteId <> "\" not found. "
    QuoteDoesNotExist quoteId -> Just $ "No quote matches passed data \"" <> show quoteId <> "\" not exist. "
    QuoteExpired quoteId -> Just $ "Quote with quoteId \"" <> show quoteId <> "\" has already expired. "
    QuoteFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this quote."

instance IsHTTPError QuoteError where
  toErrorCode = \case
    QuoteNotFound _ -> "QUOTE_NOT_FOUND"
    QuoteDoesNotExist _ -> "QUOTE_DOES_NOT_EXIST"
    QuoteExpired _ -> "QUOTE_EXPIRED"
    QuoteFieldNotPresent _ -> "QUOTE_FIELD_NOT_PRESENT"
  toHttpCode = \case
    QuoteNotFound _ -> E500
    QuoteDoesNotExist _ -> E400
    QuoteExpired _ -> E400
    QuoteFieldNotPresent _ -> E500

instance IsAPIError QuoteError

data BookingError
  = BookingNotFound Text
  | BookingDoesNotExist Text
  | BookingFieldNotPresent Text
  | BookingForRiderNotFound Text
  | BookingInvalidStatus Text
  | BookingBppOrderIdNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''BookingError

instance IsBaseError BookingError where
  toMessage = \case
    BookingNotFound bookingId -> Just $ "Booking with bookingId \"" <> show bookingId <> "\" not found. "
    BookingDoesNotExist bookingId -> Just $ "No booking matches passed data \"" <> show bookingId <> "\" not exist. "
    BookingFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this booking."
    BookingForRiderNotFound riderId -> Just $ "Booking with riderId \"" <> show riderId <> "\" not found. "
    BookingInvalidStatus msg -> Just $ "Attempted to do some action in wrong booking status. " <> msg
    _ -> Nothing

instance IsHTTPError BookingError where
  toErrorCode = \case
    BookingNotFound _ -> "BOOKING_NOT_FOUND"
    BookingDoesNotExist _ -> "BOOKING_DOES_NOT_EXIST"
    BookingFieldNotPresent _ -> "BOOKING_FIELD_NOT_PRESENT"
    BookingForRiderNotFound _ -> "BOOKING_NOT_FOUND"
    BookingInvalidStatus _ -> "BOOKING_INVALID_STATUS"
    BookingBppOrderIdNotFound -> "BOOKING_BPP_ORDER_ID_NOT_FOUND"
  toHttpCode = \case
    BookingNotFound _ -> E500
    BookingDoesNotExist _ -> E400
    BookingFieldNotPresent _ -> E500
    BookingForRiderNotFound _ -> E400
    BookingInvalidStatus _ -> E400
    BookingBppOrderIdNotFound -> E500

instance IsAPIError BookingError

data RideError
  = RideNotFound Text
  | RideDoesNotExist Text
  | RideFieldNotPresent Text
  | RideWithBookingIdNotFound Text
  | RideForDriverNotFound Text
  | RideInvalidStatus Text
  | DriverNotAtPickupLocation Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RideError

instance IsBaseError RideError where
  toMessage = \case
    RideNotFound rideId -> Just $ "Ride with rideId \"" <> show rideId <> "\"not found. "
    RideDoesNotExist rideId -> Just $ "No ride matches passed data \"" <> show rideId <> "\" not exist. "
    RideFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this ride."
    RideWithBookingIdNotFound bookingId -> Just $ "Ride with booking id \"" <> show bookingId <> "\"not found. "
    RideForDriverNotFound driverId -> Just $ "Ride for driver id \"" <> show driverId <> "\"not found. "
    RideInvalidStatus msg -> Just $ "Attempted to do some action in wrong ride status. " <> msg
    DriverNotAtPickupLocation driverId -> Just $ "Driver id \"" <> show driverId <> "\" has not reached the pickup location."

instance IsHTTPError RideError where
  toErrorCode = \case
    RideNotFound _ -> "RIDE_NOT_FOUND"
    RideDoesNotExist _ -> "RIDE_DOES_NOT_EXIST"
    RideFieldNotPresent _ -> "RIDE_FIELD_NOT_PRESENT"
    RideWithBookingIdNotFound _ -> "RIDE_NOT_FOUND"
    RideForDriverNotFound _ -> "RIDE_NOT_FOUND"
    RideInvalidStatus _ -> "RIDE_INVALID_STATUS"
    DriverNotAtPickupLocation _ -> "DRIVER_NOT_AT_PICKUP_LOCATION"

  toHttpCode = \case
    RideNotFound _ -> E500
    RideDoesNotExist _ -> E400
    RideFieldNotPresent _ -> E500
    RideWithBookingIdNotFound _ -> E500
    RideForDriverNotFound _ -> E422
    RideInvalidStatus _ -> E400
    DriverNotAtPickupLocation _ -> E400

instance IsAPIError RideError

newtype RiderDetailsError = RiderDetailsNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RiderDetailsError

instance IsBaseError RiderDetailsError where
  toMessage (RiderDetailsNotFound rideDetailId) = Just $ "RideDetails with rideDetailsId \"" <> show rideDetailId <> "\" not found. "

instance IsHTTPError RiderDetailsError where
  toErrorCode _ = "RIDER_DETAILS_NOT_FOUND"
  toHttpCode _ = E500

instance IsAPIError RiderDetailsError

data SMSError
  = SMSError SubmitSmsRes
  | SMSInvalidNumber
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SMSError

instance IsBaseError SMSError where
  toMessage = \case
    SMSError err -> Just $ submitSmsResToText err
    _ -> Nothing

instance IsHTTPError SMSError where
  toErrorCode = \case
    SMSError _ -> "SMS_NOT_SENT"
    SMSInvalidNumber -> "SMS_INVALID_NUMBER"

  toHttpCode = \case
    SMSError _ -> E500
    SMSInvalidNumber -> E400

instance IsAPIError SMSError

data GoogleMapsCallError = GoogleMapsInvalidRequest | GoogleMapsCallError Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''GoogleMapsCallError

instance IsBaseError GoogleMapsCallError where
  toMessage = \case
    GoogleMapsInvalidRequest -> Just "Invalid request to Google Maps."
    GoogleMapsCallError googleErrorCode -> Just googleErrorCode

instance IsHTTPError GoogleMapsCallError where
  toErrorCode = \case
    GoogleMapsInvalidRequest -> "GOOGLE_MAPS_INVALID_REQUEST"
    GoogleMapsCallError _ -> "GOOGLE_MAPS_CALL_ERROR"
  toHttpCode = \case
    GoogleMapsInvalidRequest -> E400
    GoogleMapsCallError _ -> E500

instance IsAPIError GoogleMapsCallError

data GoogleTranslateCallError = GoogleTranslateInvalidRequest
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''GoogleTranslateCallError

instance IsBaseError GoogleTranslateCallError where
  toMessage GoogleTranslateInvalidRequest = Just "Invalid request to Google Translate."

instance IsHTTPError GoogleTranslateCallError where
  toErrorCode GoogleTranslateInvalidRequest = "GOOGLE_TRANSLATE_INVALID_REQUEST"
  toHttpCode GoogleTranslateInvalidRequest = E400

instance IsAPIError GoogleTranslateCallError

data GupShupError
  = GupShupInvalidRequest
  | GupShupNotConfigured
  | GupShupUserIdNotFound
  | GupShupInvalidPhoneNumber
  | GupShupUnauthorized
  | GupShupWrongMethodService
  | GupShupInterNationalPhoneNumber
  | GupShupTooManyRequests
  | GupShupUnknownServerError
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''GupShupError

instance IsBaseError GupShupError where
  toMessage = \case
    GupShupInvalidRequest -> Just "Invalid request to GupShup."
    GupShupNotConfigured -> Just "GupShup env variables aren't properly set."
    GupShupUserIdNotFound -> Just "GupShup Authentication Failed as userid X does not exist."
    GupShupInvalidPhoneNumber -> Just "The phone number XXXXX is not a valid phone number."
    GupShupUnauthorized -> Just "Authentication failed due to invalid userId or password."
    GupShupWrongMethodService -> Just "The method is not supported."
    GupShupInterNationalPhoneNumber -> Just "The INTERNATIONAL_PHONE service is disabled for you. Kindly get the service enabled before using this action"
    GupShupTooManyRequests -> Just "The phone number has already been marked as requested"
    GupShupUnknownServerError -> Just "An unknown exception has occurred. Please retry the request after some time."

instance IsHTTPError GupShupError where
  toErrorCode = \case
    GupShupNotConfigured -> "GUPSHUP_NOT_CONFIGURED"
    GupShupInvalidRequest -> "GUPSHUP_INVALID_REQUEST"
    GupShupUserIdNotFound -> "GUPSHUP_USER_NOT_FOUND"
    GupShupInvalidPhoneNumber -> "GUPSHUP_INVALID_PHONE_NUMBER"
    GupShupUnauthorized -> "GUPSHUP_AUTHENTICATION_FAILED"
    GupShupWrongMethodService -> "GUPSHUP_WRONG_METHOD_SERVICE"
    GupShupInterNationalPhoneNumber -> "GUPSHUP_INTERNATIONAL_PHONE_DISABLED"
    GupShupTooManyRequests -> "GUPSHUP_TOO_MANY_REQUEST_FOR_SAME"
    GupShupUnknownServerError -> "GUPSHUP_UNKNOWN_ERROR"

instance FromResponse GupShupError where
  fromResponse resp = case statusCode $ responseStatusCode resp of
    400 -> Just GupShupInvalidRequest
    _ -> Just GupShupNotConfigured

instance IsAPIError GupShupError

data AgencyDisabled
  = AgencyDisabled
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''AgencyDisabled

instance IsBaseError AgencyDisabled

instance IsHTTPError AgencyDisabled where
  toErrorCode AgencyDisabled = "AGENCY_DISABLED"
  toHttpCode AgencyDisabled = E503

instance IsAPIError AgencyDisabled

data ExotelError
  = ExotelNotConfigured
  | ExotelBadRequest
  | ExotelUnauthorized
  | ExitelPaymentRequired
  | ExotelAccessDenied
  | ExotelNotFound
  | ExotelConflict
  | ExotelTooManyRequests
  | ExotelServerError
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''ExotelError

instance FromResponse ExotelError where
  fromResponse resp = case statusCode $ responseStatusCode resp of
    400 -> Just ExotelBadRequest
    401 -> Just ExotelUnauthorized
    402 -> Just ExitelPaymentRequired
    403 -> Just ExotelAccessDenied
    404 -> Just ExotelNotFound
    409 -> Just ExotelConflict
    429 -> Just ExotelTooManyRequests
    _ -> Just ExotelServerError

instance IsBaseError ExotelError where
  toMessage = \case
    ExotelNotConfigured -> Just "Exotel env variables aren't properly set."
    ExotelBadRequest -> Just "Something in your header or request body was malformed."
    ExotelUnauthorized -> Just "Necessary credentials were either missing or invalid."
    ExitelPaymentRequired -> Just "The action is not available on your plan, or you have exceeded usage limits for your current plan."
    ExotelAccessDenied -> Just "Your credentials are valid, but you don’t have access to the requested resource."
    ExotelNotFound -> Just "The object you’re requesting doesn’t exist."
    ExotelConflict -> Just "You might be trying to update the same resource concurrently."
    ExotelTooManyRequests -> Just "You are calling our APIs more frequently than we allow."
    ExotelServerError -> Just "Something went wrong on our end. Please try again."

instance IsHTTPError ExotelError where
  toErrorCode = \case
    ExotelNotConfigured -> "EXOTEL_NOT_CONFIGURED"
    ExotelBadRequest -> "EXOTEL_BAD_REQUEST"
    ExotelUnauthorized -> "EXOTEL_UNAUTHORIZED"
    ExitelPaymentRequired -> "EXOTEL_PAYMENT_REQUIRED"
    ExotelAccessDenied -> "EXOTEL_ACCESS_DENIED"
    ExotelNotFound -> "EXOTEL_NOT_FOUND"
    ExotelConflict -> "EXOTEL_CONFLICT"
    ExotelTooManyRequests -> "EXOTEL_TOO_MANY_REQUESTS"
    ExotelServerError -> "EXOTEL_SERVER_ERROR"

instance IsAPIError ExotelError

data CallStatusError
  = CallStatusDoesNotExist
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''CallStatusError

instance IsBaseError CallStatusError where
  toMessage CallStatusDoesNotExist = Just "No call callback received yet."

instance IsHTTPError CallStatusError where
  toErrorCode CallStatusDoesNotExist = "CALL_DOES_NOT_EXIST"
  toHttpCode CallStatusDoesNotExist = E400

instance IsAPIError CallStatusError

data ServiceabilityError
  = RideNotServiceable
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''ServiceabilityError

instance IsBaseError ServiceabilityError where
  toMessage RideNotServiceable = Just "Requested ride is not serviceable due to georestrictions."

instance IsHTTPError ServiceabilityError where
  toErrorCode RideNotServiceable = "RIDE_NOT_SERVICEABLE"
  toHttpCode RideNotServiceable = E400

instance IsAPIError ServiceabilityError

data IdfyCallError
  = IdfyBadRequest
  | IdfyUnauthorized
  | IdfyAccessDenied
  | IdfyNotFound
  | IdfySizeLimit
  | IdfyUnprocessableEntity
  | IdfyTooManyRequests
  | IdfyServerError
  | IdfyCallError Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''IdfyCallError

instance FromResponse IdfyCallError where
  fromResponse resp = case statusCode $ responseStatusCode resp of
    400 -> Just IdfyBadRequest
    401 -> Just IdfyUnauthorized
    402 -> Just IdfyAccessDenied
    404 -> Just IdfyNotFound
    413 -> Just IdfySizeLimit
    422 -> Just IdfyUnprocessableEntity
    429 -> Just IdfyTooManyRequests
    _ -> Just IdfyServerError

instance IsBaseError IdfyCallError where
  toMessage = \case
    IdfyBadRequest -> Just "Something in your header or request body was malformed."
    IdfyUnauthorized -> Just "Necessary credentials were either missing or invalid."
    IdfyAccessDenied -> Just "Your credentials are valid, but you don’t have access to the requested resource."
    IdfyNotFound -> Just "The object you’re requesting doesn’t exist."
    IdfySizeLimit -> Just "You might be trying to update the same resource concurrently."
    IdfyUnprocessableEntity -> Just "Unprocessable Entity"
    IdfyTooManyRequests -> Just "You are calling our APIs more frequently than we allow."
    IdfyServerError -> Just "Something went wrong on our end. Please try again."
    IdfyCallError googleErrorCode -> Just googleErrorCode

instance IsHTTPError IdfyCallError where
  toErrorCode = \case
    IdfyBadRequest -> "IDFY_BAD_REQUEST"
    IdfyUnauthorized -> "IDFY_UNAUTHORIZED"
    IdfyAccessDenied -> "IDFY_ACCESS_DENIED"
    IdfyNotFound -> "IDFY_NOT_FOUND"
    IdfySizeLimit -> "IDFY_CONFLICT"
    IdfyTooManyRequests -> "IDFY_TOO_MANY_REQUESTS"
    IdfyServerError -> "IDFY_SERVER_ERROR"
    IdfyCallError _ -> "IDFY_CALL_ERROR"
    IdfyUnprocessableEntity -> "IDFY_UNPROCESSABLE_ENTITY"

instance IsAPIError IdfyCallError

data MMIError
  = MMINotConfigured
  | MMIBadRequest
  | MMIUnauthorized
  | MMIForbidden
  | MMIServerError
  | MMIUnderMaintenance
  | MMIDBConnectionError
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''MMIError

instance FromResponse MMIError where
  fromResponse resp = case statusCode $ responseStatusCode resp of
    400 -> Just MMIBadRequest
    401 -> Just MMIUnauthorized
    403 -> Just MMIForbidden
    503 -> Just MMIUnderMaintenance
    204 -> Just MMIDBConnectionError
    _ -> Just MMIServerError

instance IsBaseError MMIError where
  toMessage = \case
    MMINotConfigured -> Just "MMI env variables aren't properly set."
    MMIBadRequest -> Just "Bad request; User made an error while creating a valid request."
    MMIUnauthorized -> Just "Unauthorized, either clientID doesn’t exist or an invalid clientSecret is provided."
    MMIForbidden -> Just "Forbidden."
    MMIUnderMaintenance -> Just "Maintenance break."
    MMIDBConnectionError -> Just "DB Connection error"
    MMIServerError -> Just "Something went wrong."

instance IsHTTPError MMIError where
  toErrorCode = \case
    MMINotConfigured -> "MMI_NOT_CONFIGURED"
    MMIBadRequest -> "MMI_BAD_REQUEST"
    MMIUnauthorized -> "MMI_UNAUTHORIZED"
    MMIForbidden -> "MMI_FORBIDDEN"
    MMIUnderMaintenance -> "MMI_UNDER_MAINTENANCE"
    MMIDBConnectionError -> "MMIDBConnectionError"
    MMIServerError -> "MMI_SERVER_ERROR"

instance IsAPIError MMIError

data MerchantMessageError
  = MerchantMessageNotFound Text Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''MerchantMessageError

instance IsBaseError MerchantMessageError where
  toMessage = \case
    MerchantMessageNotFound merchantId messageKey -> Just $ "MerchantMessage with merchantId \"" <> show merchantId <> " and message key" <> show messageKey <> "\" not found. "

instance IsHTTPError MerchantMessageError where
  toErrorCode = \case
    MerchantMessageNotFound _ _ -> "MERCHANT_MESSAGE_NOT_FOUND"
  toHttpCode = \case
    MerchantMessageNotFound _ _ -> E500

instance IsAPIError MerchantMessageError

newtype SosError = SosIdDoesNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SosError

instance IsBaseError SosError where
  toMessage (SosIdDoesNotExist sosId) = Just $ "Sos with sosId \"" <> show sosId <> "\" not found. "

instance IsHTTPError SosError where
  toErrorCode _ = "SOS_ID_DOES_NOT_EXITS"
  toHttpCode _ = E400

instance IsAPIError SosError

data HealthCheckError
  = ServiceUnavailable
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''HealthCheckError

instance IsBaseError HealthCheckError

instance IsHTTPError HealthCheckError where
  toErrorCode ServiceUnavailable = "SERVICE_UNAVAILABLE"
  toHttpCode ServiceUnavailable = E503

instance IsAPIError HealthCheckError

newtype RedisError
  = RedisError KVDBReply
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RedisError

instance IsBaseError RedisError where
  toMessage = \case
    RedisError err -> Just $ show err

instance IsHTTPError RedisError where
  toErrorCode = \case
    RedisError _ -> "REDIS_ERROR"
  toHttpCode _ = E500

instance IsAPIError RedisError
