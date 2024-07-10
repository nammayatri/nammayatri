{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Tools.Error (module Tools.Error) where

import EulerHS.Prelude
import Kernel.Types.Error as Tools.Error
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse
import Network.HTTP.Types (Status (statusCode))
import Servant.Client (ResponseF (responseStatusCode))

data CustomerError = PersonMobileAlreadyExists Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''CustomerError

instance IsBaseError CustomerError where
  toMessage (PersonMobileAlreadyExists phoneNo) = Just $ "Mobile number " <> phoneNo <> " already exists with another user."

instance IsHTTPError CustomerError where
  toErrorCode = \case
    PersonMobileAlreadyExists _ -> "PERSON_MOBILE_ALREADY_EXISTS"
  toHttpCode = \case
    PersonMobileAlreadyExists _ -> E400

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

data RiderError
  = RiderConfigNotFound Text
  | RiderConfigDoesNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RiderError

instance IsBaseError RiderError where
  toMessage (RiderConfigNotFound merchantOperatingCityId) = Just $ "Rider with merchantOperatingCityId \"" <> show merchantOperatingCityId <> "\" not found."
  toMessage (RiderConfigDoesNotExist merchantOperatingCityId) = Just $ "Rider with merchantOperatingCityId \"" <> show merchantOperatingCityId <> "\" does not exist."

instance IsHTTPError RiderError where
  toErrorCode = \case
    RiderConfigNotFound _ -> "RIDER_NOT_FOUND"
    RiderConfigDoesNotExist _ -> "RIDER_NOT_EXISTS"
  toHttpCode = \case
    RiderConfigNotFound _ -> E500
    RiderConfigDoesNotExist _ -> E400

instance IsAPIError RiderError

data LocationMappingError
  = FromLocationMappingNotFound Text
  | FromLocationNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''LocationMappingError

instance IsBaseError LocationMappingError where
  toMessage = \case
    FromLocationMappingNotFound id_ -> Just $ "From location mapping not found for entity id: " <> id_ <> "."
    FromLocationNotFound id_ -> Just $ "From location not found for locationId: " <> id_ <> "."

instance IsHTTPError LocationMappingError where
  toErrorCode = \case
    FromLocationMappingNotFound _ -> "FROM_LOCATION_MAPPING_NOT_FOUND"
    FromLocationNotFound _ -> "FROM_LOCATION_NOT_FOUND"

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

data StationError
  = StationNotFound Text
  | StationDoesNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''StationError

instance IsBaseError StationError where
  toMessage = \case
    StationNotFound msg -> Just $ "Station Not Found:-" <> msg
    StationDoesNotExist msg -> Just $ "Station Does Not Exist:-" <> msg

instance IsHTTPError StationError where
  toErrorCode = \case
    StationNotFound _ -> "STATION_NOT_FOUND"
    StationDoesNotExist _ -> "STATION_DOES_NOT_EXIST"

  toHttpCode = \case
    StationNotFound _ -> E500
    StationDoesNotExist _ -> E400

instance IsAPIError StationError

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
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FRFSTicketBookingError

instance IsBaseError FRFSTicketBookingError where
  toMessage = \case
    FRFSTicketBookingNotFound bookingId -> Just $ "FRFS Ticket Booking with bookingId:" +|| bookingId ||+ " not found."
    FRFSTicketBookingDoesNotExist bookingId -> Just $ "FRFS Ticket Booking with bookingId:" +|| bookingId ||+ " does not exist."
    FRFSTicketsForBookingExpired bookingId -> Just $ "FRFS Tickets for booking with bookingId:" +|| bookingId ||+ " has expired."
    FRFSTicketsForBookingDoesNotExist bookingId -> Just $ "FRFS Tickets for booking with bookingId:" +|| bookingId ||+ " does not exist."

instance IsHTTPError FRFSTicketBookingError where
  toErrorCode = \case
    FRFSTicketBookingNotFound _ -> "FRFS_TICKET_BOOKING_NOT_FOUND"
    FRFSTicketBookingDoesNotExist _ -> "FRFS_TICKET_BOOKING_DOES_NOT_EXIST"
    FRFSTicketsForBookingExpired _ -> "FRFS_TICKETS_FOR_BOOKING_EXPIRED"
    FRFSTicketsForBookingDoesNotExist _ -> "FRFS_TICKETS_FOR_BOOKING_DOES_NOT_EXIST"

  toHttpCode = \case
    FRFSTicketBookingNotFound _ -> E500
    FRFSTicketBookingDoesNotExist _ -> E400
    FRFSTicketsForBookingExpired _ -> E400
    FRFSTicketsForBookingDoesNotExist _ -> E400

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
    CustomerPaymentIdNotFound cusomterId -> Just $ "Customer payment id with id \"" <> show cusomterId <> "\" not found."
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
