{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Error (module Tools.Error) where

import EulerHS.Prelude
import Kernel.Types.Error as Tools.Error hiding (PersonError, RideError)
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Utils.Common (Meters)

data RatingError
  = InvalidRatingValue
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RatingError

instance IsBaseError RatingError

instance IsHTTPError RatingError where
  toErrorCode InvalidRatingValue = "INVALID_RATING_VALUE"
  toHttpCode InvalidRatingValue = E400

instance IsAPIError RatingError

data RideError
  = InvalidRideRequest
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RideError

instance IsBaseError RideError

instance IsHTTPError RideError where
  toErrorCode InvalidRideRequest = "INVALID_RIDE_START_REQUEST"
  toHttpCode InvalidRideRequest = E400 -- should we keep it 500?

instance IsAPIError RideError

data FarePolicyError
  = NoFarePolicy
  | NoPerExtraKmRate
  | CantCalculateDistance
  | AverageVehicleSpeedLessThanZero
  deriving (Generic, Eq, Show, FromJSON, ToJSON, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FarePolicyError

instance IsBaseError FarePolicyError where
  toMessage NoFarePolicy = Just "No fare policy matches passed data."
  toMessage _ = Nothing

instance IsHTTPError FarePolicyError where
  toErrorCode NoFarePolicy = "NO_FARE_POLICY"
  toErrorCode NoPerExtraKmRate = "NO_PER_EXTRA_KM_RATE"
  toErrorCode CantCalculateDistance = "CANT_CALCULATE_DISTANCE"
  toErrorCode AverageVehicleSpeedLessThanZero = "AVERAGE_SPEED_OF_VEHICLE LESS THAN ZERO"
  toHttpCode _ = E500

instance IsAPIError FarePolicyError

data RentalFarePolicyError
  = NoRentalFarePolicy
  deriving (Generic, Eq, Show, FromJSON, ToJSON, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RentalFarePolicyError

instance IsBaseError RentalFarePolicyError where
  toMessage NoRentalFarePolicy = Just "No rental fare policy matches passed data."

instance IsHTTPError RentalFarePolicyError where
  toErrorCode NoRentalFarePolicy = "NO_RENTAL_FARE_POLICY"
  toHttpCode _ = E500

instance IsAPIError RentalFarePolicyError

data FPDiscountError
  = FPDiscountDoesNotExist
  | FPDiscountAlreadyEnabled
  deriving (Generic, Eq, Show, FromJSON, ToJSON, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FPDiscountError

instance IsBaseError FPDiscountError where
  toMessage = \case
    FPDiscountDoesNotExist -> Just "No discount matches passed data."
    FPDiscountAlreadyEnabled -> Just "Some discount is already enabled."

instance IsHTTPError FPDiscountError where
  toErrorCode = \case
    FPDiscountDoesNotExist -> "FARE_POLICY_DISCOUNT_DOES_NOT_EXIST"
    FPDiscountAlreadyEnabled -> "FARE_POLICY_DISCOUNT_ALREADY_ENABLED"
  toHttpCode = \case
    FPDiscountDoesNotExist -> E400
    FPDiscountAlreadyEnabled -> E400

instance IsAPIError FPDiscountError

data FareProductError
  = NoFareProduct
  deriving (Generic, Eq, Show, FromJSON, ToJSON, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FareProductError

instance IsBaseError FareProductError where
  toMessage NoFareProduct = Just "No fare product matches passed data."

instance IsHTTPError FareProductError where
  toErrorCode NoFareProduct = "NO_FARE_PRODUCT"
  toHttpCode NoFareProduct = E500

instance IsAPIError FareProductError

data AllocationError
  = EmptyDriverPool
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''AllocationError

instance IsBaseError AllocationError

instance IsHTTPError AllocationError where
  toErrorCode EmptyDriverPool = "EMPTY_DRIVER_POOL"

instance IsAPIError AllocationError

data DriverInformationError
  = DriverInfoNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DriverInformationError

instance IsBaseError DriverInformationError

instance IsHTTPError DriverInformationError where
  toErrorCode DriverInfoNotFound = "DRIVER_INFORMATON_NOT_FOUND"

instance IsAPIError DriverInformationError

data ProductsError
  = ProductsNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''ProductsError

instance IsBaseError ProductsError

instance IsHTTPError ProductsError where
  toErrorCode = \case
    ProductsNotFound -> "PRODUCTS_NOT_FOUND"
  toHttpCode = \case
    ProductsNotFound -> E500

instance IsAPIError ProductsError

newtype ShardMappingError = ShardMappingError Text
  deriving (Show, Typeable)

instance IsBaseError ShardMappingError where
  toMessage (ShardMappingError msg) = Just msg

instanceExceptionWithParent 'BaseException ''ShardMappingError

data DriverError
  = DriverAccountDisabled
  | DriverWithoutVehicle Text
  | NoPlanSelected Text
  | DriverAccountBlocked
  | DriverAccountAlreadyBlocked
  | DriverUnsubscribed
  | DriverNotFound Text
  | DriverReferralCodeNotGenerated
  | DriverAlreadyLinkedWithVehicle Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DriverError

instance IsBaseError DriverError where
  toMessage DriverAccountDisabled = Just "Driver account has been disabled. He can't go online and receive ride offers in this state."
  toMessage (DriverWithoutVehicle personId) = Just $ "Driver with id = " <> personId <> " has no linked vehicle"
  toMessage (NoPlanSelected personId) = Just $ "Driver with id = " <> personId <> " has not selected any plan"
  toMessage DriverAccountBlocked = Just "Driver account has been blocked."
  toMessage DriverAccountAlreadyBlocked = Just "Driver account has been already blocked."
  toMessage DriverUnsubscribed = Just "Driver has been unsubscibed from platform. Pay pending amount to subscribe back."
  toMessage (DriverNotFound phoneNo) = Just $ "No Driver is found Registered  with this phone number = " <> phoneNo
  toMessage DriverReferralCodeNotGenerated = Just "Not able to generate driver referral code"
  toMessage (DriverAlreadyLinkedWithVehicle vehicleNo) = Just $ "Driver is already linked with vehicle " <> vehicleNo

instance IsHTTPError DriverError where
  toErrorCode = \case
    DriverAccountDisabled -> "DRIVER_ACCOUNT_DISABLED"
    DriverWithoutVehicle _ -> "DRIVER_WITHOUT_VEHICLE"
    NoPlanSelected _ -> "NO_PLAN_SELECTED"
    DriverAccountBlocked -> "DRIVER_ACCOUNT_BLOCKED"
    DriverAccountAlreadyBlocked -> "DRIVER_ACCOUNT_ALREADY_BLOCKED"
    DriverUnsubscribed -> "DRIVER_UNSUBSCRIBED"
    DriverNotFound _ -> "DRIVER_NOT_FOUND"
    DriverReferralCodeNotGenerated -> "DRIVER_REFERRAL_CODE_NOT_GENERATED"
    DriverAlreadyLinkedWithVehicle _ -> "DRIVER_ALREADY_LINKED"
  toHttpCode = \case
    DriverAccountDisabled -> E403
    DriverWithoutVehicle _ -> E400
    NoPlanSelected _ -> E400
    DriverAccountBlocked -> E403
    DriverAccountAlreadyBlocked -> E403
    DriverUnsubscribed -> E403
    DriverNotFound _ -> E403
    DriverReferralCodeNotGenerated -> E400
    DriverAlreadyLinkedWithVehicle _ -> E403

instance IsAPIError DriverError

data AadhaarError
  = AadhaarAlreadyVerified
  | TransactionIdNotFound
  | AadhaarAlreadyLinked
  | AadhaarDataAlreadyPresent
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''AadhaarError

instance IsBaseError AadhaarError where
  toMessage AadhaarAlreadyVerified = Just " Driver aadhar is already verified."
  toMessage TransactionIdNotFound = Just " transaction id not found for this verification"
  toMessage AadhaarAlreadyLinked = Just "aadhaar number is already linked"
  toMessage AadhaarDataAlreadyPresent = Just "aadhaar data is already present for this driver"

instance IsHTTPError AadhaarError where
  toErrorCode = \case
    AadhaarAlreadyVerified -> "AADHAAR_ALREADY_VERIFIED"
    TransactionIdNotFound -> "TRANSACTION_ID_NOT_FOUND"
    AadhaarAlreadyLinked -> "AADHAAR_ALREADY_LINKED"
    AadhaarDataAlreadyPresent -> "AADHAAR_DATA_ALREADY_PRESENT"
  toHttpCode = \case
    AadhaarAlreadyVerified -> E400
    TransactionIdNotFound -> E400
    AadhaarAlreadyLinked -> E400
    AadhaarDataAlreadyPresent -> E400

instance IsAPIError AadhaarError

--
newtype OfferError
  = NotAllowedExtraFee Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''OfferError

instance IsBaseError OfferError where
  toMessage (NotAllowedExtraFee x) = Just $ "Not allowed extra fee: " <> x

instance IsHTTPError OfferError where
  toErrorCode = \case
    NotAllowedExtraFee {} -> "EXTRA_FEE_NOT_ALLOWED"
  toHttpCode = \case
    NotAllowedExtraFee {} -> E400

instance IsAPIError OfferError

newtype SearchRequestErrorARDU
  = SearchRequestNotRelevant Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SearchRequestErrorARDU

instance IsBaseError SearchRequestErrorARDU where
  toMessage (SearchRequestNotRelevant _) = Just "Search request no longer relevant"

instance IsHTTPError SearchRequestErrorARDU where
  toErrorCode = \case
    SearchRequestNotRelevant _ -> "SEARCH_REQUEST_NOT_RELEVANT"
  toHttpCode = \case
    SearchRequestNotRelevant _ -> E400

instance IsAPIError SearchRequestErrorARDU

--
data DriverQuoteError
  = FoundActiveQuotes
  | DriverOnRide
  | DriverQuoteExpired
  | NoSearchRequestForDriver
  | RideRequestAlreadyAccepted
  | QuoteAlreadyRejected
  | UnexpectedResponseValue
  | NoActiveRidePresent
  | RecentActiveRide
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DriverQuoteError

instance IsBaseError DriverQuoteError where
  toMessage FoundActiveQuotes = Just "Failed to offer quote, there are other active quotes from this driver"
  toMessage DriverOnRide = Just "Unable to offer a quote while being on ride"
  toMessage DriverQuoteExpired = Just "Driver quote expired"
  toMessage NoSearchRequestForDriver = Just "No search request for this driver"
  toMessage RideRequestAlreadyAccepted = Just "Ride request already accepted by other driver"
  toMessage QuoteAlreadyRejected = Just "Quote Already Rejected"
  toMessage UnexpectedResponseValue = Just "The response type is unexpected"
  toMessage NoActiveRidePresent = Just "No active ride is present for this driver registered with given vehicle Number"
  toMessage RecentActiveRide = Just "Cannot End Ride before 60 seconds"

instance IsHTTPError DriverQuoteError where
  toErrorCode = \case
    FoundActiveQuotes -> "FOUND_ACTIVE_QUOTES"
    DriverOnRide -> "DRIVER_ON_RIDE"
    DriverQuoteExpired -> "QUOTE_EXPIRED"
    NoSearchRequestForDriver -> "NO_SEARCH_REQUEST_FOR_DRIVER"
    RideRequestAlreadyAccepted -> "RIDE_REQUEST_ALREADY_ACCEPTED"
    QuoteAlreadyRejected -> "QUOTE_ALREADY_REJECTED"
    UnexpectedResponseValue -> "UNEXPECTED_RESPONSE_VALUE"
    NoActiveRidePresent -> "NO_ACTIVE_RIDE_PRESENT"
    RecentActiveRide -> "RECENT_ACTIVE_RIDE"

  toHttpCode = \case
    FoundActiveQuotes -> E400
    DriverOnRide -> E400
    DriverQuoteExpired -> E400
    NoSearchRequestForDriver -> E400
    RideRequestAlreadyAccepted -> E400
    QuoteAlreadyRejected -> E400
    UnexpectedResponseValue -> E400
    NoActiveRidePresent -> E400
    RecentActiveRide -> E400

instance IsAPIError DriverQuoteError

data FareParametersError
  = FareParametersNotFound Text
  | FareParametersDoNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FareParametersError

instance IsHTTPError FareParametersError where
  toErrorCode = \case
    FareParametersNotFound _ -> "FARE_PARAMETERS_NOT_FOUND"
    FareParametersDoNotExist _ -> "FARE_PARAMETERS_DO_NOT_EXIST"
  toHttpCode = \case
    FareParametersNotFound _ -> E500
    FareParametersDoNotExist _ -> E400

instance IsAPIError FareParametersError

instance IsBaseError FareParametersError where
  toMessage = \case
    FareParametersNotFound fareParamsId -> Just $ "FareParameters with fareParametersId \"" <> show fareParamsId <> "\" not found."
    FareParametersDoNotExist rideId -> Just $ "FareParameters for ride \"" <> show rideId <> "\" do not exist."

data OnboardingDocumentConfigError
  = OnboardingDocumentConfigNotFound Text Text
  | OnboardingDocumentConfigDoesNotExist Text Text
  | OnboardingDocumentConfigAlreadyExists Text Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''OnboardingDocumentConfigError

instance IsBaseError OnboardingDocumentConfigError where
  toMessage (OnboardingDocumentConfigNotFound merchantId doctype) =
    Just $
      "OnboardingDocumentConfig with merchantId \"" <> show merchantId <> "\" and docType \"" <> show doctype <> "\" not found."
  toMessage (OnboardingDocumentConfigDoesNotExist merchantId doctype) =
    Just $
      "OnboardingDocumentConfig with merchantId \"" <> show merchantId <> "\" and docType \"" <> show doctype <> "\" does not exist."
  toMessage (OnboardingDocumentConfigAlreadyExists merchantId doctype) =
    Just $
      "OnboardingDocumentConfig with merchantId \"" <> show merchantId <> "\" and docType \"" <> show doctype <> "\" already exists."

instance IsHTTPError OnboardingDocumentConfigError where
  toErrorCode = \case
    OnboardingDocumentConfigNotFound {} -> "ONBOARDING_DOCUMENT_CONFIG_NOT_FOUND"
    OnboardingDocumentConfigDoesNotExist {} -> "ONBOARDING_DOCUMENT_CONFIG_DOES_NOT_EXIST"
    OnboardingDocumentConfigAlreadyExists {} -> "ONBOARDING_DOCUMENT_CONFIG_ALREADY_EXISTS"
  toHttpCode = \case
    OnboardingDocumentConfigNotFound {} -> E500
    OnboardingDocumentConfigDoesNotExist {} -> E400
    OnboardingDocumentConfigAlreadyExists {} -> E400

instance IsAPIError OnboardingDocumentConfigError

newtype IssueReportError
  = IssueReportDoNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''IssueReportError

instance IsBaseError IssueReportError where
  toMessage = \case
    IssueReportDoNotExist issueReportId -> Just $ "IssueReport with issueReportId \"" <> show issueReportId <> "\" do not exist."

instance IsHTTPError IssueReportError where
  toErrorCode (IssueReportDoNotExist _) = "ISSUE_REPORT_DO_NOT_EXIST"
  toHttpCode (IssueReportDoNotExist _) = E400

instance IsAPIError IssueReportError

data IssueOptionError
  = IssueOptionNotFound Text
  | IssueOptionDoNotExist Text
  | IssueOptionInvalid Text Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''IssueOptionError

instance IsBaseError IssueOptionError where
  toMessage = \case
    IssueOptionNotFound issueOptionId -> Just $ "IssueOption with issueOptionId \"" <> show issueOptionId <> "\" not found."
    IssueOptionDoNotExist issueOptionId -> Just $ "IssueOption with issueOptionId \"" <> show issueOptionId <> "\" do not exist."
    IssueOptionInvalid issueOptionId issueCategoryId -> Just $ "IssueOption with issueOptionId \"" <> show issueOptionId <> "\" not linked to IssueCategory with issueCategoryId \"" <> show issueCategoryId <> "\"."

instance IsHTTPError IssueOptionError where
  toErrorCode = \case
    IssueOptionNotFound _ -> "ISSUE_OPTION_NOT_FOUND"
    IssueOptionDoNotExist _ -> "ISSUE_OPTION_DO_NOT_EXIST"
    IssueOptionInvalid _ _ -> "ISSUE_OPTION_INVALID"

  toHttpCode = \case
    IssueOptionNotFound _ -> E500
    IssueOptionDoNotExist _ -> E400
    IssueOptionInvalid _ _ -> E400

instance IsAPIError IssueOptionError

data IssueCategoryError
  = IssueCategoryNotFound Text
  | IssueCategoryDoNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''IssueCategoryError

instance IsBaseError IssueCategoryError where
  toMessage = \case
    IssueCategoryNotFound issueCategoryId -> Just $ "IssueCategory with issueCategoryId \"" <> show issueCategoryId <> "\" not found."
    IssueCategoryDoNotExist issueCategoryId -> Just $ "IssueCategory with issueCategoryId \"" <> show issueCategoryId <> "\" do not exist."

instance IsHTTPError IssueCategoryError where
  toErrorCode = \case
    IssueCategoryNotFound _ -> "ISSUE_CATEGORY_NOT_FOUND"
    IssueCategoryDoNotExist _ -> "ISSUE_CATEGORY_DO_NOT_EXIST"
  toHttpCode = \case
    IssueCategoryNotFound _ -> E500
    IssueCategoryDoNotExist _ -> E400

instance IsAPIError IssueCategoryError

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
    FileSizeExceededError _ -> E400
    FileDoNotExist _ -> E400
    FileFormatNotSupported _ -> E400

instance IsAPIError MediaFileError

instance IsBaseError MediaFileError where
  toMessage = \case
    FileSizeExceededError fileSize -> Just $ "Filesize is " <> fileSize <> " Bytes, which is more than the allowed 10MB limit."
    FileDoNotExist fileId -> Just $ "MediaFile with fileId \"" <> show fileId <> "\" do not exist."
    FileFormatNotSupported fileFormat -> Just $ "MediaFile with fileFormat \"" <> show fileFormat <> "\" not supported."

newtype DriverIntelligentPoolConfigError
  = DriverIntelligentPoolConfigNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DriverIntelligentPoolConfigError

instance IsBaseError DriverIntelligentPoolConfigError where
  toMessage (DriverIntelligentPoolConfigNotFound merchantId) =
    Just $
      "DriverIntelligentPoolConfig with merchantId \"" <> show merchantId <> "\" not found."

instance IsHTTPError DriverIntelligentPoolConfigError where
  toErrorCode = \case
    DriverIntelligentPoolConfigNotFound {} -> "DRIVER_INTELLIGENT_POOL_CONFIG_NOT_FOUND"
  toHttpCode = \case
    DriverIntelligentPoolConfigNotFound {} -> E500

instance IsAPIError DriverIntelligentPoolConfigError

data DriverPoolConfigError
  = DriverPoolConfigDoesNotExist Text Meters
  | DriverPoolConfigAlreadyExists Text Meters
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DriverPoolConfigError

instance IsBaseError DriverPoolConfigError where
  toMessage (DriverPoolConfigDoesNotExist merchantId tripDistance) =
    Just $
      "DriverPoolConfig with merchantId \"" <> show merchantId <> "\" and tripDistance " <> show tripDistance <> " does not exist."
  toMessage (DriverPoolConfigAlreadyExists merchantId tripDistance) =
    Just $
      "DriverPoolConfig with merchantId \"" <> show merchantId <> "\" and tripDistance " <> show tripDistance <> " already exists."

instance IsHTTPError DriverPoolConfigError where
  toErrorCode = \case
    DriverPoolConfigDoesNotExist {} -> "DRIVER_POOL_CONFIG_DOES_NOT_EXIST"
    DriverPoolConfigAlreadyExists {} -> "DRIVER_POOL_CONFIG_ALREADY_EXISTS"
  toHttpCode = \case
    DriverPoolConfigDoesNotExist {} -> E400
    DriverPoolConfigAlreadyExists {} -> E400

instance IsAPIError DriverPoolConfigError

newtype VehicleRegCertError = VehicleRegCertNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''VehicleRegCertError

instance IsBaseError VehicleRegCertError where
  toMessage = \case
    VehicleRegCertNotFound vehicleNo -> Just $ "Fleet-driver association with vehicle no. \"" <> show vehicleNo <> "\"not found. "

instance IsHTTPError VehicleRegCertError where
  toErrorCode = \case
    VehicleRegCertNotFound _ -> "VEHICLE_REG_CERT_NOT_FOUND"
  toHttpCode = \case
    VehicleRegCertNotFound _ -> E400

instance IsAPIError VehicleRegCertError

data SearchTryError
  = SearchTryNotFound Text
  | SearchTryDoesNotExist Text
  | SearchTryExpired
  | SearchTryCancelled Text
  | SearchTryEstimatedFareChanged
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SearchTryError

instance IsBaseError SearchTryError where
  toMessage = \case
    SearchTryNotFound searchTryId -> Just $ "Search try with searchTryId \"" <> show searchTryId <> "\"not found. "
    SearchTryDoesNotExist searchTryId -> Just $ "No search try matches passed data \"<>" <> show searchTryId <> "\"."
    SearchTryCancelled searchTryId -> Just $ "Search try with searchTryId \"<>" <> show searchTryId <> "\" was cancelled."
    SearchTryEstimatedFareChanged -> Just "Search try estimated fare changed."
    _ -> Nothing

instance IsHTTPError SearchTryError where
  toErrorCode = \case
    SearchTryNotFound _ -> "SEARCH_TRY_NOT_FOUND"
    SearchTryDoesNotExist _ -> "SEARCH_TRY_DOES_NOT_EXIST"
    SearchTryExpired -> "SEARCH_TRY_EXPIRED"
    SearchTryCancelled _ -> "SEARCH_TRY_CANCELLED"
    SearchTryEstimatedFareChanged -> "SEARCH_TRY_ESTIMATED_FARE_CHANGED"
  toHttpCode = \case
    SearchTryNotFound _ -> E500
    SearchTryDoesNotExist _ -> E400
    SearchTryExpired -> E400
    SearchTryCancelled _ -> E403
    SearchTryEstimatedFareChanged -> E503

instance IsAPIError SearchTryError

data EstimateError
  = EstimateNotFound Text
  | EstimateDoesNotExist Text
  | EstimateCancelled Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''EstimateError

instance IsBaseError EstimateError where
  toMessage = \case
    EstimateNotFound estimateId -> Just $ "Estimate with estimateId \"" <> show estimateId <> "\"not found. "
    EstimateDoesNotExist estimateId -> Just $ "No estimate matches passed data \"<>" <> show estimateId <> "\"."
    EstimateCancelled estimateId -> Just $ "Estimate with estimateId \"<>" <> show estimateId <> "\" was cancelled. "

instance IsHTTPError EstimateError where
  toErrorCode = \case
    EstimateNotFound _ -> "ESTIMATE_NOT_FOUND"
    EstimateDoesNotExist _ -> "ESTIMATE_DOES_NOT_EXIST"
    EstimateCancelled _ -> "ESTIMATE_CANCELLED"
  toHttpCode = \case
    EstimateNotFound _ -> E500
    EstimateDoesNotExist _ -> E400
    EstimateCancelled _ -> E403

instance IsAPIError EstimateError

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

data DriverHomeLocationError
  = DriverHomeLocationNotFound Text
  | DriverHomeLocationDoesNotExist Text
  | DriverHomeLocationLimitReached
  | DriverHomeLocationUpdateWhileActiveError
  | DriverHomeLocationDeleteWhileActiveError
  | DriverHomeLocationUpdateBeforeTime
  | DriverHomeLocationOutsideServiceArea
  | NewLocationTooCloseToPreviousHomeLocation
  | DriverHomeLocationDoesNotBelongToDriver
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DriverHomeLocationError

instance IsBaseError DriverHomeLocationError where
  toMessage = \case
    DriverHomeLocationNotFound driverHomeLocationId -> Just $ "Driver home location with id \"" <> show driverHomeLocationId <> "\" not found."
    DriverHomeLocationDoesNotExist driverHomeLocationId -> Just $ "No driver home location matches passed data \"<>" <> show driverHomeLocationId <> "\"."
    DriverHomeLocationLimitReached -> Just "Driver home location limit already reached."
    DriverHomeLocationUpdateWhileActiveError -> Just "Cannot Update Driver Home Location while Go To feature is active."
    DriverHomeLocationDeleteWhileActiveError -> Just "Cannot Delete Driver Home Location while Go To feature is active."
    DriverHomeLocationUpdateBeforeTime -> Just "Driver trying to update home location before time."
    DriverHomeLocationOutsideServiceArea -> Just "Driver home location outside service area."
    NewLocationTooCloseToPreviousHomeLocation -> Just "New location too close to a previously added home location."
    DriverHomeLocationDoesNotBelongToDriver -> Just "Driver home location does not belong to this driver."

instance IsHTTPError DriverHomeLocationError where
  toErrorCode = \case
    DriverHomeLocationNotFound _ -> "DRIVER_HOME_LOCATION_NOT_FOUND"
    DriverHomeLocationDoesNotExist _ -> "DRIVER_HOME_LOCATION_DOES_NOT_EXIST"
    DriverHomeLocationLimitReached -> "DRIVER_HOME_LOCATION_LIMIT_REACHED"
    DriverHomeLocationUpdateWhileActiveError -> "DRIVER_HOME_LOCATION_UPDATE_WHILE_ACTIVE_ERROR"
    DriverHomeLocationDeleteWhileActiveError -> "DRIVER_HOME_LOCATION_DELETE_WHILE_ACTIVE_ERROR"
    DriverHomeLocationUpdateBeforeTime -> "DRIVER_HOME_LOCATION_UPDATE_BEFORE_TIME"
    DriverHomeLocationOutsideServiceArea -> "DRIVER_HOME_LOCATION_OUTSIDE_SERVICE_AREA"
    NewLocationTooCloseToPreviousHomeLocation -> "NEW_LOCATION_TOO_CLOSE_TO_PREVIOUS_HOME_LOCATION"
    DriverHomeLocationDoesNotBelongToDriver -> "DRIVER_HOME_LOCATION_DOES_NOT_BELONG_TO_DRIVER"
  toHttpCode = \case
    DriverHomeLocationNotFound _ -> E500
    DriverHomeLocationDoesNotExist _ -> E400
    DriverHomeLocationLimitReached -> E400
    DriverHomeLocationUpdateWhileActiveError -> E400
    DriverHomeLocationDeleteWhileActiveError -> E400
    DriverHomeLocationUpdateBeforeTime -> E400
    DriverHomeLocationOutsideServiceArea -> E400
    NewLocationTooCloseToPreviousHomeLocation -> E400
    DriverHomeLocationDoesNotBelongToDriver -> E400

instance IsAPIError DriverHomeLocationError

data DriverGoHomeRequestError
  = DriverGoHomeRequestErrorNotFound Text
  | DriverGoHomeRequestErrorDoesNotExist Text
  | DriverGoHomeRequestDailyUsageLimitReached
  | DriverGoHomeRequestAlreadyActive
  | DriverGoHomeRequestNotPresent
  | GoHomeFeaturePermanentlyDisabled
  | DriverCloseToHomeLocation
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DriverGoHomeRequestError

instance IsBaseError DriverGoHomeRequestError where
  toMessage = \case
    DriverGoHomeRequestErrorNotFound goHomeReqId -> Just $ "Driver GoHome request with id \"" <> show goHomeReqId <> "\" not found."
    DriverGoHomeRequestErrorDoesNotExist goHomeReqId -> Just $ "No driver GoHome request matches passed data \"<>" <> show goHomeReqId <> "\"."
    DriverGoHomeRequestDailyUsageLimitReached -> Just "GoHome feature daily usage limit reached."
    DriverGoHomeRequestAlreadyActive -> Just "GoHome feature is already active."
    DriverGoHomeRequestNotPresent -> Just "GoHome feature is not activated"
    GoHomeFeaturePermanentlyDisabled -> Just "GoHome feature is permanently disabled."
    DriverCloseToHomeLocation -> Just "Driver is close to home location."

instance IsHTTPError DriverGoHomeRequestError where
  toErrorCode = \case
    DriverGoHomeRequestErrorNotFound _ -> "DRIVER_GO_HOME_REQUEST_NOT_FOUND"
    DriverGoHomeRequestErrorDoesNotExist _ -> "DRIVER_GO_HOME_REQUEST_DOES_NOT_EXIST"
    DriverGoHomeRequestDailyUsageLimitReached -> "DRIVER_GO_HOME_REQUEST_DAILY_USAGE_LIMIT_REACHED"
    DriverGoHomeRequestAlreadyActive -> "DRIVER_GO_HOME_REQUEST_ALREADY_ACTIVE"
    DriverGoHomeRequestNotPresent -> "DRIVER_GO_HOME_REQUEST_NOT_PRESENT"
    GoHomeFeaturePermanentlyDisabled -> "GO_HOME_FEATURE_PERMANENTLY_DISABLED"
    DriverCloseToHomeLocation -> "DRIVER_CLOSE_TO_HOME_LOCATION"
  toHttpCode = \case
    DriverGoHomeRequestErrorNotFound _ -> E500
    DriverGoHomeRequestErrorDoesNotExist _ -> E400
    DriverGoHomeRequestDailyUsageLimitReached -> E400
    DriverGoHomeRequestAlreadyActive -> E400
    DriverGoHomeRequestNotPresent -> E400
    GoHomeFeaturePermanentlyDisabled -> E400
    DriverCloseToHomeLocation -> E400

instance IsAPIError DriverGoHomeRequestError

data SubscriptionError
  = PlanNotFound Text
  | MandateNotFound Text
  | ActiveMandateExists Text
  | ActiveMandateDoNotExist Text
  | InActiveMandateDoNotExist Text
  | InvalidPaymentMode
  | OngoingManualPayment
  | NoCurrentPlanForDriver Text
  | NoDriverPlanForMandate Text
  | InvalidAutoPayStatus
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SubscriptionError

instance IsBaseError SubscriptionError where
  toMessage = \case
    PlanNotFound planId -> Just $ "Plan with planId \"" <> show planId <> "\"not found."
    MandateNotFound mandateId -> Just $ "Mandate with mandateId \"" <> show mandateId <> "\"not found."
    ActiveMandateExists driverId -> Just $ "mandate already exists for driverId \"" <> show driverId <> "\""
    ActiveMandateDoNotExist driverId -> Just $ "no mandate exist for driverId \"" <> show driverId <> "\""
    InActiveMandateDoNotExist driverId -> Just $ "no inactive mandate exist for driverId \"" <> show driverId <> "\""
    NoCurrentPlanForDriver driverId -> Just $ "No plan exists for driverId \"" <> show driverId <> "\""
    NoDriverPlanForMandate mandateId -> Just $ "No plan exists for mandateId \"" <> show mandateId <> "\""
    InvalidPaymentMode -> Just "Invalid payment method"
    InvalidAutoPayStatus -> Just "Invalid auto pay status"
    OngoingManualPayment -> Just "There is ongoing manual payment pls wait"

instance IsHTTPError SubscriptionError where
  toErrorCode = \case
    PlanNotFound _ -> "PLAN_NOT_FOUND"
    MandateNotFound _ -> "MANDATE_NOT_FOUND"
    ActiveMandateExists _ -> "ACTIVE_MANDATE_EXISTS"
    ActiveMandateDoNotExist _ -> "NO_ACTIVE_MANDATE_EXIST"
    InActiveMandateDoNotExist _ -> "NO_INACTIVE_MANDATE_EXIST"
    NoCurrentPlanForDriver _ -> "NO_PLAN_FOR_DRIVER"
    NoDriverPlanForMandate _ -> "NO_DRIVER_PLAN_FOR_MANDATE"
    InvalidPaymentMode -> "INVALID_PAYMENT_MODE"
    InvalidAutoPayStatus -> "INVALID_AUTO_PAY_STATUS"
    OngoingManualPayment -> "ONGOING_PAYMENT_EXECUTION"
  toHttpCode = \case
    PlanNotFound _ -> E500
    MandateNotFound _ -> E500
    ActiveMandateExists _ -> E400
    ActiveMandateDoNotExist _ -> E400
    InActiveMandateDoNotExist _ -> E400
    InvalidPaymentMode -> E400
    InvalidAutoPayStatus -> E400
    NoCurrentPlanForDriver _ -> E500
    NoDriverPlanForMandate _ -> E500
    OngoingManualPayment -> E400

instance IsAPIError SubscriptionError

data FleetErrors
  = FleetOwnerVehicleMismatchError Text
  | VehicleBelongsToAnotherFleet
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FleetErrors

instance IsBaseError FleetErrors where
  toMessage = \case
    FleetOwnerVehicleMismatchError fleetOwnerId -> Just $ "Vehicle does not belong to fleet owner with id " <> show fleetOwnerId
    VehicleBelongsToAnotherFleet -> Just "Vehicle is already part of another fleet"

instance IsHTTPError FleetErrors where
  toErrorCode = \case
    FleetOwnerVehicleMismatchError _ -> "FLEET_OWNER_AND_VEHICLE_MISMATCH"
    VehicleBelongsToAnotherFleet -> "FLEET_OWNER_AND_VEHICLE_MISMATCH"
  toHttpCode = \case
    FleetOwnerVehicleMismatchError _ -> E400
    VehicleBelongsToAnotherFleet -> E400

instance IsAPIError FleetErrors

data OverlayError
  = OverlayKeyAndUdfNotFound Text
  | OverlayKeyAndUdfAlreadyPresent Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''OverlayError

instance IsBaseError OverlayError where
  toMessage = \case
    OverlayKeyAndUdfNotFound overlayKeyAndUdf -> Just $ "Overlay Key and Udf \"" <> show overlayKeyAndUdf <> "\" not found. "
    OverlayKeyAndUdfAlreadyPresent overlayKeyAndUdf -> Just $ "Overlay Key and Udf \"" <> show overlayKeyAndUdf <> "\" already present."

instance IsHTTPError OverlayError where
  toErrorCode = \case
    OverlayKeyAndUdfNotFound _ -> "OVERLAY_KEY_AND_UDF_NOT_FOUND"
    OverlayKeyAndUdfAlreadyPresent _ -> "OVERLAY_KEY_AND_UDF_ALREADY_PRESENT"
  toHttpCode = \case
    OverlayKeyAndUdfNotFound _ -> E400
    OverlayKeyAndUdfAlreadyPresent _ -> E400

instance IsAPIError OverlayError
