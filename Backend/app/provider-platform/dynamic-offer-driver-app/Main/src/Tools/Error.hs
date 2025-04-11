{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Error (module Tools.Error) where

import Data.Aeson (Value (Null), object, (.=))
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Error as Tools.Error hiding (PersonError)
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Utils.Common (Meters)
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data RatingError
  = InvalidRatingValue
  deriving (Eq, Show, IsBecknAPIError)

instance IsBaseError RatingError

instance IsHTTPError RatingError where
  toErrorCode InvalidRatingValue = "INVALID_RATING_VALUE"
  toHttpCode InvalidRatingValue = E400

instance IsAPIError RatingError

instanceExceptionWithParent 'HTTPException ''RatingError

data LocationServiceabilityError
  = LocationUnserviceable
  deriving (Eq, Show, IsBecknAPIError)

instance IsBaseError LocationServiceabilityError

instance IsHTTPError LocationServiceabilityError where
  toErrorCode LocationUnserviceable = "LOCATION_UNSERVICEABLE"
  toHttpCode LocationUnserviceable = E400

instance IsAPIError LocationServiceabilityError

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

data BlockReasonFlag = CancellationRateWeekly | CancellationRateDaily | CancellationRate | ByDashboard | ExtraFareDaily | ExtraFareWeekly deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''BlockReasonFlag)

data BlockErrorPayload = BlockErrorPayload
  { blockExpiryTime :: Maybe UTCTime,
    blockReason :: Maybe BlockReasonFlag
  }
  deriving (Eq, Show, IsBecknAPIError)

instance ToJSON BlockErrorPayload where
  toJSON (BlockErrorPayload expiryTime reason) =
    object
      [ "blockExpiryTime" .= expiryTime,
        "blockReason" .= reason
      ]

data DriverError
  = DriverAccountDisabled
  | DriverWithoutVehicle Text
  | NoPlanSelected Text
  | DriverAccountBlocked BlockErrorPayload
  | DriverAccountAlreadyBlocked
  | DriverUnsubscribed
  | DriverNotFound Text
  | DriverEmailNotFound Text
  | DriverMobileAlreadyExists Text
  | DriverEmailAlreadyExists Text
  | DriverReferralCodeNotGenerated
  | DriverAlreadyLinkedWithVehicle Text
  | FleetOwnerAccountBlocked
  | AccountBlocked
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DriverError

instance IsBaseError DriverError where
  toMessage DriverAccountDisabled = Just "Driver account has been disabled. He can't go online and receive ride offers in this state."
  toMessage (DriverWithoutVehicle personId) = Just $ "Driver with id = " <> personId <> " has no linked vehicle"
  toMessage (NoPlanSelected personId) = Just $ "Driver with id = " <> personId <> " has not selected any plan"
  toMessage (DriverAccountBlocked _) = Just "Driver account has been blocked."
  toMessage DriverAccountAlreadyBlocked = Just "Driver account has been already blocked."
  toMessage DriverUnsubscribed = Just "Driver has been unsubscibed from platform. Pay pending amount to subscribe back."
  toMessage (DriverNotFound phoneNo) = Just $ "No Driver is found Registered  with this phone number = " <> phoneNo
  toMessage (DriverEmailNotFound personId) = Just $ "Driver email not found driverId = " <> personId
  toMessage (DriverMobileAlreadyExists phoneNo) = Just $ "Mobile number " <> phoneNo <> " already exists with another user."
  toMessage (DriverEmailAlreadyExists email) = Just $ "Email " <> email <> " already exists with another user."
  toMessage DriverReferralCodeNotGenerated = Just "Not able to generate driver referral code"
  toMessage (DriverAlreadyLinkedWithVehicle vehicleNo) = Just $ "Driver is already linked with vehicle " <> vehicleNo
  toMessage FleetOwnerAccountBlocked = Just "Fleet Owner account has been blocked."
  toMessage AccountBlocked = Just "Account has been blocked."

instance IsHTTPError DriverError where
  toErrorCode = \case
    DriverAccountDisabled -> "DRIVER_ACCOUNT_DISABLED"
    DriverWithoutVehicle _ -> "DRIVER_WITHOUT_VEHICLE"
    NoPlanSelected _ -> "NO_PLAN_SELECTED"
    DriverAccountBlocked _ -> "DRIVER_ACCOUNT_BLOCKED"
    DriverAccountAlreadyBlocked -> "DRIVER_ACCOUNT_ALREADY_BLOCKED"
    DriverUnsubscribed -> "DRIVER_UNSUBSCRIBED"
    DriverNotFound _ -> "DRIVER_NOT_FOUND"
    DriverEmailNotFound _ -> "DRIVER_EMAIL_NOT_FOUND"
    DriverReferralCodeNotGenerated -> "DRIVER_REFERRAL_CODE_NOT_GENERATED"
    DriverAlreadyLinkedWithVehicle _ -> "DRIVER_ALREADY_LINKED"
    DriverMobileAlreadyExists _ -> "DRIVER_MOBILE_ALREADY_EXISTS"
    DriverEmailAlreadyExists _ -> "DRIVER_EMAIL_ALREADY_EXISTS"
    FleetOwnerAccountBlocked -> "FLEET_OWNER_ACCOUNT_BLOCKED"
    AccountBlocked -> "ACCOUNT_BLOCKED"
  toHttpCode = \case
    DriverAccountDisabled -> E403
    DriverWithoutVehicle _ -> E400
    NoPlanSelected _ -> E400
    DriverAccountBlocked _ -> E403
    DriverAccountAlreadyBlocked -> E403
    DriverUnsubscribed -> E403
    DriverNotFound _ -> E403
    DriverEmailNotFound _ -> E500
    DriverReferralCodeNotGenerated -> E400
    DriverAlreadyLinkedWithVehicle _ -> E403
    DriverMobileAlreadyExists _ -> E400
    DriverEmailAlreadyExists _ -> E400
    FleetOwnerAccountBlocked -> E403
    AccountBlocked -> E403

instance IsAPIError DriverError where
  toPayload (DriverAccountBlocked errorPayload) = toJSON errorPayload
  toPayload _ = Null

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
  | DriverQuoteNotFound Text
  | DriverQuoteExpired
  | NoSearchRequestForDriver
  | RideRequestAlreadyAccepted
  | RideRequestAlreadyAcceptedOrCancelled Text
  | DriverAlreadyQuoted Text
  | QuoteAlreadyRejected
  | UnexpectedResponseValue
  | NoActiveRidePresent
  | RecentActiveRide
  | DriverTransactionTryAgain (Maybe Text)
  | CustomerDestinationUpdated
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DriverQuoteError

instance IsBaseError DriverQuoteError where
  toMessage FoundActiveQuotes = Just "Failed to offer quote, there are other active quotes from this driver"
  toMessage DriverOnRide = Just "Unable to offer a quote while being on ride"
  toMessage (DriverQuoteNotFound dqId) = Just $ "Driver quote not found with id:-" <> show dqId
  toMessage DriverQuoteExpired = Just "Driver quote expired"
  toMessage NoSearchRequestForDriver = Just "No search request for this driver"
  toMessage RideRequestAlreadyAccepted = Just "Ride request already accepted by other driver"
  toMessage (RideRequestAlreadyAcceptedOrCancelled srfd) = Just $ "Ride request already accepted by other driver or cancelled:-" <> show srfd
  toMessage (DriverAlreadyQuoted stId) = Just $ "Driver already quoted for stId:-" <> show stId
  toMessage QuoteAlreadyRejected = Just "Quote Already Rejected"
  toMessage UnexpectedResponseValue = Just "The response type is unexpected"
  toMessage NoActiveRidePresent = Just "No active ride is present for this driver registered with given vehicle Number"
  toMessage RecentActiveRide = Just "Cannot End Ride before 60 seconds"
  toMessage (DriverTransactionTryAgain driverId) = Just $ "Ongoing Transaction" <> maybe "" (<> "For DriverId") driverId <> ". Please try again later."
  toMessage CustomerDestinationUpdated = Just "Customer destination updated"

instance IsHTTPError DriverQuoteError where
  toErrorCode = \case
    FoundActiveQuotes -> "FOUND_ACTIVE_QUOTES"
    DriverOnRide -> "DRIVER_ON_RIDE"
    DriverQuoteNotFound _ -> "DRIVER_QUOTE_NOT_FOUND"
    DriverQuoteExpired -> "QUOTE_EXPIRED"
    NoSearchRequestForDriver -> "NO_SEARCH_REQUEST_FOR_DRIVER"
    RideRequestAlreadyAccepted -> "RIDE_REQUEST_ALREADY_ACCEPTED"
    RideRequestAlreadyAcceptedOrCancelled _ -> "RIDE_REQUEST_ALREADY_ACCEPTED_OR_CANCELLED"
    DriverAlreadyQuoted _ -> "DRIVER_ALREADY_QUOTED"
    QuoteAlreadyRejected -> "QUOTE_ALREADY_REJECTED"
    UnexpectedResponseValue -> "UNEXPECTED_RESPONSE_VALUE"
    NoActiveRidePresent -> "NO_ACTIVE_RIDE_PRESENT"
    RecentActiveRide -> "RECENT_ACTIVE_RIDE"
    DriverTransactionTryAgain _ -> "DRIVER_TRANSACTION_TRY_AGAIN"
    CustomerDestinationUpdated -> "CUSTOMER_DESTINATION_UPDATED"

  toHttpCode = \case
    FoundActiveQuotes -> E400
    DriverOnRide -> E400
    DriverQuoteNotFound _ -> E400
    DriverQuoteExpired -> E400
    NoSearchRequestForDriver -> E400
    RideRequestAlreadyAccepted -> E400
    RideRequestAlreadyAcceptedOrCancelled _ -> E409
    DriverAlreadyQuoted _ -> E409
    QuoteAlreadyRejected -> E400
    UnexpectedResponseValue -> E400
    NoActiveRidePresent -> E400
    RecentActiveRide -> E400
    DriverTransactionTryAgain _ -> E409
    CustomerDestinationUpdated -> E400

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

data DocumentVerificationConfigError
  = DocumentVerificationConfigNotFound Text Text
  | DocumentVerificationConfigDoesNotExist Text Text
  | DocumentVerificationConfigAlreadyExists Text Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DocumentVerificationConfigError

instance IsBaseError DocumentVerificationConfigError where
  toMessage (DocumentVerificationConfigNotFound merchantOperatingCityId doctype) =
    Just $
      "DocumentVerificationConfig with merchantOperatingCityId \"" <> show merchantOperatingCityId <> "\" and docType \"" <> show doctype <> "\" not found."
  toMessage (DocumentVerificationConfigDoesNotExist merchantOperatingCityId doctype) =
    Just $
      "DocumentVerificationConfig with merchantOperatingCityId \"" <> show merchantOperatingCityId <> "\" and docType \"" <> show doctype <> "\" does not exist."
  toMessage (DocumentVerificationConfigAlreadyExists merchantOperatingCityId doctype) =
    Just $
      "DocumentVerificationConfig with merchantOperatingCityId \"" <> show merchantOperatingCityId <> "\" and docType \"" <> show doctype <> "\" already exists."

instance IsHTTPError DocumentVerificationConfigError where
  toErrorCode = \case
    DocumentVerificationConfigNotFound {} -> "ONBOARDING_DOCUMENT_CONFIG_NOT_FOUND"
    DocumentVerificationConfigDoesNotExist {} -> "ONBOARDING_DOCUMENT_CONFIG_DOES_NOT_EXIST"
    DocumentVerificationConfigAlreadyExists {} -> "ONBOARDING_DOCUMENT_CONFIG_ALREADY_EXISTS"
  toHttpCode = \case
    DocumentVerificationConfigNotFound {} -> E500
    DocumentVerificationConfigDoesNotExist {} -> E400
    DocumentVerificationConfigAlreadyExists {} -> E400

instance IsAPIError DocumentVerificationConfigError

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
  toMessage (DriverIntelligentPoolConfigNotFound merchantOperatingCityId) =
    Just $
      "DriverIntelligentPoolConfig with merchantOperatingCityId \"" <> show merchantOperatingCityId <> "\" not found."

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
  toMessage (DriverPoolConfigDoesNotExist merchantOperatingCityId tripDistance) =
    Just $
      "DriverPoolConfig with merchantOperatingCityId \"" <> show merchantOperatingCityId <> "\" and tripDistance " <> show tripDistance <> " does not exist."
  toMessage (DriverPoolConfigAlreadyExists merchantOperatingCityId tripDistance) =
    Just $
      "DriverPoolConfig with merchantOperatingCityId \"" <> show merchantOperatingCityId <> "\" and tripDistance " <> show tripDistance <> " already exists."

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
  | CannotEnableGoHomeForDifferentCity
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
    CannotEnableGoHomeForDifferentCity -> Just "Cannot Enable Go To For a Location outside currentCity."

instance IsHTTPError DriverGoHomeRequestError where
  toErrorCode = \case
    DriverGoHomeRequestErrorNotFound _ -> "DRIVER_GO_HOME_REQUEST_NOT_FOUND"
    DriverGoHomeRequestErrorDoesNotExist _ -> "DRIVER_GO_HOME_REQUEST_DOES_NOT_EXIST"
    DriverGoHomeRequestDailyUsageLimitReached -> "DRIVER_GO_HOME_REQUEST_DAILY_USAGE_LIMIT_REACHED"
    DriverGoHomeRequestAlreadyActive -> "DRIVER_GO_HOME_REQUEST_ALREADY_ACTIVE"
    DriverGoHomeRequestNotPresent -> "DRIVER_GO_HOME_REQUEST_NOT_PRESENT"
    GoHomeFeaturePermanentlyDisabled -> "GO_HOME_FEATURE_PERMANENTLY_DISABLED"
    DriverCloseToHomeLocation -> "DRIVER_CLOSE_TO_HOME_LOCATION"
    CannotEnableGoHomeForDifferentCity -> "CANNOT_ENABLE_GO_HOME_FOR_DIFFERENT_CITY"
  toHttpCode = \case
    DriverGoHomeRequestErrorNotFound _ -> E500
    DriverGoHomeRequestErrorDoesNotExist _ -> E400
    DriverGoHomeRequestDailyUsageLimitReached -> E400
    DriverGoHomeRequestAlreadyActive -> E400
    DriverGoHomeRequestNotPresent -> E400
    GoHomeFeaturePermanentlyDisabled -> E400
    DriverCloseToHomeLocation -> E400
    CannotEnableGoHomeForDifferentCity -> E400

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
  | NoSubscriptionConfigForService Text Text
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
    NoSubscriptionConfigForService merchantOperatingCityId serviceName -> Just $ "No subscription config exists for merchantOperatingCityId \"" <> show merchantOperatingCityId <> "\" and serviceName \"" <> show serviceName <> "\""

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
    NoSubscriptionConfigForService _ _ -> "NO_SUBSCRIPTION_CONFIG_FOR_SERVICE"
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
    NoSubscriptionConfigForService _ _ -> E500

instance IsAPIError SubscriptionError

data FleetErrors
  = FleetOwnerVehicleMismatchError Text
  | VehicleBelongsToAnotherFleet
  | VehicleNotPartOfFleet
  | DriverNotPartOfFleet
  | DriverNotActiveWithFleet
  | UserAlreadyExists Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FleetErrors

instance IsBaseError FleetErrors where
  toMessage = \case
    FleetOwnerVehicleMismatchError fleetOwnerId -> Just $ "Vehicle does not belong to fleet owner with id " <> show fleetOwnerId
    VehicleBelongsToAnotherFleet -> Just "Vehicle is already part of another fleet"
    VehicleNotPartOfFleet -> Just "Vehicle is not part of any fleet"
    DriverNotPartOfFleet -> Just "Driver is not part of the fleet"
    DriverNotActiveWithFleet -> Just "Driver is not active with the fleet"
    UserAlreadyExists userId -> Just $ "User with id " <> show userId <> " already exists"

instance IsHTTPError FleetErrors where
  toErrorCode = \case
    FleetOwnerVehicleMismatchError _ -> "FLEET_OWNER_AND_VEHICLE_MISMATCH"
    VehicleBelongsToAnotherFleet -> "FLEET_OWNER_AND_VEHICLE_MISMATCH"
    VehicleNotPartOfFleet -> "VEHICLE_NOT_PART_OF_FLEET"
    DriverNotPartOfFleet -> "DRIVER_NOT_PART_OF_FLEET"
    DriverNotActiveWithFleet -> "DRIVER_NOT_ACTIVE_WITH_FLEET"
    UserAlreadyExists _ -> "USER_ALREADY_EXISTS"
  toHttpCode = \case
    FleetOwnerVehicleMismatchError _ -> E400
    VehicleBelongsToAnotherFleet -> E400
    VehicleNotPartOfFleet -> E400
    DriverNotPartOfFleet -> E400
    DriverNotActiveWithFleet -> E400
    UserAlreadyExists _ -> E400

instance IsAPIError FleetErrors

data OverlayError
  = OverlayKeyAndUdfNotFound Text
  | OverlayKeyNotFound Text
  | OverlayKeyAndUdfAlreadyPresent Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''OverlayError

instance IsBaseError OverlayError where
  toMessage = \case
    OverlayKeyAndUdfNotFound overlayKeyAndUdf -> Just $ "Overlay Key and Udf \"" <> show overlayKeyAndUdf <> "\" not found. "
    OverlayKeyNotFound overlayKey -> Just $ "Overlay Key\"" <> overlayKey <> "\" not found. "
    OverlayKeyAndUdfAlreadyPresent overlayKeyAndUdf -> Just $ "Overlay Key and Udf \"" <> show overlayKeyAndUdf <> "\" already present."

instance IsHTTPError OverlayError where
  toErrorCode = \case
    OverlayKeyAndUdfNotFound _ -> "OVERLAY_KEY_AND_UDF_NOT_FOUND"
    OverlayKeyNotFound _ -> "OVERLAY_KEY_NOT_FOUND"
    OverlayKeyAndUdfAlreadyPresent _ -> "OVERLAY_KEY_AND_UDF_ALREADY_PRESENT"
  toHttpCode = \case
    OverlayKeyAndUdfNotFound _ -> E400
    OverlayKeyNotFound _ -> E400
    OverlayKeyAndUdfAlreadyPresent _ -> E400

instance IsAPIError OverlayError

data DashboardSMSError
  = VolunteerMessageSendingLimitExceeded Text
  | DriverMessageReceivingLimitExceeded Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DashboardSMSError

instance IsBaseError DashboardSMSError where
  toMessage = \case
    VolunteerMessageSendingLimitExceeded channel -> Just $ "Volunteer message Sending limit exceeded for channel : " <> channel
    DriverMessageReceivingLimitExceeded channel -> Just $ "Drivers' message receiving limit exceeded for channel : " <> channel

instance IsHTTPError DashboardSMSError where
  toErrorCode = \case
    VolunteerMessageSendingLimitExceeded _ -> "VOLUNTEER_MESSAGE_SENDING_LIMIT_EXCEEDED"
    DriverMessageReceivingLimitExceeded _ -> "DRIVER_MESSAGE_RECEIVING_LIMIT_EXCEEDED"
  toHttpCode = \case
    VolunteerMessageSendingLimitExceeded _ -> E400
    DriverMessageReceivingLimitExceeded _ -> E400

instance IsAPIError DashboardSMSError

data DriverCoinError
  = CoinServiceUnavailable Text
  | InsufficientCoins Text Int
  | CoinConversionToCash Text Int Int
  | CoinUsedForConverting Text Int Int
  | NonBulkUploadCoinFunction Text
  | CoinInfoTranslationNotFound Text Text
  | NoPlanAgaintsDriver Text
  deriving (Generic, Eq, Show, FromJSON, ToJSON, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DriverCoinError

instance IsBaseError DriverCoinError where
  toMessage = \case
    CoinServiceUnavailable merchantId -> Just ("Coin Service is not available for merchantId " <> show merchantId <> ".")
    InsufficientCoins driverId coins -> Just ("Insufficient coin balance for driverId : " <> show driverId <> ". Given coins value is : " <> show coins <> ".")
    CoinConversionToCash driverId coins val -> Just ("Atleast " <> show val <> " coins is required for conversion driverId : " <> show driverId <> ". Given coins value is : " <> show coins <> ".")
    CoinUsedForConverting driverId coins val -> Just ("Coins used for converting to cash should be multiple of " <> show val <> " for driverId : " <> show driverId <> ". Given coins value is : " <> show coins <> ".")
    CoinInfoTranslationNotFound key lang -> Just $ "Coin info translation not found for " <> key <> " in " <> lang <> " language."
    NonBulkUploadCoinFunction eventFunction -> Just ("Coin function " <> show eventFunction <> " is not bulk upload function.")
    NoPlanAgaintsDriver driverId -> Just ("No plan found against driverId " <> show driverId <> ".")

instance IsHTTPError DriverCoinError where
  toErrorCode = \case
    CoinServiceUnavailable _ -> "COIN_SERVICE_UNAVAILABLE"
    InsufficientCoins _ _ -> "INSUFFICIENT_COINS"
    CoinConversionToCash _ _ val -> "ATLEAST_" <> show val <> "_COINS_REQUIRED_FOR_CONVERSION"
    CoinUsedForConverting _ _ val -> "COINS_USED_FOR_CONVERTING_TO_CASH_SHOULD_BE_MULTIPLE_OF_" <> show val
    NonBulkUploadCoinFunction _ -> "NON_BULK_UPLOAD_COIN_FUNCTION"
    CoinInfoTranslationNotFound _ _ -> "COIN_INFO_TRANSLATION_NOT_FOUND"
    NoPlanAgaintsDriver _ -> "NO_PLAN_AGAINST_DRIVER"
  toHttpCode = \case
    CoinServiceUnavailable _ -> E400
    InsufficientCoins _ _ -> E400
    CoinConversionToCash _ _ _ -> E400
    CoinUsedForConverting _ _ _ -> E400
    NonBulkUploadCoinFunction _ -> E400
    CoinInfoTranslationNotFound _ _ -> E400
    NoPlanAgaintsDriver _ -> E400

instance IsAPIError DriverCoinError

data DriverReferralError
  = InvalidReferralCode Text
  | DriverNotFoundForReferralCode Text
  deriving (Generic, Eq, Show, FromJSON, ToJSON, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DriverReferralError

instance IsBaseError DriverReferralError where
  toMessage = \case
    InvalidReferralCode referralCode -> Just ("Invalid referral code " <> show referralCode <> ".")
    DriverNotFoundForReferralCode referralCode -> Just ("Driver not found for referral code " <> show referralCode <> ".")

instance IsHTTPError DriverReferralError where
  toErrorCode = \case
    InvalidReferralCode _ -> "INVALID_REFERRAL_CODE"
    DriverNotFoundForReferralCode _ -> "DRIVER_NOT_FOUND_FOR_REFERRAL_CODE"
  toHttpCode = \case
    InvalidReferralCode _ -> E400
    DriverNotFoundForReferralCode _ -> E400

instance IsAPIError DriverReferralError

data RiderDetailsError
  = RiderDetailsNotFound Text
  | RiderDetailsDoNotExist Text Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RiderDetailsError

instance IsBaseError RiderDetailsError where
  toMessage = \case
    (RiderDetailsNotFound riderDetailId) -> Just $ "RideDetails with rideDetailsId \"" <> show riderDetailId <> "\" not found. "
    (RiderDetailsDoNotExist entity entityData) -> Just $ "RiderDetails not found for " <> entity <> " " <> entityData

instance IsHTTPError RiderDetailsError where
  toErrorCode _ = "RIDER_DETAILS_NOT_FOUND"
  toHttpCode _ = E500

instance IsAPIError RiderDetailsError

data CustomerCancellationDuesError
  = DisputeChancesLimitNotMet Text Text Text
  | CityRestrictionOnCustomerCancellationDuesAddition Text
  | DisputeChancesOrCancellationDuesHasToBeNull
  | CustomerCancellationDuesLimitNotMet Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''CustomerCancellationDuesError

instance IsBaseError CustomerCancellationDuesError where
  toMessage = \case
    (DisputeChancesLimitNotMet riderDetailsId disputeChancesUsed disputeChanceThreshold) -> Just $ "Limits not met for dispute chances for riderDetailsId " <> riderDetailsId <> ". Dispute Chances Used are :" <> disputeChancesUsed <> "and DisputeChanceThreshold is: " <> disputeChanceThreshold
    (CityRestrictionOnCustomerCancellationDuesAddition city) -> Just $ city <> " is restricted from addition of customer cancellation dues on ride cancellation"
    DisputeChancesOrCancellationDuesHasToBeNull -> Just "Either of the two , Due Amount or Dispute Chances has to be Null"
    (CustomerCancellationDuesLimitNotMet riderDetailsId) -> Just $ "Limits not met for cancellation dues for riderDetailsId :" <> riderDetailsId

instance IsHTTPError CustomerCancellationDuesError where
  toErrorCode = \case
    DisputeChancesLimitNotMet {} -> "DISPUTE_CHANCES_LIMIT_NOT_MET"
    CityRestrictionOnCustomerCancellationDuesAddition _ -> "CITY_RESTRICTION_ON_CUSTOMER_CANCELLATION_DUES_ADDITION"
    DisputeChancesOrCancellationDuesHasToBeNull -> "DISPUTE_CHANCES_OR_CANCELLATION_DUES_HAS_TO_BE_NULL"
    CustomerCancellationDuesLimitNotMet _ -> "CUSTOMER_CANCELLATION_DUES_LIMIT_NOT_MET"

  toHttpCode _ = E400

instance IsAPIError CustomerCancellationDuesError

data RentalError
  = OdometerReadingRequired Text
  | EndRideOtpRequired Text
  | InvalidEndOdometerReading
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RentalError

instance IsBaseError RentalError where
  toMessage = \case
    OdometerReadingRequired tripCategory -> Just $ "Odometer Readings are required for " <> tripCategory <> " ride."
    EndRideOtpRequired tripCategory -> Just $ "End Ride OTP is required to end a " <> tripCategory <> " ride."
    InvalidEndOdometerReading -> Just "End odometer reading cannot be larger than start odometer reading."

instance IsHTTPError RentalError where
  toErrorCode = \case
    OdometerReadingRequired _ -> "ODOMETER_READING_REQUIRED"
    EndRideOtpRequired _ -> "END_RIDE_OTP_REQUIRED"
    InvalidEndOdometerReading -> "INVALID_END_ODOMETER_READING"

  toHttpCode _ = E400

instance IsAPIError RentalError

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

data LmsError
  = LmsModuleTranslationNotFound Text Language
  | LmsVideoNotFound Text Text
  | LmsVideoTranslationNotFound Text Language
  | LmsQuestionTranslationNotFound Text Language
  | LmsModuleNotFound Text
  | LmsDriverModuleCompletionEntryNotFound Text Text
  | LmsQuestionNotFound Text Language
  | LmsQuestionNotFoundForModule Text Text
  | LmsCorrectOptionNotFound Text Language
  | NotAbleToDecodeTheOptionsInLms
  | NotEnoughQuestionsForModuleCompletionCriteria Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''LmsError

instance IsBaseError LmsError where
  toMessage = \case
    LmsModuleTranslationNotFound moduleId language -> Just $ "Module Translation Not found for moduleId :" <> moduleId <> "- and Language:" <> show language
    LmsVideoNotFound videoId moduleId -> Just $ "LMS video not found with videoId " <> videoId <> "- and moduleId:" <> moduleId
    LmsVideoTranslationNotFound videoId language -> Just $ "Video Translation Not found for videoId :" <> videoId <> "- and Language:" <> show language
    LmsQuestionTranslationNotFound questionId language -> Just $ "Question Translation Not found for questionId :" <> questionId <> "- and Language:" <> show language
    LmsModuleNotFound moduleId -> Just $ "Module not found with id : " <> moduleId
    LmsDriverModuleCompletionEntryNotFound moduleId personId -> Just $ "Driver module completion entry not found with module id : " <> moduleId <> " - and driver Id : " <> personId
    LmsQuestionNotFound questionId language -> Just $ "Lms Question not found with id : " <> questionId <> " with language : " <> show language
    LmsQuestionNotFoundForModule questionId moduleId -> Just $ "Lms Question not found with id : " <> questionId <> "for module with id : " <> moduleId
    LmsCorrectOptionNotFound questionId language -> Just $ "Correct Option not found for question : " <> questionId <> " and language :" <> show language
    NotAbleToDecodeTheOptionsInLms -> Just $ "Not able to deocde the options in lms question information table"
    NotEnoughQuestionsForModuleCompletionCriteria moduleId -> Just $ "Not enough question for module completion criteria for module: " <> moduleId

instance IsHTTPError LmsError where
  toErrorCode = \case
    LmsModuleTranslationNotFound _moduleId _language -> "LMS_MODULE_TRANSLATION_NOT_FOUND"
    LmsVideoNotFound _ _ -> "LMS_VIDEO_NOT_FOUND"
    LmsVideoTranslationNotFound _videoId _language -> "LMS_VIDEO_TRANSLATION_NOT_FOUND"
    LmsQuestionTranslationNotFound _questionId _language -> "LMS_QUESTION_TRANSLATION_NOT_FOUND"
    LmsModuleNotFound _moduleId -> "LMS_MODULE_NOT_FOUND"
    LmsDriverModuleCompletionEntryNotFound _ _ -> "LMS_DRIVER_MODULE_COMPLETION_ENTRY_NOT_FOUND"
    LmsQuestionNotFound _questionId _ -> "LMS_QUESTION_NOT_FOUND"
    LmsQuestionNotFoundForModule _ _ -> "LMS_QUESTION_NOT_FOUND_FOR_MODULE"
    LmsCorrectOptionNotFound _questionid _ -> "LMS_CORRECT_OPTION_NOT_FOUND"
    NotAbleToDecodeTheOptionsInLms -> "NOT_ABLE_TO_DECODE_THE_OPTIONS_FIELD_IN_QUESTION_INFORMATION"
    NotEnoughQuestionsForModuleCompletionCriteria _ -> "NOT_ENOUGH_QUESTIONS_FOR_MOUDLE_COMPLETION_CRITERIA"

  toHttpCode _ = E400

instance IsAPIError LmsError

------------ OAuth Errors ------------

data OAuthError = FailedToVerifyIdToken Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''OAuthError

instance IsBaseError OAuthError where
  toMessage = \case
    FailedToVerifyIdToken provider -> Just $ "Failed to verify identity token with " <> show provider

instance IsHTTPError OAuthError where
  toErrorCode = \case
    FailedToVerifyIdToken provider -> "OAUTH_ERROR_" <> provider
  toHttpCode = \case
    FailedToVerifyIdToken _ -> E401

instance IsAPIError OAuthError

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

data DriverOnboardingError
  = ImageValidationExceedLimit Text
  | ImageValidationFailed
  | ImageNotReadable
  | ImageLowQuality
  | ImageInvalidType Text Text
  | ImageDocumentNumberMismatch Text Text
  | ImageExtractionFailed
  | ImageNotFound Text
  | ImageNotValid Text
  | DriverAlreadyLinked
  | DLAlreadyLinked
  | DLAlreadyUpdated
  | RCAlreadyLinked
  | RCAlreadyUpdated
  | RCLimitReached Int
  | RCNotFound Text
  | RCNotLinked
  | RCMandatory Text
  | ActiveRCNotFound
  | RCVehicleOnRide
  | RCActiveOnOtherAccount
  | VehicleIsNotRegistered
  | InvalidOperatingCity Text
  | GenerateAadhaarOtpExceedLimit Text
  | RCActivationFailedPaymentDue Text
  | RCDependentDocExpired Text
  | DLInvalid
  | VehicleServiceTierNotFound Text
  | DocumentUnderManualReview Text
  | DocumentAlreadyValidated Text
  | InvalidWebhookPayload Text Text
  | InvalidReviewStatus Text Text
  | InvalidImageType Text Text
  | ImageNotFoundForWorkflowId Text
  | PanAlreadyLinked
  | GstAlreadyLinked
  | RCNotLinkedWithFleet
  | RCAssociationNotFound
  | DriverSSNNotFound Text
  | DriverDLNotFound Text
  | DriverBankAccountNotFound Text
  | DriverChargesDisabled Text
  | DocumentAlreadyLinkedToAnotherDriver Text
  | UnsyncedImageNotFound
  | DocumentAlreadyInSync
  | NotValidatedUisngFrontendSDK
  | InvalidDocumentType Text
  | DriverOnboardingVehicleCategoryNotFound
  | HyperVergeWebhookPayloadRecordNotFound Text
  | DuplicateWebhookReceived
  | WebhookAuthFailed
  deriving (Show, Eq, Read, Ord, Generic, FromJSON, ToJSON, ToSchema, IsBecknAPIError)

instance IsBaseError DriverOnboardingError where
  toMessage = \case
    ImageValidationExceedLimit id_ -> Just $ "Number of validation try exceed for person \"" <> id_ <> "\"."
    ImageValidationFailed -> Just "Validation of Image failed."
    ImageNotReadable -> Just "Image is not readable."
    ImageLowQuality -> Just "Image quality is not good"
    ImageInvalidType provided actual -> Just $ "Provided image type \"" <> provided <> "\" doesn't match actual type \"" <> actual <> "\"."
    ImageDocumentNumberMismatch a b -> Just $ "Document number \"" <> a <> "\" in image is not matching with input \"" <> b <> "\"."
    ImageExtractionFailed -> Just "Image extraction failed"
    ImageNotFound id_ -> Just $ "Image with imageId \"" <> id_ <> "\" not found."
    ImageNotValid id_ -> Just $ "Image with imageId \"" <> id_ <> "\" is not valid."
    DriverAlreadyLinked -> Just "Other doc is already linked with driver."
    DLAlreadyLinked -> Just "Driver license not available."
    DLAlreadyUpdated -> Just "No action required. Driver license is already linked to driver."
    RCAlreadyLinked -> Just "Vehicle RC not available."
    RCAlreadyUpdated -> Just "No action required. Vehicle RC is already linked to driver."
    InvalidOperatingCity city -> Just $ "Operating city \"" <> city <> "\" is invalid."
    GenerateAadhaarOtpExceedLimit id_ -> Just $ "Generate Aadhaar otp  try limit exceeded for person \"" <> id_ <> "\"."
    RCLimitReached limit -> Just $ "Linked RC limit exceed. Can't link more than " <> show limit <> " RCs."
    RCNotFound rcNo -> Just $ "Vehicle Registration Certificate with registration number " <> rcNo <> " not found."
    RCNotLinked -> Just $ "Vehicle Registration Certificate is not linked with driver."
    RCMandatory docType -> Just $ "Vehicle Registration Certificate number is mandatory for document \"" <> docType <> "\"."
    ActiveRCNotFound -> Just "Vehicle Registration Certificate is not active with any driver."
    VehicleIsNotRegistered -> Just " Vehicle is not Registered "
    RCVehicleOnRide -> Just "Vehicle on ride. Please try again later."
    RCActiveOnOtherAccount -> Just "RC active on another driver account."
    RCActivationFailedPaymentDue id_ -> Just $ "cannot activate RC for person \"" <> id_ <> "\" Due to paymentDue."
    RCDependentDocExpired detail -> Just $ "RC dependent doc not valid : " <> detail
    DLInvalid -> Just "Contact Customer Support, class of vehicles is not supported"
    VehicleServiceTierNotFound serviceTier -> Just $ "Service tier config not found for vehicle service tier \"" <> serviceTier <> "\"."
    DocumentUnderManualReview docName -> Just $ "Your " <> docName <> " is under manual review."
    DocumentAlreadyValidated docName -> Just $ "Your " <> docName <> " is already validated."
    InvalidWebhookPayload svcName errMsg -> Just $ "Unable to parse webhook payload from  " <> svcName <> ". Error: " <> errMsg
    InvalidReviewStatus svcName status -> Just $ "Invalid review status from svc : " <> svcName <> ". Value : " <> status
    InvalidImageType svcName imageType -> Just $ "Invalid image type from svc : " <> svcName <> ". Value : " <> imageType
    ImageNotFoundForWorkflowId workflowId -> Just $ "Image not found for workflowId : " <> workflowId
    PanAlreadyLinked -> Just "PAN already linked with driver."
    GstAlreadyLinked -> Just "GST already linked with driver."
    RCNotLinkedWithFleet -> Just "Vehicle Registration Certificate is not linked with Fleet."
    RCAssociationNotFound -> Just "RC association not found."
    DriverSSNNotFound id_ -> Just $ "Driver SSN not found for driverId \"" <> id_ <> "\"."
    DriverDLNotFound id_ -> Just $ "Driver DL not found for driverId \"" <> id_ <> "\"."
    DriverBankAccountNotFound id_ -> Just $ "Driver Bank Account not found for driverId \"" <> id_ <> "\"."
    DriverChargesDisabled id_ -> Just $ "Bank account is not verified for driverId \"" <> id_ <> "\"."
    DocumentAlreadyLinkedToAnotherDriver docName -> Just $ "Document Already linked to another driver " <> docName
    UnsyncedImageNotFound -> Just "Unsynced image not found"
    DocumentAlreadyInSync -> Just "Document already in sync"
    NotValidatedUisngFrontendSDK -> Just "Document not validated using frontend SDK"
    InvalidDocumentType docType -> Just $ "Document type send in the query is invalid or not supported!!!! query = " <> docType
    DriverOnboardingVehicleCategoryNotFound -> Just $ "Driver Onboarding Vehicle Catgeory Not Found"
    HyperVergeWebhookPayloadRecordNotFound reqId -> Just $ "Request id in Hyperverge webhook does not match any request id in HypervergeVerification table. RequestId : " <> reqId
    DuplicateWebhookReceived -> Just "Multiple webhooks received for same request id."
    WebhookAuthFailed -> Just "Auth header data mismatch ocurred!!!!!!"

instance IsHTTPError DriverOnboardingError where
  toErrorCode = \case
    ImageValidationExceedLimit _ -> "IMAGE_VALIDATION_EXCEED_LIMIT"
    ImageValidationFailed -> "IMAGE_VALIDATION_FAILED"
    ImageNotReadable -> "IMAGE_NOT_READABLE"
    ImageLowQuality -> "IMAGE_LOW_QUALITY"
    ImageInvalidType _ _ -> "IMAGE_INVALID_TYPE"
    ImageDocumentNumberMismatch _ _ -> "IMAGE_DOCUMENT_NUMBER_MISMATCH"
    ImageExtractionFailed -> "IMAGE_EXTRACTION_FAILED"
    ImageNotFound _ -> "IMAGE_NOT_FOUND"
    ImageNotValid _ -> "IMAGE_NOT_VALID"
    DriverAlreadyLinked -> "DRIVER_ALREADY_LINKED"
    DLAlreadyLinked -> "DL_ALREADY_LINKED"
    DLAlreadyUpdated -> "DL_ALREADY_UPDATED"
    RCAlreadyLinked -> "RC_ALREADY_LINKED"
    RCAlreadyUpdated -> "RC_ALREADY_UPDATED"
    InvalidOperatingCity _ -> "OPERATING_CITY_INVALID"
    GenerateAadhaarOtpExceedLimit _ -> "GENERATE_AADHAAR_OTP_EXCEED_LIMIT"
    RCLimitReached _ -> "MAXIMUM_RC_LIMIT_REACHED"
    RCNotFound _ -> "RC_NOT_FOUND"
    RCNotLinked -> "RC_NOT_LINKED"
    RCMandatory _ -> "RC_MANDATORY"
    ActiveRCNotFound -> "ACTIVE_RC_NOT_FOUND"
    VehicleIsNotRegistered -> "VEHICLE_IS_NOT_REGISTERED"
    RCVehicleOnRide -> "RC_Vehicle_ON_RIDE"
    RCActiveOnOtherAccount -> "RC_ACTIVE_ON_OTHER_ACCOUNT"
    RCActivationFailedPaymentDue _ -> "RC_ACTIVATION_FAILED_PAYMENT_DUE"
    RCDependentDocExpired _ -> "RC_DEPENDENT_DOC_EXPIRED"
    DLInvalid -> "DL_INVALID"
    VehicleServiceTierNotFound _ -> "VEHICLE_SERVICE_TIER_NOT_FOUND"
    DocumentUnderManualReview _ -> "DOCUMENT_UNDER_MANUAL_REVIEW"
    DocumentAlreadyValidated _ -> "DOCUMENT_ALREADY_VALIDATED"
    InvalidWebhookPayload _ _ -> "INVALID_WEBHOOK_PAYLOAD"
    InvalidReviewStatus _ _ -> "INVALID_REVIEW_STATUS"
    InvalidImageType _ _ -> "INVALID_IMAGE_TYPE"
    ImageNotFoundForWorkflowId _ -> "IMAGE_NOT_FOUND_FOR_WORKFLOW_ID"
    PanAlreadyLinked -> "PAN_ALREADY_LINKED"
    GstAlreadyLinked -> "GST_ALREADY_LINKED"
    RCNotLinkedWithFleet -> "RC_NOT_LINKED_WITH_FLEET"
    RCAssociationNotFound -> "RC_ASSOCIATION_NOT_FOUND"
    DriverSSNNotFound _ -> "DRIVER_SSN_NOT_FOUND"
    DriverDLNotFound _ -> "DRIVER_DL_NOT_FOUND"
    DriverBankAccountNotFound _ -> "DRIVER_BANK_ACCOUNT_NOT_FOUND"
    DriverChargesDisabled _ -> "DRIVER_CHARGES_DISABLED"
    DocumentAlreadyLinkedToAnotherDriver _ -> "DOCUMENT_ALREADY_LINKED_TO_ANOTHER_DRIVER"
    UnsyncedImageNotFound -> "UNSYNCED_IMAGE_NOT_FOUND"
    DocumentAlreadyInSync -> "DOCUMENT_ALREADY_IN_SYNC"
    NotValidatedUisngFrontendSDK -> "DOCUMENT_NOT_VALIDATED_USING_FRONTEND_SDK"
    InvalidDocumentType _ -> "INAVLID_DOCUMENT_TYPE"
    DriverOnboardingVehicleCategoryNotFound -> "DRIVER_ONBOARDING_VEHICLE_CATEGORY_NOT_FOUND"
    HyperVergeWebhookPayloadRecordNotFound _ -> "HYPERVERGE_WEBHOOK_PAYLOAD_RECORD_NOT_FOUND"
    DuplicateWebhookReceived -> "DUPLICATE_WEBHOOK_RECEIVED"
    WebhookAuthFailed -> "WEBHOOK_AUTH_FAILED"
  toHttpCode = \case
    ImageValidationExceedLimit _ -> E429
    ImageValidationFailed -> E400
    ImageNotReadable -> E400
    ImageLowQuality -> E400
    ImageInvalidType _ _ -> E400
    ImageDocumentNumberMismatch _ _ -> E400
    ImageExtractionFailed -> E400
    ImageNotFound _ -> E400
    ImageNotValid _ -> E400
    DriverAlreadyLinked -> E400
    DLAlreadyLinked -> E400
    DLAlreadyUpdated -> E400
    RCAlreadyLinked -> E400
    RCAlreadyUpdated -> E400
    InvalidOperatingCity _ -> E400
    GenerateAadhaarOtpExceedLimit _ -> E429
    RCLimitReached _ -> E400
    RCNotFound _ -> E400
    RCNotLinked -> E400
    RCMandatory _ -> E400
    ActiveRCNotFound -> E400
    VehicleIsNotRegistered -> E400
    RCVehicleOnRide -> E400
    RCActiveOnOtherAccount -> E400
    RCActivationFailedPaymentDue _ -> E400
    RCDependentDocExpired _ -> E400
    DLInvalid -> E400
    VehicleServiceTierNotFound _ -> E500
    DocumentUnderManualReview _ -> E400
    DocumentAlreadyValidated _ -> E400
    InvalidWebhookPayload _ _ -> E400
    InvalidReviewStatus _ _ -> E400
    InvalidImageType _ _ -> E400
    ImageNotFoundForWorkflowId _ -> E400
    PanAlreadyLinked -> E400
    GstAlreadyLinked -> E400
    RCNotLinkedWithFleet -> E400
    RCAssociationNotFound -> E400
    DriverSSNNotFound _ -> E400
    DriverDLNotFound _ -> E400
    DriverBankAccountNotFound _ -> E400
    DriverChargesDisabled _ -> E400
    DocumentAlreadyLinkedToAnotherDriver _ -> E400
    UnsyncedImageNotFound -> E400
    DocumentAlreadyInSync -> E400
    NotValidatedUisngFrontendSDK -> E400
    InvalidDocumentType _ -> E400
    DriverOnboardingVehicleCategoryNotFound -> E500
    HyperVergeWebhookPayloadRecordNotFound _ -> E400
    DuplicateWebhookReceived -> E400
    WebhookAuthFailed -> E401

instance IsAPIError DriverOnboardingError

instanceExceptionWithParent 'HTTPException ''DriverOnboardingError

$(mkBeamInstancesForEnum ''DriverOnboardingError)

data TokenizationResponseError
  = ServiceConfigError Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''TokenizationResponseError

instance IsBaseError TokenizationResponseError where
  toMessage = \case
    ServiceConfigError msg -> Just $ "Service Config Error. Error msg : " <> msg

instance IsHTTPError TokenizationResponseError where
  toErrorCode = \case
    ServiceConfigError _ -> "SERVICE_CONFIG_ERROR"
  toHttpCode = \case
    ServiceConfigError _ -> E500

instance IsAPIError TokenizationResponseError

-------- forward batching errors ------------
data ForwardBatchingErrors = CurrentRideInprogress Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''ForwardBatchingErrors

instance IsBaseError ForwardBatchingErrors where
  toMessage = \case
    CurrentRideInprogress driverId -> Just $ "Can't start advanced ride till current inprogress " <> driverId

instance IsHTTPError ForwardBatchingErrors where
  toErrorCode = \case
    CurrentRideInprogress _ -> "CURRENT_RIDE_INPROGRESS"
  toHttpCode = \case
    CurrentRideInprogress _ -> E500

instance IsAPIError ForwardBatchingErrors

data DeliveryErrors = DeliveryImageNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DeliveryErrors

instance IsBaseError DeliveryErrors where
  toMessage = \case
    DeliveryImageNotFound -> Just "Delivery image not found."

instance IsHTTPError DeliveryErrors where
  toErrorCode = \case
    DeliveryImageNotFound -> "DELIVERY_IMAGE_NOT_FOUND"
  toHttpCode = \case
    DeliveryImageNotFound -> E400

instance IsAPIError DeliveryErrors

data SpecialZoneErrors = DriverLocationOutOfRestictionBounds
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SpecialZoneErrors

instance IsBaseError SpecialZoneErrors where
  toMessage = \case
    DriverLocationOutOfRestictionBounds -> Just "Driver location out of restriction bounds."

instance IsHTTPError SpecialZoneErrors where
  toErrorCode = \case
    DriverLocationOutOfRestictionBounds -> "DRIVER_LOCATION_OUT_OF_RESTRICTION_BOUNDS"
  toHttpCode = \case
    DriverLocationOutOfRestictionBounds -> E400

instance IsAPIError SpecialZoneErrors

data LlmPromptError
  = LlmPromptNotFound Text Text Text Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''LlmPromptError

instance IsBaseError LlmPromptError where
  toMessage (LlmPromptNotFound merchantOperatingCityId service useCase promptKey) = Just $ "LLMPrompt with merchantOperatingCityId \"" <> merchantOperatingCityId <> "\" service \"" <> service <> "\" useCase \"" <> useCase <> "\" promptKey \"" <> promptKey <> "\" not found."

instance IsHTTPError LlmPromptError where
  toErrorCode = \case
    LlmPromptNotFound {} -> "LLM_PROMPT_NOT_FOUND"

  toHttpCode = \case
    LlmPromptNotFound {} -> E500

instance IsAPIError LlmPromptError

-------- FRFS Trip Errors ------------
data FRFSTripErrors = TripInvalidStatus Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FRFSTripErrors

instance IsBaseError FRFSTripErrors where
  toMessage = \case
    TripInvalidStatus msg -> Just $ "Attempted to do some action in wrong trip status. " <> msg

instance IsHTTPError FRFSTripErrors where
  toErrorCode = \case
    TripInvalidStatus _ -> "TRIP_INVALID_STATUS"
  toHttpCode = \case
    TripInvalidStatus _ -> E400

instance IsAPIError FRFSTripErrors

---------- WMB ERRORS --------------------

data RouteNotFoundError
  = RouteNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RouteNotFoundError

instance IsBaseError RouteNotFoundError where
  toMessage = \case
    RouteNotFound code -> Just $ "Requested Route does not exist : " <> code

instance IsHTTPError RouteNotFoundError where
  toErrorCode = \case
    RouteNotFound _ -> "ROUTE_NOT_FOUND"
  toHttpCode = \case
    RouteNotFound _ -> E400

instance IsAPIError RouteNotFoundError

data FleetOwnerNotFoundError
  = FleetOwnerNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FleetOwnerNotFoundError

instance IsBaseError FleetOwnerNotFoundError where
  toMessage = \case
    FleetOwnerNotFound id -> Just $ "Requested Fleet Owner does not exist : " <> id

instance IsHTTPError FleetOwnerNotFoundError where
  toErrorCode = \case
    FleetOwnerNotFound _ -> "FLEET_OWNER_NOT_FOUND"
  toHttpCode = \case
    FleetOwnerNotFound _ -> E400

instance IsAPIError FleetOwnerNotFoundError

newtype OperatorError
  = OperatorNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''OperatorError

instance IsBaseError OperatorError where
  toMessage = \case
    OperatorNotFound id -> Just $ "Requested Operator not found: " <> id

instance IsHTTPError OperatorError where
  toErrorCode = \case
    OperatorNotFound _ -> "OPERATOR_NOT_FOUND"
  toHttpCode = \case
    OperatorNotFound _ -> E500

instance IsAPIError OperatorError

data WMBErrors
  = DriverNotInFleet Text Text
  | DriverNotActiveInFleet Text Text
  | VehicleLinkedToInvalidDriver
  | DriverRequestNotFound Text
  | RequestAlreadyProcessed Text
  | FleetOwnerIdRequired
  | MaxVehiclesLimitExceeded Int
  | VehicleLinkedToAnotherDriver Text
  | DriverNotLinkedToFleet Text
  | MobileNumberAlreadyLinked Text
  | EmailAlreadyLinked Text
  | DeviceTokenNotFound
  | OtpNotFound
  | InvalidOtp
  | RcNotValid
  | RoundTripNotAllowedForRoute Text
  | RoundTripFrequencyShouldBeGreaterThanZero
  | MaxDriversLimitExceeded Int
  | TripTransactionNotFound Text
  | EndRideRequestAlreadyPresent
  | RideNotEligibleForEnding
  | AlertRequestIdNotFound Text
  | NoActiveFleetAssociated Text
  | FleetConfigNotFound Text
  | InactiveFleetDriverAssociationNotFound Text
  | InactiveOperatorDriverAssociationNotFound Text -- TODO separate error type
  | VehicleRouteMappingNotFound Text Text
  | InvalidTripStatus Text
  | DriverNotFoundWithId
  | StopNotFound
  | RouteTripStopMappingNotFound Text
  | VehicleRouteMappingBlocked
  | AlreadyOnActiveTripWithAnotherVehicle Text
  | AlreadyOnActiveTrip
  | FleetBadgeNotFound Text
  | FleetBadgeAlreadyLinked Text
  | AlreadyOnActiveTripWithAnotherBadge Text
  | VehicleBelongsToFleet
  | InvalidRequesterRole Text
  | InvalidRoleForVehicleAdd Text
  | DriverHasActiveLink Text
  | FleetNotActiveInOperator Text Text
  | VehicleAlreadyLinkedToAnotherDriver
  | InvalidFleetOwner Text
  | DriverAlreadyLinkedToAnotherVehicle
  | VehicleNotVerified Text
  | FleetOwnerOrOperatorIdRequired
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''WMBErrors

instance IsBaseError WMBErrors where
  toMessage = \case
    AlreadyOnActiveTripWithAnotherVehicle vehicleNumber -> Just $ "Driver is already on an Active Trip with Another Vehicle : " <> vehicleNumber
    DriverNotInFleet driverId fleetId -> Just $ "Driver :" <> driverId <> "is not part of the fleet " <> fleetId
    DriverNotActiveInFleet id fleetId -> Just $ "Driver : " <> id <> " is not active with the fleet: " <> fleetId
    VehicleLinkedToInvalidDriver -> Just "Vehicle is associated with a driver who is not part of the fleet: "
    DriverRequestNotFound id -> Just $ "DriverRequest not found" <> id
    RequestAlreadyProcessed id -> Just $ "Request already processed" <> id
    FleetOwnerIdRequired -> Just "Fleet Owner Id is required"
    MaxVehiclesLimitExceeded limit -> Just $ "Maximum " <> show limit <> " vehicles can be added in one go"
    VehicleLinkedToAnotherDriver vehicleNo -> Just $ "Vehicle" <> vehicleNo <> "is linked to some other driver"
    DriverNotLinkedToFleet id -> Just $ "Driver is not linked to the fleet, driver id :" <> id
    MobileNumberAlreadyLinked mobile -> Just $ "Mobile number is already linked with another fleet owner: " <> mobile
    EmailAlreadyLinked email -> Just $ "Email is already linked with another fleet owner: " <> email
    DeviceTokenNotFound -> Just "Device Token not found"
    OtpNotFound -> Just "OTP not found"
    InvalidOtp -> Just "Invalid OTP"
    RcNotValid -> Just "Cannot link to driver because Rc is not valid"
    RoundTripNotAllowedForRoute routeCode -> Just $ "Round trip not allowed for this route: " <> routeCode
    RoundTripFrequencyShouldBeGreaterThanZero -> Just "Round Trip Frequency should be greater than 0"
    MaxDriversLimitExceeded limit -> Just $ "Maximum " <> show limit <> " drivers can be added in one go"
    TripTransactionNotFound id -> Just $ "Trip transaction not found for :" <> id
    EndRideRequestAlreadyPresent -> Just "EndRide request already present"
    RideNotEligibleForEnding -> Just "Ride Not Eligible For Ending"
    AlertRequestIdNotFound id -> Just $ "Alert Request Id not found" <> id
    NoActiveFleetAssociated id -> Just $ "No Active Fleet Associated for driver :" <> id
    FleetConfigNotFound id -> Just $ "Fleet Config Info not found for owner id : " <> id
    InactiveFleetDriverAssociationNotFound id -> Just $ "Inactive Fleet Driver Association Not Found for driver : " <> id
    InactiveOperatorDriverAssociationNotFound id -> Just $ "Inactive Operator Driver Association Not Found for driver : " <> id
    VehicleRouteMappingNotFound vhclNo routeCode -> Just $ "Vehicle Route Mapping not found for vehicle no hash : " <> vhclNo <> " and route code :" <> routeCode
    InvalidTripStatus id -> Just $ "Invalid trip status, current status : " <> id
    DriverNotFoundWithId -> Just "Driver Not Found"
    StopNotFound -> Just "Stop Not Found"
    RouteTripStopMappingNotFound routeCode -> Just $ "Route trip stop mapping not found for route code : " <> routeCode
    VehicleRouteMappingBlocked -> Just "Vehicle Route Mapping is blocked, unblock and try again."
    AlreadyOnActiveTrip -> Just "Driver is already on an Active trip."
    FleetBadgeNotFound badgeName -> Just $ "Fleet Badge Name Not Found : " <> badgeName
    FleetBadgeAlreadyLinked driverId -> Just $ "Fleet Badge already linked to driver id : " <> driverId
    AlreadyOnActiveTripWithAnotherBadge driverName -> Just $ "Driver is already on an Active trip with another badge : " <> driverName
    VehicleBelongsToFleet -> Just "Vehicle already belongs to a fleet."
    InvalidRequesterRole role -> Just $ "Requester has invalid role: " <> show role
    InvalidRoleForVehicleAdd role -> Just $ "Invalid role for adding vehicle: " <> show role
    DriverHasActiveLink driverId ->
      Just $ "Driver: " <> driverId <> " already has active association."
    FleetNotActiveInOperator fleetOwnerId operatorId ->
      Just $ "FleetOwner: " <> fleetOwnerId <> " is not active under operator: " <> operatorId
    VehicleAlreadyLinkedToAnotherDriver -> Just "Vehicle is already linked to another driver"
    InvalidFleetOwner fleetOwnerId -> Just $ "FleetOwner: " <> fleetOwnerId <> " is not valid."
    DriverAlreadyLinkedToAnotherVehicle -> Just "Driver is already linked to another vehicle"
    VehicleNotVerified vehicleNo -> Just $ "Vehicle: " <> vehicleNo <> " is not verified"
    FleetOwnerOrOperatorIdRequired -> Just "Fleet Owner or Operator Id is required"

instance IsHTTPError WMBErrors where
  toErrorCode = \case
    AlreadyOnActiveTripWithAnotherVehicle _ -> "ALREADY_ON_ACTIVE_TRIP_WITH_ANOTHER_VEHICLE"
    DriverNotInFleet _ _ -> "DRIVER_NOT_IN_FLEET"
    DriverNotActiveInFleet _ _ -> "DRIVER_NOT_ACTIVE_IN_FLEET"
    VehicleLinkedToInvalidDriver -> "VEHICLE_LINKED_TO_INVALID_DRIVER"
    DriverRequestNotFound _ -> "DRIVER_REQUEST_NOT_FOUND"
    RequestAlreadyProcessed _ -> "REQUEST_ALREADY_PROCESSED"
    FleetOwnerIdRequired -> "FLEET_OWNER_ID_REQUIRED"
    MaxVehiclesLimitExceeded _ -> "MAX_VEHICLES_LIMIT_EXCEEDED"
    VehicleLinkedToAnotherDriver _ -> "VEHICLE_LINKED_TO_ANOTHER_DRIVER"
    DriverNotLinkedToFleet _ -> "DRIVER_NOT_LINKED_TO_FLEET"
    MobileNumberAlreadyLinked _ -> "MOBILE_NUMBER_ALREADY_LINKED"
    EmailAlreadyLinked _ -> "EMAIL_ALREADY_LINKED"
    DeviceTokenNotFound -> "DEVICE_TOKEN_NOT_FOUND"
    OtpNotFound -> "OTP_NOT_FOUND"
    InvalidOtp -> "INVALID_OTP"
    RcNotValid -> "RC_NOT_VALID"
    RoundTripNotAllowedForRoute _ -> "ROUND_TRIP_NOT_ALLOWED_FOR_ROUTE"
    RoundTripFrequencyShouldBeGreaterThanZero -> "ROUND_TRIP_FREQUENCY_SHOULD_BE_GREATER_THAN_ZERO"
    MaxDriversLimitExceeded _ -> "MAX_DRIVERS_LIMIT_EXCEEDED"
    TripTransactionNotFound _ -> "INVALID_TRIP_TRANSACTION_ID"
    EndRideRequestAlreadyPresent -> "END_RIDE_REQUEST_ALREADY_PRESENT"
    RideNotEligibleForEnding -> "RIDE_NOT_ELIGIBLE_FOR_ENDING"
    AlertRequestIdNotFound _ -> "ALERT_REQUEST_ID_NOT_FOUND"
    NoActiveFleetAssociated _ -> "NO_ACTIVE_FLEET_ASSOCIATED"
    FleetConfigNotFound _ -> "FLEET_CONFIG_NOT_FOUND"
    InactiveFleetDriverAssociationNotFound _ -> "INACTIVE_FLEET_DRIVER_ASSOCIATION_NOT_FOUND"
    InactiveOperatorDriverAssociationNotFound _ -> "INACTIVE_OPERATOR_DRIVER_ASSOCIATION_NOT_FOUND"
    VehicleRouteMappingNotFound _ _ -> "VEHICLE_ROUTE_MAPPING_NOT_FOUND"
    InvalidTripStatus _ -> "INVALID_TRIP_STATUS"
    DriverNotFoundWithId -> "DRIVER_NOT_FOUND"
    StopNotFound -> "STOP_NOT_FOUND"
    RouteTripStopMappingNotFound _ -> "ROUTE_TRIP_STOP_MAPPING_NOT_FOUND"
    VehicleRouteMappingBlocked -> "VEHICLE_ROUTE_MAPPING_BLOCKED"
    AlreadyOnActiveTrip -> "ALREADY_ON_ACTIVE_TRIP"
    FleetBadgeNotFound _ -> "FLEET_BADGE_NOT_FOUND"
    FleetBadgeAlreadyLinked _ -> "FLEET_BADGE_ALREADY_LINKED"
    AlreadyOnActiveTripWithAnotherBadge _ -> "ALREADY_ON_ACTIVE_TRIP_WITH_ANOTHER_BADGE"
    VehicleBelongsToFleet -> "VEHICLE_BELONGS_TO_FLEET"
    InvalidRequesterRole _ -> "INVALID_REQUESTER_ROLE"
    InvalidRoleForVehicleAdd _ -> "INVALID_ROLE_FOR_VEHICLE_ADD"
    DriverHasActiveLink _ -> "DRIVER_HAS_ACTIVE_LINK"
    FleetNotActiveInOperator _ _ -> "FLEET_NOT_ACTIVE_IN_OPERATOR"
    VehicleAlreadyLinkedToAnotherDriver -> "VEHICLE_ALREADY_LINKED_TO_ANOTHER_DRIVER"
    InvalidFleetOwner _ -> "INVALID_FLEET_OWNER"
    DriverAlreadyLinkedToAnotherVehicle -> "DRIVER_ALREADY_LINKED_TO_ANOTHER_VEHICLE"
    VehicleNotVerified _ -> "VEHICLE_NOT_VERIFIED"
    FleetOwnerOrOperatorIdRequired -> "FLEET_OWNER_OR_OPERATOR_ID_REQUIRED"
  toHttpCode = \case
    AlreadyOnActiveTripWithAnotherVehicle _ -> E400
    DriverNotInFleet _ _ -> E400
    DriverNotActiveInFleet _ _ -> E400
    VehicleLinkedToInvalidDriver -> E400
    DriverRequestNotFound _ -> E404
    RequestAlreadyProcessed _ -> E400
    FleetOwnerIdRequired -> E400
    MaxVehiclesLimitExceeded _ -> E400
    VehicleLinkedToAnotherDriver _ -> E400
    DriverNotLinkedToFleet _ -> E400
    MobileNumberAlreadyLinked _ -> E400
    EmailAlreadyLinked _ -> E400
    DeviceTokenNotFound -> E404
    OtpNotFound -> E400
    InvalidOtp -> E400
    RcNotValid -> E400
    RoundTripNotAllowedForRoute _ -> E400
    RoundTripFrequencyShouldBeGreaterThanZero -> E400
    MaxDriversLimitExceeded _ -> E400
    TripTransactionNotFound _ -> E400
    EndRideRequestAlreadyPresent -> E400
    RideNotEligibleForEnding -> E400
    AlertRequestIdNotFound _ -> E404
    NoActiveFleetAssociated _ -> E400
    FleetConfigNotFound _ -> E404
    InactiveFleetDriverAssociationNotFound _ -> E404
    InactiveOperatorDriverAssociationNotFound _ -> E404
    VehicleRouteMappingNotFound _ _ -> E404
    InvalidTripStatus _ -> E400
    DriverNotFoundWithId -> E400
    StopNotFound -> E400
    RouteTripStopMappingNotFound _ -> E400
    VehicleRouteMappingBlocked -> E400
    AlreadyOnActiveTrip -> E400
    FleetBadgeNotFound _ -> E400
    FleetBadgeAlreadyLinked _ -> E400
    AlreadyOnActiveTripWithAnotherBadge _ -> E400
    VehicleBelongsToFleet -> E400
    InvalidRequesterRole _ -> E400
    InvalidRoleForVehicleAdd _ -> E400
    DriverHasActiveLink _ -> E400
    FleetNotActiveInOperator _ _ -> E400
    VehicleAlreadyLinkedToAnotherDriver -> E400
    InvalidFleetOwner _ -> E400
    DriverAlreadyLinkedToAnotherVehicle -> E400
    VehicleNotVerified _ -> E400
    FleetOwnerOrOperatorIdRequired -> E400

instance IsAPIError WMBErrors

newtype OperationHubError
  = OperationHubDoesNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''OperationHubError

instance IsBaseError OperationHubError where
  toMessage = \case
    OperationHubDoesNotExist operationHubId -> Just $ "No operation hub matches passed data \"" <> show operationHubId <> "\" not exist."

instance IsHTTPError OperationHubError where
  toErrorCode = \case
    OperationHubDoesNotExist _ -> "OPERATION_HUB_DOES_NOT_EXIST"
  toHttpCode = \case
    OperationHubDoesNotExist _ -> E400

instance IsAPIError OperationHubError

data TripAlertRequestError
  = TripAlertRequestNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''TripAlertRequestError

instance IsBaseError TripAlertRequestError where
  toMessage = \case
    TripAlertRequestNotFound tripAlertRequestId -> Just $ "Trip alert request with id \"" <> show tripAlertRequestId <> "\" not found."

instance IsHTTPError TripAlertRequestError where
  toErrorCode = \case
    TripAlertRequestNotFound _ -> "TRIP_ALERT_REQUEST_NOT_FOUND"
  toHttpCode = \case
    TripAlertRequestNotFound _ -> E404

instance IsAPIError TripAlertRequestError
