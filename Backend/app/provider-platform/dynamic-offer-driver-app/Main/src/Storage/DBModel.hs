{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.DBModel where

import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data DriverApp

instance IsDBModel DriverApp where
  type DBModelType DriverApp = DBModel

-- Each Beam table should be added to DBModel type, used for TH in drainer app
data DBModel
  = AadhaarOtpReq
  | AadhaarOtpVerify
  | AadhaarVerification
  | BapMetadata
  | BecknRequest
  | Booking
  | BookingCancellationReason
  | BookingLocation
  | BusinessEvent
  | CallStatus
  | CancellationReason
  | Comment
  | DriverBlockReason
  | DriverExtraFeeBounds
  | DriverFee
  | DriverFlowStatus
  | DriverGoHomeRequest
  | DriverHomeLocation
  | DriverInformation
  | DriverIntelligentPoolConfig
  | DriverLicense
  | DriverLocation
  | DriverPlan
  | DriverPoolConfig
  | DriverQuote
  | DriverRCAssociation
  | DriverReferral
  | DriverStats
  | Estimate
  | Exophone
  | FareParameters
  | FareParametersProgressiveDetails
  | FareParametersSlabDetails
  | FarePolicy
  | FarePolicyProgressiveDetails
  | FarePolicyProgressiveDetailsPerExtraKmRateSection
  | FarePolicySlabsDetailsSlab
  | FareProduct
  | Feedback
  | FeedbackBadge
  | FeedbackForm
  | Geometry
  | GoHomeConfig
  | IdfyVerification
  | Image
  | Invoice
  | IssueCategory
  | IssueOption
  | IssueReport
  | IssueTranslation
  | KioskLocation
  | KioskLocationTranslation
  | LeaderBoardConfigs
  | Location
  | LocationMapping
  | Mandate
  | MediaFile
  | Merchant
  | MerchantMessage
  | MerchantPaymentMethod
  | MerchantServiceConfig
  | MerchantServiceUsageConfig
  | Message
  | MessageReport
  | MessageTranslation
  | MetaData
  | Notification
  | OnboardingDocumentConfig
  | OperatingCity
  | Overlay
  | Person
  | PlaceNameCache
  | Plan
  | PlanTranslation
  | QuoteSpecialZone
  | Rating
  | RegistrationToken
  | RegistryMapFallback
  | RestrictedExtraFare
  | Ride
  | RideDetails
  | RiderDetails
  | SearchReqLocation
  | SearchRequest
  | SearchRequestForDriver
  | SearchRequestSpecialZone
  | SearchTry
  | TransporterConfig
  | Vehicle
  | VehicleRegistrationCertificate
  | Volunteer
  deriving stock (Show, Read, Enum, Bounded, Generic)
  deriving anyclass (FromJSON)
