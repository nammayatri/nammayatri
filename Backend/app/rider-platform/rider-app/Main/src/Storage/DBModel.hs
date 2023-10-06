{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.DBModel where

import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data RiderApp

instance IsDBModel RiderApp where
  type DBModelType RiderApp = DBModel

-- Each Beam table should be added to DBModel type, used for TH in drainer app
data DBModel
  = AadhaarOtpReq
  | AadhaarOtpVerify
  | AadhaarVerification
  | AppInstalls
  | BecknRequest
  | BlackListOrg
  | Booking
  | BookingCancellationReason
  | BookingLocation
  | CallStatus
  | CallbackRequest
  | CancellationReason
  | DirectionsCache
  | Disability
  | DisabilityTranslation
  | DriverOffer
  | Estimate
  | EstimateBreakup
  | Exophone
  | FareBreakup
  | FeedbackForm
  | Geometry
  | HotSpotConfig
  | Issue
  | Location
  | LocationMapping
  | Merchant
  | MerchantConfig
  | MerchantMessage
  | MerchantPaymentMethod
  | MerchantServiceConfig
  | MerchantServiceUsageConfig
  | OnSearchEvent
  | PaymentOrder
  | PaymentTransaction
  | Person
  | PersonDefaultEmergencyNumber
  | PersonDisability
  | PersonFlowStatus
  | PersonStats
  | PlaceNameCache
  | Quote
  | Rating
  | RegistrationToken
  | RentalSlab
  | Ride
  | SavedReqLocation
  | SearchReqLocation
  | SearchRequest
  | Sos
  | SpecialZoneQuote
  | TripTerms
  | Webengage
  deriving stock (Show, Read, Enum, Bounded, Generic)
  deriving anyclass (FromJSON)
